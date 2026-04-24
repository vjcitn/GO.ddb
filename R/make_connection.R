#' Establish a connection to the GO semsql database
#'
#' Retrieves GO data either from a local parquet cache or from the semsql
#' SQLite file managed by \code{ontoProc2::semsql_connect()}, then loads
#' it into an in-process DuckDB instance.
#'
#' @param ontology character scalar. Default \code{"go"}.
#' @param backend one of:
#'   \describe{
#'     \item{\code{"auto"}}{use parquet if \code{\link{has_parquet_cache}}
#'       returns \code{TRUE}, otherwise SQLite}
#'     \item{\code{"parquet"}}{require parquet cache — error if absent}
#'     \item{\code{"sqlite"}}{use SQLite via DuckDB scanner and materialize
#'       hot tables into native DuckDB storage}
#'   }
#'
#' @return \code{NULL} invisibly.
#'
#' @examples
#' # auto selects parquet if available, SQLite otherwise
#' make_go_con()
#' go_connection_active()
#' disconnect_go()
#'
#' # force parquet (must have run build_parquet_cache() first)
#' if (has_parquet_cache()) {
#'   make_go_con(backend = "parquet")
#'   disconnect_go()
#' }
#'
#' @seealso \code{\link{build_parquet_cache}}, \code{\link{has_parquet_cache}},
#'   \code{\link{disconnect_go}}
#'
#' @export
make_go_con <- function(ontology = "go",
                        backend  = c("auto", "parquet", "sqlite")) {
  backend <- match.arg(backend)

  # If already connected, return silently — make_go_con() is safe to call
  # at the top of any script or example without checking first.
  if (!is.null(.cache) &&
      exists("con", envir = .cache, inherits = FALSE) &&
      DBI::dbIsValid(.cache$con))
    return(invisible(NULL))


  if (!is.null(.cache) &&
      exists("con", envir = .cache) &&
      DBI::dbIsValid(.cache$con)) {
    message(
      "GO connection already active (schema = 'main', ",
      "backend = '", .cache$backend, "'). ",
      "Call disconnect_go() first to establish a new connection."
    )
    return(invisible(NULL))
  }

  # Resolve backend
  if (backend == "auto")
    backend <- if (has_parquet_cache(ontology)) "parquet" else "sqlite"

  if (backend == "parquet" && !has_parquet_cache(ontology))
    stop(
      "No parquet cache found for ontology '", ontology, "'.\n",
      "Run build_parquet_cache() first.",
      call. = FALSE
    )

  con <- DBI::dbConnect(duckdb::duckdb())

  tryCatch({
    if (backend == "parquet") {
      message("Loading GO from parquet cache ...")
      t0 <- proc.time()["elapsed"]
      .attach_parquet(con, ontology)
      elapsed <- round(proc.time()["elapsed"] - t0, 1)
      message(sprintf("  -> ready in %.1fs", elapsed))

    } else {
      # SQLite path
      gcon        <- ontoProc2::semsql_connect(ontology = ontology)
      sqlite_path <- gcon@db_path

      if (!file.exists(sqlite_path))
        stop("semsql_connect() returned path that does not exist: ",
             sqlite_path, call. = FALSE)

      message("Attaching GO semsql SQLite ...")
      .attach_sqlite(con, ontology, sqlite_path)
      materialize_hot_tables(con)
    }
  }, error = function(e) {
    DBI::dbDisconnect(con, shutdown = TRUE)
    stop(conditionMessage(e), call. = FALSE)
  })

  # Validate hot tables landed in main
  present <- DBI::dbGetQuery(
    con,
    "SELECT table_name FROM duckdb_tables() WHERE schema_name = 'main'"
  )$table_name

  required <- c("statements", "entailed_edge")
  missing  <- setdiff(required, present)
  if (length(missing))
    stop(
      "Required tables missing from main schema after loading: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )

  .cache$con     <- con
  .cache$schema  <- "main"
  .cache$backend <- backend

  message("GO connection ready (backend = '", backend, "').")
  invisible(NULL)
}


#' Close the cached GO semsql connection
#'
#' Disconnects the DuckDB instance and clears the package cache.  The next
#' call to any query function will trigger reconnection automatically.
#'
#' @return \code{NULL} invisibly.
#'
#' @examples
#' GO.ddb::make_go_con()
#' GO.ddb::disconnect_go()
#' GO.ddb::go_connection_active()
#'
#' @seealso \code{\link{make_go_con}}, \code{\link{go_connection_active}}
#'
#' @export
disconnect_go <- function() {
  if (!is.null(.cache) && exists("con", envir = .cache, inherits = FALSE)) {
    if (DBI::dbIsValid(.cache$con))
      DBI::dbDisconnect(.cache$con, shutdown = TRUE)
    rm(list = ls(.cache, all.names = TRUE), envir = .cache)
    message("GO semsql connection closed.")
  } else {
    message("No active GO connection to close.")
  }
  invisible(NULL)
}
