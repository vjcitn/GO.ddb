# make_connection.R — connection establishment and teardown

#' Establish a connection to the GO semsql database
#'
#' Retrieves the GO semsql SQLite file path via
#' \code{ontoProc2::semsql_connect()} (which manages caching through
#' BiocFileCache), then attaches it to an in-process DuckDB instance.
#' The connection is stored in a private package cache and reused by all
#' subsequent calls to \code{\link{go_terms}}, \code{\link{lookup_curie}},
#' \code{\link{go_ancestors}}, etc.
#'
#' This function must be called explicitly — it is intentionally not invoked
#' in \code{.onLoad} because \code{semsql_connect()} may trigger a
#' BiocFileCache download, which must always be user-initiated.
#'
#' @param ontology character scalar passed to
#'   \code{ontoProc2::semsql_connect()}.  Default \code{"go"}.
#'
#' @return \code{NULL} invisibly.  The connection is stored in the package
#'   cache; use \code{\link{disconnect_go}} to close it.
#'
#' @seealso \code{\link{disconnect_go}}, \code{\link{go_connection_active}}
#'
#' @examples
#' make_go_con()
#' go_connection_active()
#' disconnect_go()
#'
#' @export
make_go_con <- function(ontology = "go") {
  if (!is.null(.cache) &&
      exists("con", envir = .cache) &&
      DBI::dbIsValid(.cache$con)) {
    message(
      "GO connection already active (schema = '", .cache$schema, "'). ",
      "Call disconnect_go() first to establish a new connection."
    )
    return(invisible(NULL))
  }

  gcon  <- ontoProc2::semsql_connect(ontology = ontology)
  gpath <- gcon@db_path

  if (!file.exists(gpath))
    stop(
      "semsql_connect() returned a path that does not exist: ", gpath,
      call. = FALSE
    )

  con     <- DBI::dbConnect(duckdb::duckdb())
  sch_id  <- DBI::dbQuoteIdentifier(con, ontology)
  gpath_l <- DBI::dbQuoteLiteral(con, gpath)

  tryCatch({
    DBI::dbExecute(con, "INSTALL sqlite; LOAD sqlite;")
    DBI::dbExecute(con,
      paste("ATTACH", gpath_l, "AS", sch_id, "(TYPE sqlite, READ_ONLY)")
    )
  }, error = function(e) {
    DBI::dbDisconnect(con, shutdown = TRUE)
    stop(
      "Failed to attach GO semsql database: ", conditionMessage(e),
      call. = FALSE
    )
  })

  # Validate required base tables using duckdb_tables().
  # - duckdb_views() fails on semsql SQLite: DuckDB cannot parse the UNION
  #   syntax in owl_reified_axiom.
  # - sqlite_master is not exposed by DuckDB for attached databases.
  present <- DBI::dbGetQuery(con,
    glue::glue_sql(
      "SELECT table_name FROM duckdb_tables() WHERE database_name = {ontology}",
      .con = con
    )
  )$table_name

  required <- c("statements", "entailed_edge")
  missing  <- setdiff(required, present)
  if (length(missing)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
    stop(
      "GO semsql database missing required base tables: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  .cache$con    <- con
  .cache$schema <- ontology
  message("GO semsql connection established (schema = '", ontology, "').")
  invisible(NULL)
}


#' Close the cached GO semsql connection
#'
#' Disconnects the DuckDB instance and clears the package cache.  The next
#' call to any query function will trigger reconnection automatically.
#'
#' @return \code{NULL} invisibly.
#'
#' @seealso \code{\link{make_go_con}}, \code{\link{go_connection_active}}
#'
#' @examples
#' make_go_con()
#' disconnect_go()
#' go_connection_active()
#'
#' @export
disconnect_go <- function() {
  if (!is.null(.cache) && exists("con", envir = .cache)) {
    if (DBI::dbIsValid(.cache$con))
      DBI::dbDisconnect(.cache$con, shutdown = TRUE)
    rm("con", "schema", envir = .cache)
    message("GO semsql connection closed.")
  } else {
    message("No active GO connection to close.")
  }
  invisible(NULL)
}
