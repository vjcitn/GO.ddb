# R/parquet.R - parquet cache management and hot table materialization

#' Directory used for locally cached parquet files
#' Returns the path where GO semsql parquet files are expected, using
#' BiocFileCache's cache directory as the root so the location is
#' consistent with how the SQLite file is managed.
#' @param ontology character scalar, e.g. \code{"go"}.
#' @return character scalar path to the parquet cache directory.
#' @keywords internal
.parquet_cache_dir <- function(ontology = "go") {
  bfc  <- BiocFileCache::BiocFileCache(ask = FALSE)
  root <- BiocFileCache::bfccache(bfc)
  file.path(root, paste0(ontology, "_parquet"))
}


#' Test whether a local parquet cache exists for an ontology
#' Checks that the parquet cache directory exists and contains at least
#' the two required files: \code{statements.parquet} and
#' \code{entailed_edge.parquet}.
#' @import BiocFileCache
#' @param ontology character scalar. Default \code{"go"}.
#' @return logical scalar.
#' @examples
#' has_parquet_cache()
#' @export
has_parquet_cache <- function(ontology = "go") {
  dir <- .parquet_cache_dir(ontology)
  if (!dir.exists(dir))
    return(FALSE)
  required <- c("statements.parquet", "entailed_edge.parquet")
  all(file.exists(file.path(dir, required)))
}


#' Populate the local parquet cache from a semsql SQLite file
#'
#' Converts the semsql SQLite tables to parquet format and writes them
#' to the BiocFileCache-managed parquet directory.  This is a one-time
#' setup step - subsequent calls to \code{make_go_con(backend = "parquet")}
#' or \code{make_go_con(backend = "auto")} will use the cached files
#' without re-converting.
#'
#' @param sqlite_path character scalar path to the semsql SQLite file.
#'   If \code{NULL} (default), retrieved via \code{ontoProc2::semsql_connect()}.
#' @param ontology character scalar. Default \code{"go"}.
#' @param tables character vector of table names to convert. Default covers
#'   the two tables the package actively queries.
#'
#' @return the parquet cache directory path, invisibly.
#'
#' @examples
#' if (!has_parquet_cache()) {
#'   build_parquet_cache()
#' }
#'
#' @export
build_parquet_cache <- function(
    sqlite_path = NULL,
    ontology    = "go",
    tables      = c("statements", "entailed_edge", "term_association")) {

  if (is.null(sqlite_path)) {
    gcon        <- ontoProc2::semsql_connect(ontology = ontology)
    sqlite_path <- gcon@db_path
  }

  if (!file.exists(sqlite_path))
    stop("SQLite file not found: ", sqlite_path, call. = FALSE)

  out_dir <- .parquet_cache_dir(ontology)
  if (!dir.exists(out_dir))
    dir.create(out_dir, recursive = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con, "INSTALL sqlite; LOAD sqlite;")
  DBI::dbExecute(con,
    paste(
      "ATTACH", DBI::dbQuoteLiteral(con, sqlite_path),
      "AS go (TYPE sqlite, READ_ONLY)"
    )
  )

  # Check which tables actually exist before attempting conversion -
  # term_association is present but empty in the standard GO semsql build
  present <- DBI::dbGetQuery(con,
    glue::glue_sql(
      "SELECT table_name FROM duckdb_tables() WHERE database_name = 'go'",
      .con = con
    )
  )$table_name

  for (tbl in tables) {
    if (!tbl %in% present) {
      message("Skipping ", tbl, " - not present in source database.")
      next
    }
    out_path <- file.path(out_dir, paste0(tbl, ".parquet"))
    message("Writing ", tbl, ".parquet ...")
    DBI::dbExecute(con, sprintf(
      "COPY (SELECT * FROM go.%s) TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)",
      tbl, out_path
    ))
    sz <- file.size(out_path) / 1024^2
    message(sprintf("  -> %.1f MB", sz))
  }

  message("Parquet cache written to: ", out_dir)
  invisible(out_dir)
}


#' Materialize hot tables from SQLite into native DuckDB storage
#'
#' When using the SQLite backend, DuckDB must translate every query through
#' its SQLite scanner bridge, which adds overhead on repeated scans of large
#' tables.  This function copies \code{statements} and \code{entailed_edge}
#' into native DuckDB columnar storage in the \code{main} schema, amortizing
#' the bridge cost to a one-time hit at connection time.
#'
#' Called automatically by \code{make_go_con(backend = "sqlite")} - not
#' normally called directly.
#'
#' @param con a \code{DBIConnection} to an open DuckDB instance with the
#'   semsql SQLite attached as schema \code{"go"}.
#' @param tables character vector of table names to materialize.
#'
#' @return \code{NULL} invisibly.
#' @keywords internal
materialize_hot_tables <- function(
    con,
    tables = c("statements", "entailed_edge")) {

  for (tbl in tables) {
    message("Materializing ", tbl, " into DuckDB native storage ...")
    t0 <- proc.time()["elapsed"]
    DBI::dbExecute(con, sprintf(
      "CREATE TABLE main.%s AS SELECT * FROM go.%s", tbl, tbl
    ))
    elapsed <- round(proc.time()["elapsed"] - t0, 1)
    nr <- DBI::dbGetQuery(
      con, sprintf("SELECT COUNT(*) AS n FROM main.%s", tbl)
    )$n
    message(sprintf("  -> %s rows in %.1fs", format(nr, big.mark=","), elapsed))
  }
  invisible(NULL)
}


# Internal backend attachment helpers

.attach_sqlite <- function(con, ontology, sqlite_path) {
  sch_id  <- DBI::dbQuoteIdentifier(con, ontology)
  gpath_l <- DBI::dbQuoteLiteral(con, sqlite_path)
  DBI::dbExecute(con, "INSTALL sqlite; LOAD sqlite;")
  DBI::dbExecute(con,
    paste("ATTACH", gpath_l, "AS", sch_id, "(TYPE sqlite, READ_ONLY)")
  )
}

.attach_parquet <- function(con, ontology) {
  dir      <- .parquet_cache_dir(ontology)
  required <- c("statements", "entailed_edge")

  for (tbl in required) {
    path <- file.path(dir, paste0(tbl, ".parquet"))
    DBI::dbExecute(con, sprintf(
      "CREATE TABLE main.%s AS SELECT * FROM read_parquet('%s')", tbl, path
    ))
  }

  # term_association is optional
  ta_path <- file.path(dir, "term_association.parquet")
  if (file.exists(ta_path))
    DBI::dbExecute(con, sprintf(
      "CREATE TABLE main.term_association AS SELECT * FROM read_parquet('%s')",
      ta_path
    ))
}

