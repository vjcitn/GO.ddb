#' Return the raw DuckDB connection from the package cache
#'
#' Useful for diagnostics, custom queries, or passing to dbplyr directly.
#' Returns \code{NULL} if no connection is active.
#'
#' @return a \code{DBIConnection} or \code{NULL}.
#'
#' @examples
#' make_go_con()
#' con <- get_go_con()
#' DBI::dbGetQuery(con, "SELECT database_name, schema_name, table_name
#'                        FROM duckdb_tables()")
#' disconnect_go()
#'
#' @export
get_go_con <- function() {
  if (!go_connection_active())
    return(NULL)
  .cache$con
}
