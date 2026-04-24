# utils.R - internal helpers

# Null-coalescing operator.
# Defined locally to avoid an rlang import dependency.
`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0L && !is.na(x)) x else y


#' Internal accessor for the cached DuckDB connection
#'
#' Never exported.  All low-level functions call this to retrieve the
#' connection and schema name from the package cache.  If no connection
#' is active, \code{make_go_con()} is called automatically with a
#' message so that the BiocFileCache activity is visible to the user.
#'
#' @return a list with elements \code{con} (DBIConnection) and
#'   \code{schema} (character).
#' @keywords internal
.get_con <- function() {
  if (is.null(.cache) ||
      !exists("con", envir = .cache) ||
      !DBI::dbIsValid(.cache$con)) {
    message("No active GO connection - calling make_go_con() ...")
    make_go_con()
  }
  list(con = .cache$con, schema = .cache$schema)
}


#' Report whether a live GO connection is cached
#'
#' Lightweight predicate used in examples and tests to guard against
#' running query code when no connection has been established and no
#' automatic reconnection is desired.
#'
#' @return logical scalar.
#' @export
go_connection_active <- function() {
  !is.null(.cache) &&
    exists("con", envir = .cache) &&
    DBI::dbIsValid(.cache$con)
}
