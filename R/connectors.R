# connectors.R — low-level lazy tbl accessors for semsql base tables
#
# These functions return lazy tbl_duckdb objects.  Call dplyr::collect()
# to materialize results.  They are the substrate on which all higher-level
# functions (go_terms, go_ancestors, etc.) are built.
#
# The optional con/schema arguments exist for unit testing — production
# code calls these without arguments and relies on .get_con().

#' Lazy tbl for the semsql statements table
#'
#' The \code{statements} table contains all RDF triples from the GO OWL
#' source, including term labels (\code{rdfs:label}), definitions
#' (\code{IAO:0000115}), namespaces (\code{oio:hasOBONamespace}), and
#' deprecation flags (\code{owl:deprecated}).
#'
#' @param con    optional \code{DBIConnection}.  If \code{NULL} (default),
#'   the package cache is used via \code{.get_con()}.
#' @param schema optional character schema name.  If \code{NULL} (default),
#'   the cached schema is used.
#'
#' @return a lazy \code{tbl_duckdb} with columns
#'   \code{stanza, subject, predicate, object, value, datatype, language}.
#'
#' @examples
#' make_go_con()
#' go_statements()
#' disconnect_go()
#'
#' @export
go_statements <- function(con = NULL, schema = NULL) {
  if (is.null(con)) {
    x      <- .get_con()
    con    <- x$con
    schema <- x$schema
  }
  suppressMessages(
    dplyr::tbl(con, DBI::Id(schema = schema, table = "statements"))
  )
}


#' Lazy tbl for the semsql entailed_edge table
#'
#' The \code{entailed_edge} table contains the precomputed transitive
#' closure of all object property relations in GO, including
#' \code{rdfs:subClassOf} (is_a) and \code{BFO:0000050} (part_of).
#' It is the basis for \code{\link{go_ancestors}} and
#' \code{\link{go_descendants}}.
#'
#' Self-edges (\code{subject == object}) encode reflexivity under the
#' transitive closure and are excluded by default in
#' \code{\link{go_ancestors}} and \code{\link{go_descendants}}.
#'
#' @param con    optional \code{DBIConnection}.  If \code{NULL} (default),
#'   the package cache is used via \code{.get_con()}.
#' @param schema optional character schema name.  If \code{NULL} (default),
#'   the cached schema is used.
#'
#' @return a lazy \code{tbl_duckdb} with columns
#'   \code{subject, predicate, object}.
#'
#' @examples
#' make_go_con()
#' go_entailed_edges()
#' disconnect_go()
#'
#' @export
go_entailed_edges <- function(con = NULL, schema = NULL) {
  if (is.null(con)) {
    x      <- .get_con()
    con    <- x$con
    schema <- x$schema
  }
  suppressMessages(
    dplyr::tbl(con, DBI::Id(schema = schema, table = "entailed_edge"))
  )
}
