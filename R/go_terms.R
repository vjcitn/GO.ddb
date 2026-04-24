# go_terms.R — curated GO term metadata reconstructed from statements
#
# The semsql node and edge views are unavailable via DuckDB's SQLite scanner:
# DuckDB cannot parse the UNION syntax in owl_reified_axiom.  go_terms()
# reconstructs equivalent functionality directly from the statements base table.

#' Lazy tbl of GO term metadata
#'
#' Reconstructs term labels, definitions, ontology namespace (BP/MF/CC),
#' and deprecation status from the semsql \code{statements} table.
#' Filters to GO-prefixed identifiers, excluding imported terms from
#' Uberon, CHEBI, RO, and other ontologies present in the GO OWL source.
#'
#' Uses \code{\%like\%} rather than \code{startsWith()} for the GO prefix
#' filter — \code{startsWith()} is not translated by dbplyr to DuckDB's
#' \code{starts_with()} function and will cause a catalog error.
#'
#' @param include_deprecated logical.  If \code{FALSE} (default), obsolete
#'   GO terms (\code{owl:deprecated = "true"}) are excluded.  Set
#'   \code{TRUE} to include them; the \code{deprecated} column is always
#'   present in the output regardless.
#' @param con    optional \code{DBIConnection} for testing.
#' @param schema optional character schema name for testing.
#'
#' @return a lazy \code{tbl_duckdb} with columns:
#'   \describe{
#'     \item{id}{GO CURIE, e.g. \code{"GO:0006954"}}
#'     \item{label}{human-readable term name}
#'     \item{definition}{IAO:0000115 term definition}
#'     \item{ontology}{namespace string: \code{"biological_process"},
#'       \code{"molecular_function"}, or \code{"cellular_component"}}
#'     \item{deprecated}{logical}
#'   }
#'
#' @examples
#' make_go_con()
#'
#' # All non-deprecated terms
#' go_terms()
#'
#' # Biological process terms only
#' go_terms() |>
#'   dplyr::filter(ontology == "biological_process") |>
#'   dplyr::collect()
#'
#' # Include deprecated terms
#' go_terms(include_deprecated = TRUE) |>
#'   dplyr::filter(deprecated) |>
#'   dplyr::select(id, label) |>
#'   dplyr::collect()
#'
#' disconnect_go()
#'
#' @seealso \code{\link{lookup_curie}}, \code{\link{go_ancestors}}
#'
#' @export
go_terms <- function(include_deprecated = FALSE, con = NULL, schema = NULL) {
  st <- go_statements(con = con, schema = schema)

  labels <- st |>
    dplyr::filter(predicate == "rdfs:label") |>
    dplyr::select(id = subject, label = value)

  ns <- st |>
    dplyr::filter(predicate == "oio:hasOBONamespace") |>
    dplyr::select(id = subject, ontology = value)

  defs <- st |>
    dplyr::filter(predicate == "IAO:0000115") |>
    dplyr::select(id = subject, definition = value)

  deprecated <- st |>
    dplyr::filter(predicate == "owl:deprecated", value == "true") |>
    dplyr::select(id = subject) |>
    dplyr::mutate(deprecated = TRUE)

  out <- labels |>
    dplyr::filter(id %like% "GO:%") |>
    dplyr::left_join(ns,         by = "id") |>
    dplyr::left_join(defs,       by = "id") |>
    dplyr::left_join(deprecated, by = "id") |>
    dplyr::mutate(deprecated = dplyr::coalesce(deprecated, FALSE))

  if (!include_deprecated)
    out <- out |> dplyr::filter(!deprecated)

  out
}
