# constants.R — curated relation CURIE map
#
# Counts below are from a 2024 GO semsql build.  The entailed_edge table
# contains ~230 distinct predicates in total, including imported Uberon,
# BSPO, and SO relations.  Queries must always filter to a specific subset
# — traversing all predicates returns biologically meaningless results.
#
# NOTE: duckdb_views() is permanently off-limits for semsql-attached SQLite
# databases.  DuckDB cannot parse the UNION syntax in the owl_reified_axiom
# view and will throw an Invalid Error.  Use duckdb_tables() for all catalog
# introspection on these connections.

#' Curated GO relation CURIEs for ontology traversal
#'
#' A named character vector mapping human-readable relation names to their
#' CURIE representations as they appear in the semsql \code{entailed_edge}
#' table.  Counts are from a representative 2024 GO build:
#'
#' \describe{
#'   \item{is_a}{\code{rdfs:subClassOf} — 1,360,314 entailed edges}
#'   \item{part_of}{\code{BFO:0000050} — 353,639 edges}
#'   \item{has_part}{\code{BFO:0000051} — 1,038,218 edges (inverse of part_of)}
#'   \item{occurs_in}{\code{BFO:0000066} — 26,234 edges (biological process)}
#'   \item{located_in}{\code{RO:0001025} — 1,459 edges (cellular component)}
#' }
#'
#' Pass one or more values from this vector as the \code{relations} argument
#' to \code{\link{go_ancestors}} and \code{\link{go_descendants}}.
#'
#' @examples
#' GO_RELATIONS
#' GO_RELATIONS["is_a"]
#' unname(GO_RELATIONS[c("is_a", "part_of")])
#'
#' @export
GO_RELATIONS <- c(
  is_a       = "rdfs:subClassOf",
  part_of    = "BFO:0000050",
  has_part   = "BFO:0000051",
  occurs_in  = "BFO:0000066",
  located_in = "RO:0001025"
)
