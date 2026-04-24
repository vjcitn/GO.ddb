# graph_traversal.R — ancestor and descendant queries via entailed_edge

#' Retrieve ancestors of GO terms
#'
#' Uses the precomputed transitive closure in the semsql
#' \code{entailed_edge} table to find all ancestors of the supplied
#' GO CURIEs under the specified relations.  Returns a long-format lazy
#' tibble suitable for direct use with \code{dplyr}, \code{ggraph}, or
#' gene set enrichment tooling.
#'
#' @param ids       character vector of GO CURIEs.
#' @param relations character vector of predicate CURIEs to traverse.
#'   Defaults to \code{is_a} and \code{part_of} from
#'   \code{\link{GO_RELATIONS}}.  The full relation set is large (~230
#'   predicates including Uberon and BSPO imports) — always specify an
#'   explicit subset.
#' @param include_self logical.  If \code{FALSE} (default), reflexive
#'   self-edges (\code{subject == object}) present in the transitive
#'   closure are excluded.
#'
#' @return a lazy \code{tbl_duckdb} with columns:
#'   \describe{
#'     \item{id}{the query term CURIE}
#'     \item{ancestor_id}{ancestor term CURIE}
#'     \item{relation}{predicate CURIE}
#'   }
#'   Call \code{dplyr::collect()} to materialize.
#'
#' @examples
#' make_go_con()
#'
#' go_ancestors("GO:0006954") |> dplyr::collect()
#'
#' # is_a only
#' go_ancestors("GO:0006954",
#'   relations = GO_RELATIONS["is_a"]) |> dplyr::collect()
#'
#' # multiple query terms
#' go_ancestors(c("GO:0006954", "GO:0008150"),
#'   relations = unname(GO_RELATIONS[c("is_a", "part_of")])) |>
#'   dplyr::count(id)
#'
#' disconnect_go()
#'
#' @seealso \code{\link{go_descendants}}, \code{\link{GO_RELATIONS}}
#'
#' @export
go_ancestors <- function(
    ids,
    relations    = unname(GO_RELATIONS[c("is_a", "part_of")]),
    include_self = FALSE) {

  if (!is.character(ids) || length(ids) == 0L)
    stop("`ids` must be a non-empty character vector.", call. = FALSE)
  if (!is.character(relations) || length(relations) == 0L)
    stop("`relations` must be a non-empty character vector.", call. = FALSE)

  out <- go_entailed_edges() |>
    dplyr::filter(
      subject   %in% ids,
      predicate %in% relations
    ) |>
    dplyr::select(id = subject, ancestor_id = object, relation = predicate)

  if (!include_self)
    out <- out |> dplyr::filter(id != ancestor_id)

  out
}


#' Retrieve descendants of GO terms
#'
#' Uses the precomputed transitive closure in the semsql
#' \code{entailed_edge} table to find all descendants of the supplied
#' GO CURIEs under the specified relations.  Returns a long-format lazy
#' tibble.
#'
#' @param ids       character vector of GO CURIEs.
#' @param relations character vector of predicate CURIEs to traverse.
#'   Defaults to \code{is_a} and \code{part_of} from
#'   \code{\link{GO_RELATIONS}}.
#' @param include_self logical.  If \code{FALSE} (default), reflexive
#'   self-edges are excluded.
#'
#' @return a lazy \code{tbl_duckdb} with columns:
#'   \describe{
#'     \item{id}{the query term CURIE}
#'     \item{descendant_id}{descendant term CURIE}
#'     \item{relation}{predicate CURIE}
#'   }
#'   Call \code{dplyr::collect()} to materialize.
#'
#' @examples
#' make_go_con()
#'
#' go_descendants("GO:0006950") |> dplyr::collect()
#'
#' # count descendants per ontology namespace
#' go_descendants("GO:0008150") |>
#'   dplyr::left_join(
#'     go_terms() |> dplyr::select(descendant_id = id, ontology),
#'     by = "descendant_id"
#'   ) |>
#'   dplyr::count(ontology) |>
#'   dplyr::collect()
#'
#' disconnect_go()
#'
#' @seealso \code{\link{go_ancestors}}, \code{\link{GO_RELATIONS}}
#'
#' @export
go_descendants <- function(
    ids,
    relations    = unname(GO_RELATIONS[c("is_a", "part_of")]),
    include_self = FALSE) {

  if (!is.character(ids) || length(ids) == 0L)
    stop("`ids` must be a non-empty character vector.", call. = FALSE)
  if (!is.character(relations) || length(relations) == 0L)
    stop("`relations` must be a non-empty character vector.", call. = FALSE)

  out <- go_entailed_edges() |>
    dplyr::filter(
      object    %in% ids,
      predicate %in% relations
    ) |>
    dplyr::select(id = object, descendant_id = subject, relation = predicate)

  if (!include_self)
    out <- out |> dplyr::filter(id != descendant_id)

  out
}
