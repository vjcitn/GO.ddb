#' Lazy tbl of GO term synonyms
#'
#' Retrieves all synonym types for GO terms from the semsql
#' \code{statements} table.  Four synonym scopes are recognised by the
#' OBO format and all are present in the GO semsql build:
#' \code{hasExactSynonym}, \code{hasRelatedSynonym},
#' \code{hasNarrowSynonym}, and \code{hasBroadSynonym}.
#'
#' @param ids optional character vector of GO CURIEs to restrict results.
#'   If \code{NULL} (default), synonyms for all non-deprecated GO terms
#'   are returned.
#' @param types character vector of synonym scopes to include.  Default
#'   includes all four.  Elements must be one or more of
#'   \code{"exact"}, \code{"related"}, \code{"narrow"}, \code{"broad"}.
#'
#' @return a lazy \code{tbl_duckdb} with columns:
#'   \describe{
#'     \item{id}{GO CURIE}
#'     \item{synonym}{synonym string}
#'     \item{scope}{one of \code{"exact"}, \code{"related"},
#'       \code{"narrow"}, \code{"broad"}}
#'   }
#'   Call \code{dplyr::collect()} to materialize.
#'
#' @examples
#' GO.ddb::make_go_con()
#'
#' # All synonyms for a term
#' GO.ddb::go_synonyms("GO:0006954") |> dplyr::collect()
#'
#' # Exact synonyms only across all terms
#' GO.ddb::go_synonyms(types = "exact") |>
#'   dplyr::collect()
#'
#' # Synonyms for multiple terms
#' GO.ddb::go_synonyms(c("GO:0006954", "GO:0008150")) |>
#'   dplyr::collect()
#'
#' GO.ddb::disconnect_go()
#'
#' @seealso \code{\link{go_terms}}, \code{\link{select_go}}
#'
#' @export
go_synonyms <- function(
    ids   = NULL,
    types = c("exact", "related", "narrow", "broad")) {

  types <- match.arg(types, several.ok = TRUE)

  scope_map <- c(
    exact   = "oio:hasExactSynonym",
    related = "oio:hasRelatedSynonym",
    narrow  = "oio:hasNarrowSynonym",
    broad   = "oio:hasBroadSynonym"
  )

  predicates <- unname(scope_map[types])

  st <- go_statements() |>
    dplyr::filter(
      subject   %like% "GO:%",
      predicate %in% predicates
    ) |>
    dplyr::mutate(
      scope = dplyr::case_when(
        predicate == "oio:hasExactSynonym"   ~ "exact",
        predicate == "oio:hasRelatedSynonym" ~ "related",
        predicate == "oio:hasNarrowSynonym"  ~ "narrow",
        predicate == "oio:hasBroadSynonym"   ~ "broad"
      )
    ) |>
    dplyr::select(id = subject, synonym = value, scope)

  if (!is.null(ids)) {
    if (!is.character(ids) || length(ids) == 0L)
      stop("`ids` must be a non-empty character vector or NULL.", call. = FALSE)
    st <- st |> dplyr::filter(id %in% ids)
  }

  st
}

