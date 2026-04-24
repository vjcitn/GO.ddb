# lookup_curie.R — user-facing GO CURIE lookup

#' Look up GO term metadata by CURIE
#'
#' Maps one or more GO CURIEs to their term label, definition, or ontology
#' namespace.  Returns a lazy tibble — call \code{dplyr::collect()} to
#' materialize results.
#'
#' @param curies character vector of GO CURIEs in the form
#'   \code{"GO:nnnnnnn"}, e.g. \code{c("GO:0006954", "GO:0008150")}.
#' @param mapto  character scalar, one of:
#'   \describe{
#'     \item{\code{"term"}}{term label (human-readable name)}
#'     \item{\code{"definition"}}{IAO:0000115 full definition}
#'     \item{\code{"ontology"}}{namespace: \code{"biological_process"},
#'       \code{"molecular_function"}, or \code{"cellular_component"}}
#'     \item{\code{"all"}}{all three columns}
#'   }
#'   Partial matching is supported via \code{match.arg()}.
#'
#' @return a lazy \code{tbl_duckdb} with column \code{id} and the
#'   requested field(s).  Call \code{dplyr::collect()} to materialize.
#'
#' @examples
#' make_go_con()
#'
#' lookup_curie(c("GO:0006954", "GO:0008150"), mapto = "term") |>
#'   dplyr::collect()
#'
#' lookup_curie("GO:0006954", mapto = "all") |>
#'   dplyr::collect()
#'
#' # partial matching works
#' lookup_curie("GO:0006954", mapto = "def") |>
#'   dplyr::collect()
#'
#' disconnect_go()
#'
#' @seealso \code{\link{go_terms}}, \code{\link{GO_RELATIONS}}
#'
#' @export
lookup_curie <- function(curies,
                         mapto = c("term", "definition", "ontology", "all")) {
  mapto <- match.arg(mapto)

  if (!is.character(curies) || length(curies) == 0L)
    stop("`curies` must be a non-empty character vector.", call. = FALSE)

  bad <- curies[!grepl("^GO:\\d+$", curies)]
  if (length(bad))
    warning(
      "The following are not valid GO CURIEs and will return no results:\n  ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )

  col_map <- c(
    term       = "label",
    definition = "definition",
    ontology   = "ontology"
  )

  base <- go_terms() |>
    dplyr::filter(id %in% curies)

  if (mapto == "all")
    return(base |> dplyr::select(id, label, definition, ontology))

  base |> dplyr::select(id, !!col_map[[mapto]])
}
