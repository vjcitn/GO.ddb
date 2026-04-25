#' Emulate AnnotationDbi::select for GO.db users
#'
#' Provides a familiar interface for users migrating from
#' \code{AnnotationDbi::select(GO.db, ...)}. Unlike the lazy tibbles
#' returned by \code{\link{go_terms}} and \code{\link{lookup_curie}},
#' this function returns an eager \code{data.frame} to match the
#' \code{AnnotationDbi} contract.
#'
#' @param keys character vector of GO CURIEs (e.g. \code{"GO:0006954"}).
#' @param columns character vector of columns to return. Valid values:
#'   \code{"GOID"}, \code{"TERM"}, \code{"DEFINITION"}, \code{"ONTOLOGY"},
#'   \code{"SYNONYM"}.
#' @param keytype character scalar. Only \code{"GOID"} is currently
#'   supported, matching the GO.db constraint.
#'
#' @return a \code{data.frame} with column \code{GOID} and the requested
#'   additional columns, in the same format as
#'   \code{AnnotationDbi::select(GO.db, ...)}.
#'
#' @examples
#' GO.ddb::make_go_con()
#'
#' # Direct replacement for AnnotationDbi::select(GO.db, ...)
#' GO.ddb::select_go(
#'   keys    = c("GO:0006954", "GO:0008150"),
#'   columns = c("TERM", "ONTOLOGY")
#' )
#'
#' # With synonyms
#' GO.ddb::select_go(
#'   keys    = "GO:0006954",
#'   columns = c("TERM", "DEFINITION", "ONTOLOGY", "SYNONYM")
#' )
#'
#' GO.ddb::disconnect_go()
#'
#' @seealso \code{\link{lookup_curie}}, \code{\link{go_terms}}
#'
#' @export
select_go <- function(keys,
                      columns = c("TERM", "DEFINITION", "ONTOLOGY"),
                      keytype = "GOID") {

  if (getOption("GO.ddb.select_compat_warn", TRUE))
    message(
      "select_go() is a compatibility bridge for GO.db users.\n",
      "Consider migrating to go_terms() and lookup_curie() for lazy evaluation."
    )

  # ── Input validation ────────────────────────────────────────────────────
  if (!is.character(keys) || length(keys) == 0L)
    stop("`keys` must be a non-empty character vector.", call. = FALSE)

  keytype <- match.arg(keytype, "GOID")

  valid_columns <- c("GOID", "TERM", "DEFINITION", "ONTOLOGY", "SYNONYM")
  bad_cols <- setdiff(columns, valid_columns)
  if (length(bad_cols))
    stop(
      "Invalid columns: ", paste(bad_cols, collapse = ", "), "\n",
      "Valid columns are: ", paste(valid_columns, collapse = ", "),
      call. = FALSE
    )

  # Warn on non-GO CURIEs as lookup_curie() does
  bad_keys <- keys[!grepl("^GO:\\d+$", keys)]
  if (length(bad_keys))
    warning(
      "The following are not valid GO CURIEs and will return no results:\n  ",
      paste(bad_keys, collapse = ", "),
      call. = FALSE
    )

  # ── Build result from go_terms() ────────────────────────────────────────
  # Synonyms require a separate join — only fetch if requested
  want_synonyms <- "SYNONYM" %in% columns
  other_cols    <- setdiff(columns, c("GOID", "SYNONYM"))

  # Column name map: AnnotationDbi → semsql
  col_map <- c(
    TERM       = "label",
    DEFINITION = "definition",
    ONTOLOGY   = "ontology"
  )

  semsql_cols <- col_map[other_cols]

  base <- go_terms() |>
    dplyr::filter(id %in% keys) |>
    dplyr::select(GOID = id, dplyr::all_of(unname(semsql_cols))) |>
    dplyr::rename_with(
      ~ names(semsql_cols)[match(., semsql_cols)],
      dplyr::all_of(unname(semsql_cols))
    )

  if (!want_synonyms) {
    result <- base |> dplyr::collect()
    # AnnotationDbi always returns GOID as first column
    return(as.data.frame(result[, c("GOID", other_cols), drop = FALSE]))
  }

  # ── Synonyms via statements ──────────────────────────────────────────────
  # GO.db collapses all synonym types into a single SYNONYM column with
  # one row per synonym (long format, like AnnotationDbi multiVals="list"
  # flattened). We match that behaviour.
  syn_predicates <- c(
    "oio:hasExactSynonym",
    "oio:hasRelatedSynonym",
    "oio:hasNarrowSynonym",
    "oio:hasBroadSynonym"
  )

  synonyms <- go_statements() |>
    dplyr::filter(
      subject   %in% keys,
      predicate %in% syn_predicates
    ) |>
    dplyr::select(GOID = subject, SYNONYM = value)

  # Left join: terms with no synonyms get NA in SYNONYM column,
  # terms with multiple synonyms get multiple rows — matching GO.db behaviour
  result <- base |>
    dplyr::left_join(synonyms, by = "GOID") |>
    dplyr::collect()

  col_order <- intersect(c("GOID", other_cols, "SYNONYM"), colnames(result))
  as.data.frame(result[, col_order, drop = FALSE])
}

