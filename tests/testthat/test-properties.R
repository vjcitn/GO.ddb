# test-properties.R — property-based tests using hedgehog
#
# These tests verify mathematical properties of the GO graph that hold
# universally, not just for specific hardcoded terms.  Each forall() call
# runs tests (default 100, reduced here for query performance) random
# examples sampled from the live database.
#
# Run selectively during development:
#   testthat::test_file("tests/testthat/test-properties.R")
#
# Requires: hedgehog (in Suggests)

testthat::skip_if_not_installed("hedgehog")
library(hedgehog)

# ── Shared generator ──────────────────────────────────────────────────────────
#
# Samples a random GO CURIE from the live non-deprecated term set.
# Cached in the test environment to avoid repeated collect() calls.

.all_go_ids <- function() {
  GO.ddb::go_terms() |>
    dplyr::select(id) |>
    dplyr::collect() |>
    dplyr::pull(id)
}

.go_ids_by_ns <- function(ns) {
  GO.ddb::go_terms() |>
    dplyr::filter(ontology == ns) |>
    dplyr::select(id) |>
    dplyr::collect() |>
    dplyr::pull(id)
}

# ── Property 1: ancestor/descendant symmetry ──────────────────────────────────
#
# For any GO term X, if A is an ancestor of X then X must appear as a
# descendant of A.

test_that("ancestor/descendant relationship is symmetric", {
  live_con()
  ids <- .all_go_ids()

  forall(
    gen.element(ids),
    function(id) {
      ancestors <- GO.ddb::go_ancestors(id) |>
        dplyr::pull(ancestor_id)

      if (length(ancestors) == 0L)
        return(invisible(NULL))

      # Sample one ancestor to check — verifying all would be too slow
      anc <- sample(ancestors, 1L)

      descendants <- GO.ddb::go_descendants(anc) |>
        dplyr::pull(descendant_id)

      expect_true(
        id %in% descendants,
        label = sprintf("%s should be a descendant of its ancestor %s", id, anc)
      )
    },
    tests = 30L
  )
})


# ── Property 2: ancestor relation is transitive ───────────────────────────────
#
# If A is an ancestor of X and B is an ancestor of A, then B must also
# be an ancestor of X.  Tested under is_a only for clarity.

test_that("is_a ancestor relation is transitive", {
  live_con()
  ids <- .all_go_ids()

  forall(
    gen.element(ids),
    function(id) {
      ancestors_of_id <- GO.ddb::go_ancestors(
        id, relations = GO.ddb::GO_RELATIONS["is_a"]
      ) |> dplyr::pull(ancestor_id)

      if (length(ancestors_of_id) < 2L)
        return(invisible(NULL))

      # Pick a random intermediate ancestor
      anc <- sample(ancestors_of_id, 1L)

      ancestors_of_anc <- GO.ddb::go_ancestors(
        anc, relations = GO.ddb::GO_RELATIONS["is_a"]
      ) |> dplyr::pull(ancestor_id)

      # Every ancestor of anc should also be an ancestor of id
      non_transitive <- setdiff(ancestors_of_anc, ancestors_of_id)

      expect_equal(
        length(non_transitive), 0L,
        label = sprintf(
          "Transitivity violated for %s via %s: missing %s",
          id, anc, paste(non_transitive, collapse = ", ")
        )
      )
    },
    tests = 30L
  )
})


# ── Property 3: is_a ancestors stay within namespace ─────────────────────────
#
# GO's is_a relation does not cross ontology namespace boundaries.
# All is_a ancestors of a BP term should be BP terms, and likewise
# for MF and CC.

test_that("is_a ancestors of BP terms are all BP terms", {
  live_con()
  bp_ids <- .go_ids_by_ns("biological_process")

  forall(
    gen.element(bp_ids),
    function(id) {
      ancestors <- GO.ddb::go_ancestors(
        id, relations = GO.ddb::GO_RELATIONS["is_a"]
      ) |> dplyr::pull(ancestor_id)

      if (length(ancestors) == 0L)
        return(invisible(NULL))

      ancestor_ns <- GO.ddb::go_terms() |>
        dplyr::filter(id %in% ancestors) |>
        dplyr::distinct(ontology) |>
        dplyr::collect() |>
        dplyr::pull(ontology)

      expect_true(
        all(ancestor_ns == "biological_process"),
        label = sprintf(
          "BP term %s has non-BP is_a ancestors: %s",
          id, paste(setdiff(ancestor_ns, "biological_process"), collapse = ", ")
        )
      )
    },
    tests = 20L
  )
})

test_that("is_a ancestors of MF terms are all MF terms", {
  live_con()
  mf_ids <- .go_ids_by_ns("molecular_function")

  forall(
    gen.element(mf_ids),
    function(id) {
      ancestors <- GO.ddb::go_ancestors(
        id, relations = GO.ddb::GO_RELATIONS["is_a"]
      ) |> dplyr::pull(ancestor_id)

      if (length(ancestors) == 0L)
        return(invisible(NULL))

      ancestor_ns <- GO.ddb::go_terms() |>
        dplyr::filter(id %in% ancestors) |>
        dplyr::distinct(ontology) |>
        dplyr::collect() |>
        dplyr::pull(ontology)

      expect_true(
        all(ancestor_ns == "molecular_function"),
        label = sprintf(
          "MF term %s has non-MF is_a ancestors: %s",
          id, paste(setdiff(ancestor_ns, "molecular_function"), collapse = ", ")
        )
      )
    },
    tests = 20L
  )
})


# ── Property 4: no self-ancestors ────────────────────────────────────────────
#
# go_ancestors() with include_self = FALSE (the default) should never
# return the query term as one of its own ancestors.

test_that("go_ancestors() never includes the query term itself", {
  live_con()
  ids <- .all_go_ids()

  forall(
    gen.element(ids),
    function(id) {
      ancestors <- GO.ddb::go_ancestors(id) |>
        dplyr::pull(ancestor_id)

      expect_false(
        id %in% ancestors,
        label = sprintf("%s should not be its own ancestor", id)
      )
    },
    tests = 50L
  )
})


# ── Property 5: no self-descendants ──────────────────────────────────────────
#
# Symmetric property for go_descendants().

test_that("go_descendants() never includes the query term itself", {
  live_con()
  ids <- .all_go_ids()

  forall(
    gen.element(ids),
    function(id) {
      descendants <- GO.ddb::go_descendants(id) |>
        dplyr::pull(descendant_id)

      expect_false(
        id %in% descendants,
        label = sprintf("%s should not be its own descendant", id)
      )
    },
    tests = 50L
  )
})


# ── Property 6: lookup_curie consistent with go_terms ────────────────────────
#
# For any GO CURIE, the label returned by lookup_curie() should exactly
# match the label in go_terms().

test_that("lookup_curie() label is consistent with go_terms() label", {
  live_con()
  ids <- .all_go_ids()

  forall(
    gen.element(ids),
    function(id) {
      label_lookup <- GO.ddb::lookup_curie(id, mapto = "term") |>
        dplyr::collect() |>
        dplyr::pull(label)

      label_terms <- GO.ddb::go_terms() |>
        dplyr::filter(id == !!id) |>
        dplyr::collect() |>
        dplyr::pull(label)

      expect_equal(
        label_lookup, label_terms,
        label = sprintf("Label mismatch for %s", id)
      )
    },
    tests = 30L
  )
})


# ── Property 7: select_go consistent with go_terms ───────────────────────────
#
# For any GO CURIE, the TERM returned by select_go() should match
# the label in go_terms(), and ONTOLOGY should match ontology.

test_that("select_go() TERM and ONTOLOGY are consistent with go_terms()", {
  live_con()
  ids <- .all_go_ids()

  forall(
    gen.element(ids),
    function(id) {
      from_select <- GO.ddb::select_go(
        id, columns = c("TERM", "ONTOLOGY")
      )

      from_terms <- GO.ddb::go_terms() |>
        dplyr::filter(id == !!id) |>
        dplyr::collect()

      expect_equal(
        from_select$TERM,
        from_terms$label,
        label = sprintf("TERM mismatch for %s", id)
      )
      expect_equal(
        from_select$ONTOLOGY,
        from_terms$ontology,
        label = sprintf("ONTOLOGY mismatch for %s", id)
      )
    },
    tests = 30L
  )
})


# ── Property 8: go_synonyms scope values are valid ───────────────────────────
#
# For any GO term that has synonyms, all scope values should be members
# of the defined vocabulary.

test_that("go_synonyms() scope values are always in the valid set", {
  live_con()
  # Restrict to terms known to have synonyms for efficiency
  ids_with_synonyms <- GO.ddb::go_synonyms() |>
    dplyr::distinct(id) |>
    dplyr::collect() |>
    dplyr::pull(id)

  valid_scopes <- c("exact", "related", "narrow", "broad")

  forall(
    gen.element(ids_with_synonyms),
    function(id) {
      scopes <- GO.ddb::go_synonyms(id) |>
        dplyr::pull(scope)

      expect_true(
        all(scopes %in% valid_scopes),
        label = sprintf(
          "Invalid scope(s) for %s: %s",
          id, paste(setdiff(scopes, valid_scopes), collapse = ", ")
        )
      )
    },
    tests = 30L
  )
})


# ── Property 9: ontology roots have no ancestors and many descendants ─────────
#
# The three GO namespace roots are structurally well-defined:
# they have no is_a ancestors (they ARE the root) and should have
# a large number of descendants.

test_that("GO namespace roots have no GO-prefixed is_a ancestors", {
  live_con()

  roots <- c(
    "GO:0008150",  # biological_process
    "GO:0003674",  # molecular_function
    "GO:0005575"   # cellular_component
  )

  forall(
    gen.element(roots),
    function(id) {
      n_ancestors <- GO.ddb::go_ancestors(
        id, relations = GO.ddb::GO_RELATIONS["is_a"]
      ) |>
        dplyr::filter(ancestor_id %like% "GO:%") |>
        dplyr::count() |>
        dplyr::pull(n)

      expect_equal(
        n_ancestors, 0L,
        label = sprintf(
          "Root term %s should have no GO-prefixed is_a ancestors", id
        )
      )
    }
  )
})

test_that("GO namespace roots each have many is_a descendants", {
  live_con()

  roots <- c(
    "GO:0008150",  # biological_process
    "GO:0003674",  # molecular_function
    "GO:0005575"   # cellular_component
  )

  forall(
    gen.element(roots),
    function(id) {
      n_descendants <- GO.ddb::go_descendants(
        id, relations = GO.ddb::GO_RELATIONS["is_a"]
      ) |>
        dplyr::count() |>
        dplyr::pull(n)

      expect_gt(
        n_descendants, 1000L,
        label = sprintf(
          "Root term %s should have >1000 descendants, got %d",
          id, n_descendants
        )
      )
    }
  )
})
