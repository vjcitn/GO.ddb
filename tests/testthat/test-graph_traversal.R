# test-graph_traversal.R — go_ancestors() and go_descendants()
#
# Reference terms:
#   GO:0006954  inflammatory response   (BP, specific)
#   GO:0006950  response to stress      (BP, broader)
#   GO:0008150  biological_process      (BP root)
#   GO:0005575  cellular_component      (CC root)

# ── Input validation (no connection needed) ──────────────────────────────────

test_that("go_ancestors() errors on empty ids", {
  expect_error(go_ancestors(character(0)), "`ids` must be")
})

test_that("go_ancestors() errors on non-character ids", {
  expect_error(go_ancestors(1L), "`ids` must be")
})

test_that("go_ancestors() errors on empty relations", {
  expect_error(
    go_ancestors("GO:0006954", relations = character(0)),
    "`relations` must be"
  )
})

test_that("go_descendants() errors on empty ids", {
  expect_error(go_descendants(character(0)), "`ids` must be")
})

test_that("go_descendants() errors on non-character ids", {
  expect_error(go_descendants(42L), "`ids` must be")
})

test_that("go_descendants() errors on empty relations", {
  expect_error(
    go_descendants("GO:0008150", relations = character(0)),
    "`relations` must be"
  )
})

# ── go_ancestors() with live connection ──────────────────────────────────────

test_that("go_ancestors() returns a lazy tbl with expected columns", {
  live_con()
  result <- go_ancestors("GO:0006954")
  expect_s3_class(result, "tbl_lazy")
  expect_setequal(colnames(result), c("id", "ancestor_id", "relation"))
})

test_that("go_ancestors() returns ancestors for a known BP term", {
  live_con()
  result <- go_ancestors("GO:0006954") |> dplyr::collect()
  expect_gt(nrow(result), 0L)
  expect_true(all(result$id == "GO:0006954"))
})

test_that("go_ancestors() finds the BP root GO:0008150 for GO:0006954", {
  live_con()
  ancestors <- go_ancestors("GO:0006954") |>
    dplyr::pull(ancestor_id)

  expect_true("GO:0008150" %in% ancestors)
})

test_that("go_ancestors() excludes self-edges by default", {
  live_con()
  result <- go_ancestors("GO:0006954") |>
    dplyr::filter(id == ancestor_id) |>
    dplyr::collect()

  expect_equal(nrow(result), 0L)
})

test_that("go_ancestors(include_self = TRUE) includes the query term", {
  live_con()
  result <- go_ancestors("GO:0006954", include_self = TRUE) |>
    dplyr::filter(id == ancestor_id) |>
    dplyr::collect()

  expect_gt(nrow(result), 0L)
})

test_that("go_ancestors() relation filter is respected", {
  live_con()
  result <- go_ancestors(
    "GO:0006954",
    relations = GO_RELATIONS["is_a"]
  ) |> dplyr::collect()

  expect_true(all(result$relation == "rdfs:subClassOf"))
})

test_that("go_ancestors() handles multiple query terms", {
  live_con()
  result <- go_ancestors(c("GO:0006954", "GO:0006950")) |>
    dplyr::collect()

  expect_true("GO:0006954" %in% result$id)
  expect_true("GO:0006950" %in% result$id)
})

# ── go_descendants() with live connection ────────────────────────────────────

test_that("go_descendants() returns a lazy tbl with expected columns", {
  live_con()
  result <- go_descendants("GO:0006950")
  expect_s3_class(result, "tbl_lazy")
  expect_setequal(colnames(result), c("id", "descendant_id", "relation"))
})

test_that("go_descendants() finds descendants for a known BP term", {
  live_con()
  result <- go_descendants("GO:0006950") |> dplyr::collect()
  expect_gt(nrow(result), 0L)
  expect_true(all(result$id == "GO:0006950"))
})

test_that("go_descendants() finds GO:0006954 as descendant of GO:0006950", {
  live_con()
  descendants <- go_descendants("GO:0006950") |>
    dplyr::pull(descendant_id)

  expect_true("GO:0006954" %in% descendants)
})

test_that("go_descendants() excludes self-edges by default", {
  live_con()
  result <- go_descendants("GO:0006950") |>
    dplyr::filter(id == descendant_id) |>
    dplyr::collect()

  expect_equal(nrow(result), 0L)
})

test_that("go_descendants(include_self = TRUE) includes the query term", {
  live_con()
  result <- go_descendants("GO:0006950", include_self = TRUE) |>
    dplyr::filter(id == descendant_id) |>
    dplyr::collect()

  expect_gt(nrow(result), 0L)
})

test_that("go_ancestors() and go_descendants() are consistent", {
  live_con()
  # If A is ancestor of B then B should be a descendant of A
  ancestors_of_954 <- go_ancestors("GO:0006954") |>
    dplyr::pull(ancestor_id)

  # GO:0006950 should be an ancestor of GO:0006954
  expect_true("GO:0006950" %in% ancestors_of_954)

  descendants_of_950 <- go_descendants("GO:0006950") |>
    dplyr::pull(descendant_id)

  # GO:0006954 should be a descendant of GO:0006950
  expect_true("GO:0006954" %in% descendants_of_950)
})

test_that("go_descendants() of BP root returns many terms", {
  live_con()
  n <- go_descendants("GO:0008150",
    relations = GO_RELATIONS["is_a"]) |>
    dplyr::count() |>
    dplyr::pull(n)

  expect_gt(n, 10000L)
})
