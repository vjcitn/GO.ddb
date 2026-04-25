# test-go_synonyms.R — go_synonyms()
#
# Reference terms:
#   GO:0006954  inflammatory response — known to have exact synonyms
#   GO:0008150  biological_process root

# ── Input validation ──────────────────────────────────────────────────────────

test_that("go_synonyms() errors on non-character ids", {
  expect_error(GO.ddb::go_synonyms(ids = 123L), "`ids` must be")
})

test_that("go_synonyms() errors on empty ids vector", {
  expect_error(GO.ddb::go_synonyms(ids = character(0)), "`ids` must be")
})

test_that("go_synonyms() errors on invalid types", {
  expect_error(
    GO.ddb::go_synonyms(types = "verbatim"),
    "'arg' should be one of"
  )
})

test_that("go_synonyms() accepts multiple valid types", {
  expect_no_error(
    GO.ddb::go_synonyms(types = c("exact", "related"))
  )
})

# ── Structure ─────────────────────────────────────────────────────────────────

test_that("go_synonyms() returns a lazy tbl", {
  live_con()
  expect_s3_class(GO.ddb::go_synonyms(), "tbl_lazy")
})

test_that("go_synonyms() has expected columns", {
  live_con()
  expect_setequal(
    colnames(GO.ddb::go_synonyms()),
    c("id", "synonym", "scope")
  )
})

test_that("go_synonyms() ids all have GO: prefix", {
  live_con()
  ids <- GO.ddb::go_synonyms() |>
    dplyr::select(id) |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::pull(id)

  expect_true(all(startsWith(ids, "GO:")))
})

test_that("go_synonyms() scope values are in expected set", {
  live_con()
  scopes <- GO.ddb::go_synonyms() |>
    dplyr::distinct(scope) |>
    dplyr::collect() |>
    dplyr::pull(scope)

  expect_true(all(scopes %in% c("exact", "related", "narrow", "broad")))
})

# ── Content ───────────────────────────────────────────────────────────────────

test_that("go_synonyms() returns synonyms for GO:0006954", {
  live_con()
  result <- GO.ddb::go_synonyms("GO:0006954") |> dplyr::collect()
  expect_gt(nrow(result), 0L)
  expect_true(all(result$id == "GO:0006954"))
})

test_that("go_synonyms() returns non-empty results across all terms", {
  live_con()
  n <- GO.ddb::go_synonyms() |>
    dplyr::count() |>
    dplyr::pull(n)
  # GO has ~92k exact synonyms alone in the 2024 build
  expect_gt(n, 50000L)
})

test_that("go_synonyms(types='exact') returns only exact scope", {
  live_con()
  scopes <- GO.ddb::go_synonyms(types = "exact") |>
    dplyr::distinct(scope) |>
    dplyr::collect() |>
    dplyr::pull(scope)

  expect_setequal(scopes, "exact")
})

test_that("go_synonyms(types='broad') returns only broad scope", {
  live_con()
  scopes <- GO.ddb::go_synonyms(types = "broad") |>
    dplyr::distinct(scope) |>
    dplyr::collect() |>
    dplyr::pull(scope)

  expect_setequal(scopes, "broad")
})

test_that("go_synonyms() with multiple types returns only those scopes", {
  live_con()
  scopes <- GO.ddb::go_synonyms(types = c("exact", "narrow")) |>
    dplyr::distinct(scope) |>
    dplyr::collect() |>
    dplyr::pull(scope)

  expect_true(all(scopes %in% c("exact", "narrow")))
})

test_that("go_synonyms() with NULL ids returns more rows than specific ids", {
  live_con()
  n_all <- GO.ddb::go_synonyms() |>
    dplyr::count() |> dplyr::pull(n)
  n_one <- GO.ddb::go_synonyms("GO:0006954") |>
    dplyr::count() |> dplyr::pull(n)

  expect_gt(n_all, n_one)
})

test_that("go_synonyms() handles multiple ids", {
  live_con()
  result <- GO.ddb::go_synonyms(
    c("GO:0006954", "GO:0008150")
  ) |> dplyr::collect()

  expect_true("GO:0006954" %in% result$id)
  # GO:0008150 (BP root) may have no synonyms — that is valid
  expect_gt(nrow(result), 0L)
})

test_that("go_synonyms() unknown id returns zero rows without error", {
  live_con()
  result <- GO.ddb::go_synonyms("GO:9999999") |> dplyr::collect()
  expect_equal(nrow(result), 0L)
})
