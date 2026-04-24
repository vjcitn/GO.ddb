# test-go_terms.R — go_terms()

test_that("go_terms() returns a lazy tbl", {
  live_con()
  expect_s3_class(go_terms(), "tbl_lazy")
})

test_that("go_terms() has expected columns", {
  live_con()
  expect_setequal(
    colnames(go_terms()),
    c("id", "label", "definition", "ontology", "deprecated")
  )
})

test_that("go_terms() ids all have GO: prefix", {
  live_con()
  ids <- go_terms() |>
    dplyr::select(id) |>
    dplyr::collect() |>
    dplyr::pull(id)

  expect_true(all(startsWith(ids, "GO:")))
})

test_that("go_terms() excludes deprecated terms by default", {
  live_con()
  n_deprecated <- go_terms() |>
    dplyr::filter(deprecated) |>
    dplyr::count() |>
    dplyr::pull(n)

  expect_equal(n_deprecated, 0L)
})

test_that("go_terms(include_deprecated = TRUE) includes deprecated terms", {
  live_con()
  n <- go_terms(include_deprecated = TRUE) |>
    dplyr::filter(deprecated) |>
    dplyr::count() |>
    dplyr::pull(n)

  expect_gt(n, 0L)
})

test_that("go_terms() ontology values are in expected set", {
  live_con()
  ontologies <- go_terms() |>
    dplyr::distinct(ontology) |>
    dplyr::collect() |>
    dplyr::pull(ontology)

  expect_true(all(ontologies %in%
    c("biological_process", "molecular_function", "cellular_component", NA)
  ))
})

test_that("go_terms() non-deprecated count is substantial", {
  live_con()
  n <- go_terms() |>
    dplyr::count() |>
    dplyr::pull(n)

  # GO has ~45k non-deprecated terms as of 2024
  expect_gt(n, 30000L)
})

test_that("go_terms() has more rows with include_deprecated = TRUE", {
  live_con()
  n_live  <- go_terms()                          |> dplyr::count() |> dplyr::pull(n)
  n_all   <- go_terms(include_deprecated = TRUE) |> dplyr::count() |> dplyr::pull(n)
  expect_gt(n_all, n_live)
})
