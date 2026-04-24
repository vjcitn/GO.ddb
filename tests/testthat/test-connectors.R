# test-connectors.R — go_statements() and go_entailed_edges()

test_that("go_statements() returns a lazy tbl with expected columns", {
  live_con()
  st <- go_statements()

  expect_s3_class(st, "tbl_lazy")

  cols <- colnames(st)
  expect_true(all(c("subject", "predicate", "value") %in% cols))
})

test_that("go_statements() contains rdfs:label triples", {
  live_con()
  n <- go_statements() |>
    dplyr::filter(predicate == "rdfs:label") |>
    dplyr::count() |>
    dplyr::pull(n)

  expect_gt(n, 0L)
})

test_that("go_statements() contains IAO:0000115 (definition) triples", {
  live_con()
  n <- go_statements() |>
    dplyr::filter(predicate == "IAO:0000115") |>
    dplyr::count() |>
    dplyr::pull(n)

  expect_gt(n, 0L)
})

test_that("go_entailed_edges() returns a lazy tbl with expected columns", {
  live_con()
  ee <- go_entailed_edges()

  expect_s3_class(ee, "tbl_lazy")
  expect_setequal(colnames(ee), c("subject", "predicate", "object"))
})

test_that("go_entailed_edges() contains rdfs:subClassOf edges", {
  live_con()
  n <- go_entailed_edges() |>
    dplyr::filter(predicate == "rdfs:subClassOf") |>
    dplyr::count() |>
    dplyr::pull(n)

  expect_gt(n, 1e5L)
})

test_that("go_entailed_edges() contains BFO:0000050 (part_of) edges", {
  live_con()
  n <- go_entailed_edges() |>
    dplyr::filter(predicate == "BFO:0000050") |>
    dplyr::count() |>
    dplyr::pull(n)

  expect_gt(n, 0L)
})

test_that("go_entailed_edges() has self-edges (reflexive closure)", {
  live_con()
  n <- go_entailed_edges() |>
    dplyr::filter(subject == object) |>
    dplyr::count() |>
    dplyr::pull(n)

  expect_gt(n, 0L)
})
