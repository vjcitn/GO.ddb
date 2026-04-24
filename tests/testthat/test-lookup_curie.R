# test-lookup_curie.R — lookup_curie()
#
# GO:0006954  inflammatory response        (biological_process)
# GO:0008150  biological_process (root)    (biological_process)
# GO:0003674  molecular_function (root)    (molecular_function)

test_that("lookup_curie() errors on empty curies", {
  expect_error(lookup_curie(character(0)), "`curies` must be")
})

test_that("lookup_curie() errors on non-character curies", {
  expect_error(lookup_curie(123L), "`curies` must be")
})

test_that("lookup_curie() warns on non-GO CURIEs", {
  live_con()
  expect_warning(
    lookup_curie("BOGUS:9999", mapto = "term"),
    "not valid GO CURIEs"
  )
})

test_that("lookup_curie() mapto partial matching works", {
  live_con()
  expect_no_error(
    lookup_curie("GO:0006954", mapto = "def") |> dplyr::collect()
  )
})

#test_that("lookup_curie() errors on ambiguous mapto", {
#  expect_error(match.arg("o", c("term", "definition", "ontology", "all")))
#})

test_that("lookup_curie(mapto='term') returns id and label columns", {
  live_con()
  result <- lookup_curie("GO:0006954", mapto = "term") |> dplyr::collect()
  expect_setequal(colnames(result), c("id", "label"))
})

test_that("lookup_curie(mapto='definition') returns id and definition columns", {
  live_con()
  result <- lookup_curie("GO:0006954", mapto = "definition") |> dplyr::collect()
  expect_setequal(colnames(result), c("id", "definition"))
})

test_that("lookup_curie(mapto='ontology') returns id and ontology columns", {
  live_con()
  result <- lookup_curie("GO:0006954", mapto = "ontology") |> dplyr::collect()
  expect_setequal(colnames(result), c("id", "ontology"))
})

test_that("lookup_curie(mapto='all') returns all four columns", {
  live_con()
  result <- lookup_curie("GO:0006954", mapto = "all") |> dplyr::collect()
  expect_setequal(colnames(result), c("id", "label", "definition", "ontology"))
})

test_that("lookup_curie() returns correct label for GO:0006954", {
  live_con()
  label <- lookup_curie("GO:0006954", mapto = "term") |>
    dplyr::collect() |>
    dplyr::pull(label)

  expect_equal(label, "inflammatory response")
})

test_that("lookup_curie() returns correct ontology for GO:0006954", {
  live_con()
  ont <- lookup_curie("GO:0006954", mapto = "ontology") |>
    dplyr::collect() |>
    dplyr::pull(ontology)

  expect_equal(ont, "biological_process")
})

test_that("lookup_curie() handles multiple CURIEs", {
  live_con()
  result <- lookup_curie(
    c("GO:0006954", "GO:0003674"),
    mapto = "term"
  ) |> dplyr::collect()

  expect_equal(nrow(result), 2L)
  expect_setequal(result$id, c("GO:0006954", "GO:0003674"))
})

test_that("lookup_curie() returns zero rows for unknown GO CURIE (no warning)", {
  live_con()
  # Structurally valid GO CURIE that does not exist
  result <- suppressWarnings(
    lookup_curie("GO:9999999", mapto = "term") |> dplyr::collect()
  )
  expect_equal(nrow(result), 0L)
})


test_that("lookup_curie() errors on invalid mapto", {
  live_con()
  expect_error(
    lookup_curie("GO:0006954", mapto = "banana"),
    "should be"
  )
})
