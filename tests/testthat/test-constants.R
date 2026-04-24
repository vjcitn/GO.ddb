# test-constants.R — GO_RELATIONS

test_that("GO_RELATIONS is a named character vector", {
  expect_type(GO_RELATIONS, "character")
  expect_named(GO_RELATIONS)
})

test_that("GO_RELATIONS contains the expected relation names", {
  expect_setequal(
    names(GO_RELATIONS),
    c("is_a", "part_of", "has_part", "occurs_in", "located_in")
  )
})

test_that("GO_RELATIONS CURIEs have expected values", {
  expect_equal(unname(GO_RELATIONS["is_a"]),       "rdfs:subClassOf")
  expect_equal(unname(GO_RELATIONS["part_of"]),    "BFO:0000050")
  expect_equal(unname(GO_RELATIONS["has_part"]),   "BFO:0000051")
  expect_equal(unname(GO_RELATIONS["occurs_in"]),  "BFO:0000066")
  expect_equal(unname(GO_RELATIONS["located_in"]), "RO:0001025")
})

test_that("GO_RELATIONS values are unique", {
  expect_equal(length(GO_RELATIONS), length(unique(GO_RELATIONS)))
})
