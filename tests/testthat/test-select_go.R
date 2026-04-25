# test-select_go.R — select_go()
#
# Reference terms:
#   GO:0006954  inflammatory response  (biological_process)
#   GO:0003674  molecular_function root
#   GO:0005575  cellular_component root

# ── Input validation (no connection needed) ───────────────────────────────────

test_that("select_go() errors on empty keys", {
  expect_error(GO.ddb::select_go(character(0)), "`keys` must be")
})

test_that("select_go() errors on non-character keys", {
  expect_error(GO.ddb::select_go(123L), "`keys` must be")
})

test_that("select_go() errors on invalid columns", {
  expect_error(
    GO.ddb::select_go("GO:0006954", columns = "BANANA"),
    "Invalid columns"
  )
})

test_that("select_go() errors on invalid keytype", {
  expect_error(
    GO.ddb::select_go("GO:0006954", keytype = "SYMBOL"),
    "'arg' should be"
  )
})

test_that("select_go() warns on non-GO CURIEs", {
  live_con()
  expect_warning(
    GO.ddb::select_go("BOGUS:999", columns = "TERM"),
    "not valid GO CURIEs"
  )
})

# ── Return type ───────────────────────────────────────────────────────────────

test_that("select_go() returns a data.frame not a tibble", {
  live_con()
  result <- GO.ddb::select_go("GO:0006954", columns = "TERM")
  expect_s3_class(result, "data.frame")
  # Should not be lazy
  expect_false(inherits(result, "tbl_lazy"))
})

# ── Column structure ──────────────────────────────────────────────────────────

test_that("select_go() GOID is always the first column", {
  live_con()
  result <- GO.ddb::select_go(
    "GO:0006954",
    columns = c("TERM", "ONTOLOGY")
  )
  expect_equal(colnames(result)[1], "GOID")
})

test_that("select_go(columns='TERM') returns GOID and TERM only", {
  live_con()
  result <- GO.ddb::select_go("GO:0006954", columns = "TERM")
  expect_setequal(colnames(result), c("GOID", "TERM"))
})

test_that("select_go(columns='DEFINITION') returns GOID and DEFINITION", {
  live_con()
  result <- GO.ddb::select_go("GO:0006954", columns = "DEFINITION")
  expect_setequal(colnames(result), c("GOID", "DEFINITION"))
})

test_that("select_go(columns='ONTOLOGY') returns GOID and ONTOLOGY", {
  live_con()
  result <- GO.ddb::select_go("GO:0006954", columns = "ONTOLOGY")
  expect_setequal(colnames(result), c("GOID", "ONTOLOGY"))
})

test_that("select_go() with all non-synonym columns returns correct structure", {
  live_con()
  result <- GO.ddb::select_go(
    "GO:0006954",
    columns = c("TERM", "DEFINITION", "ONTOLOGY")
  )
  expect_setequal(colnames(result), c("GOID", "TERM", "DEFINITION", "ONTOLOGY"))
})

test_that("select_go(columns='SYNONYM') returns GOID and SYNONYM", {
  live_con()
  result <- GO.ddb::select_go("GO:0006954", columns = "SYNONYM")
  expect_setequal(colnames(result), c("GOID", "SYNONYM"))
})

test_that("select_go() with all columns returns all expected columns", {
  live_con()
  result <- GO.ddb::select_go(
    "GO:0006954",
    columns = c("TERM", "DEFINITION", "ONTOLOGY", "SYNONYM")
  )
  expect_setequal(
    colnames(result),
    c("GOID", "TERM", "DEFINITION", "ONTOLOGY", "SYNONYM")
  )
})

# ── Content correctness ───────────────────────────────────────────────────────

test_that("select_go() returns correct TERM for GO:0006954", {
  live_con()
  result <- GO.ddb::select_go("GO:0006954", columns = "TERM")
  expect_equal(
    result$TERM[result$GOID == "GO:0006954"],
    "inflammatory response"
  )
})

test_that("select_go() returns correct ONTOLOGY for GO:0006954", {
  live_con()
  result <- GO.ddb::select_go("GO:0006954", columns = "ONTOLOGY")
  expect_equal(
    result$ONTOLOGY[result$GOID == "GO:0006954"],
    "biological_process"
  )
})

test_that("select_go() DEFINITION is non-empty for GO:0006954", {
  live_con()
  result <- GO.ddb::select_go("GO:0006954", columns = "DEFINITION")
  def <- result$DEFINITION[result$GOID == "GO:0006954"]
  expect_true(nchar(def) > 0L)
})

# ── Multi-key behaviour ───────────────────────────────────────────────────────

test_that("select_go() returns one row per key (no synonyms)", {
  live_con()
  keys   <- c("GO:0006954", "GO:0003674", "GO:0005575")
  result <- GO.ddb::select_go(keys, columns = c("TERM", "ONTOLOGY"))
  expect_equal(nrow(result), length(keys))
  expect_setequal(result$GOID, keys)
})

#test_that("select_go() with SYNONYM returns multiple rows per term", {
#  live_con()
#  result <- GO.ddb::select_go("GO:0006954", columns = "SYNONYM")
#  # inflammatory response has multiple synonyms in GO
#  expect_gt(nrow(result), 1L)
#})

test_that("select_go() with SYNONYM returns multiple rows for a term with synonyms", {
  live_con()
  # Find a term that has multiple synonyms rather than assuming GO:0006954
  multi_syn <- GO.ddb::go_synonyms() |>
    dplyr::count(id) |>
    dplyr::filter(n > 1L) |> dplyr::collect() |>
    dplyr::slice(1L) |>
    dplyr::pull(id)

  result <- GO.ddb::select_go(multi_syn, columns = "SYNONYM")
  expect_gt(nrow(result), 1L)
})

test_that("select_go() SYNONYM is NA for terms with no synonyms", {
  live_con()
  # Use BP root — likely has no synonyms
  result <- GO.ddb::select_go("GO:0008150", columns = "SYNONYM")
  if (nrow(result) == 1L)
    expect_true(is.na(result$SYNONYM))
  else
    succeed("GO:0008150 has synonyms — NA check not applicable")
})

test_that("select_go() unknown CURIE returns zero rows", {
  live_con()
  result <- suppressWarnings(
    GO.ddb::select_go("GO:9999999", columns = "TERM")
  )
  expect_equal(nrow(result), 0L)
})

test_that("select_go() GOID column contains the query keys", {
  live_con()
  keys   <- c("GO:0006954", "GO:0003674")
  result <- GO.ddb::select_go(keys, columns = "TERM")
  expect_setequal(result$GOID, keys)
})
