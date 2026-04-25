test_that("build_parquet_cache() writes expected parquet files", {
  skip_if_not_installed("ontoProc2")

  tmp_dir  <- withr::local_tempdir()
  tmp_db   <- file.path(tmp_dir, "test.sqlite")

  # Build a minimal SQLite with the required table structure
  con <- DBI::dbConnect(RSQLite::SQLite(), tmp_db)
  DBI::dbWriteTable(con, "statements",
    data.frame(
      stanza    = "GO:0000001",
      subject   = "GO:0000001",
      predicate = "rdfs:label",
      object    = NA_character_,
      value     = "test term",
      datatype  = NA_character_,
      language  = NA_character_
    )
  )
  DBI::dbWriteTable(con, "entailed_edge",
    data.frame(
      subject   = "GO:0000001",
      predicate = "rdfs:subClassOf",
      object    = "GO:0000001"
    )
  )
  DBI::dbDisconnect(con)

  out_dir <- file.path(tmp_dir, "parquet")
  dir.create(out_dir)

  # Patch .parquet_cache_dir to point at our temp location
  withr::with_envvar(list(GODDB_PARQUET_DIR = out_dir), {
    build_parquet_cache(sqlite_path = tmp_db, out_dir = out_dir)
  })

  expect_true(file.exists(file.path(out_dir, "statements.parquet")))
  expect_true(file.exists(file.path(out_dir, "entailed_edge.parquet")))
})

