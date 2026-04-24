# test-connection.R — connection lifecycle

test_that("go_connection_active() returns FALSE before any connection", {
  # Ensure clean state
  suppressMessages(disconnect_go())
  expect_false(go_connection_active())
})

test_that("make_go_con() establishes a connection", {
  live_con()   # skip if ontoProc2 unavailable
  expect_true(go_connection_active())
})

test_that("make_go_con() is idempotent — second call emits message, not error", {
  live_con()
  expect_message(make_go_con(), "already active")
  expect_true(go_connection_active())
})

test_that("disconnect_go() closes the connection", {
  live_con()
  suppressMessages(disconnect_go())
  expect_false(go_connection_active())
})

test_that("disconnect_go() on an inactive connection emits message, not error", {
  suppressMessages(disconnect_go())
  expect_message(disconnect_go(), "No active")
})

test_that("go_connection_active() returns TRUE after make_go_con()", {
  live_con()
  expect_true(go_connection_active())
  suppressMessages(disconnect_go())
})
