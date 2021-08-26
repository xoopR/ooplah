test_that("private", {
  expect_equal(private(ooplah$new()), ooplah$new()$.__enclos_env__$private)
})