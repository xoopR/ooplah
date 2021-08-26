test_that("ooplah class behaves as expected", {
  obj <- ooplah$new()
  expect_equal(private(obj)$.goodbye, "Goodbye World")
  expect_true(obj$logically)
  obj$logically <- FALSE
  expect_false(obj$logically)
})
