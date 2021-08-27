test_that("private works", {
  obj <- ooplah$new()
  expect_identical(private(obj), obj$.__enclos_env__$private)
})

test_that("super works", {
  obj <- ooplah$new()
  expect_identical(super(obj), obj$.__enclos_env__$super)
})
