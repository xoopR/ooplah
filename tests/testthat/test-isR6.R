objlah <- ooplah$new()

test_that("is.R6X", {
  expect_true(is.R6(ooplah))
  expect_true(is.R6(objlah))
  expect_false(is.R6(1))

  expect_true(is.R6Class(ooplah))
  expect_false(is.R6Class(objlah))

  expect_false(is.R6Object(ooplah))
  expect_true(is.R6Object(objlah))
})

test_that("assert R6X", {
  expect_equal(assert_R6(ooplah), ooplah)
  expect_equal(assert_R6(objlah), objlah)
  expect_error(assert_R6(1), "'1' is not")

  expect_equal(assert_R6Class(ooplah), ooplah)
  expect_error(assert_R6Class(objlah), "'objlah' is not an R6 class")

  expect_equal(assert_R6Object(objlah), objlah)
  expect_error(assert_R6Object(ooplah), "'ooplah' is not an R6 object")
})
