test_that("can't construct an abstract class", {
  expect_error(ooplahParent$new(), "is an abstract class")
})

test_that("can construct an abstract classes child", {
  expect_true(inherits(ooplah$new(), "ooplahParent"))
})
