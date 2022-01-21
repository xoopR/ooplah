test_that("can create an abstract class", {
  expect_silent(AbstractClass("abstract"))
  expect_silent(AbstractClass("abstract", public = list(hi = "hi")))
  abs <- AbstractClass("abstract",
                       public = list(initialize = function(x) cat("hi")))
  expect_output(R6Class("child", inherit = abs)$new(), "hi")
  expect_error(abs$new(), "is an abstract class")
})

test_that("can't construct an abstract class", {
  expect_error(OoplahParent$new(), "is an abstract class")
})

test_that("can construct an abstract classes child", {
  expect_true(ooplah$new()$init)
})
