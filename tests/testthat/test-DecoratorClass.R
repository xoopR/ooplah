oopl <- ooplah$new()

test_that("can create a decorator class", {
  dec <- DecoratorClass("dec", public = list(
    sleep = function() "Zzzz"
  ))
  expect_equal(class(dec$new(oopl)), c("dec", "Decorator", "R6"))
})

dec <- DecoratorClass("dec", public = list(
                      sleep = function() "Zzzz"), abstract = TRUE)

test_that("cannot construct an abstractor decorator", {
  expect_error(dec$new(oopl), "abstract class")
})

dec_child <- DecoratorClass("dec_child", inherit = dec)
obj_dec <- dec_child$new(oopl)

test_that("can construct a decorator's child", {
  expect_equal(class(obj_dec),
               c("dec_child", "dec", "Decorator", "R6"))
  expect_equal(obj_dec$sleep(), "Zzzz")
  expect_equal(obj_dec[["sleep"]](), "Zzzz")
})

test_that("can access original methods/fields", {
  expect_equal(obj_dec$hello(), "Hello World, Ooplah!")
  expect_equal(obj_dec$init, TRUE)
})

test_that("active bindings work", {
  obj_dec$logically <- FALSE
  expect_false(obj_dec$logically)

  obj_dec[["logically"]] <- TRUE
  expect_true(obj_dec[["logically"]])
})
