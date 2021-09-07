oopl <- ooplah$new()
dec <- DecoratorClass("dec", public = list(
  sleep = function() "Zzzz"
), abstract = TRUE)
dec_child <- DecoratorClass("dec_child", inherit = dec)
obj_dec <- dec_child$new(oopl)

test_that("can create a decorator class", {
  expect_equal(class(DecoratorClass("dec")$new(oopl)),
               c("dec", "ooplah", "ooplahParent", "Decorator", "R6"))
})

test_that("cannot construct an abstractor decorator", {
  expect_error(dec$new(oopl), "abstract class")
})


test_that("can construct a decorator's child", {
  expect_equal(class(obj_dec),
                c("dec_child", "dec", "ooplah", "ooplahParent", "Decorator",
                  "R6"))
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

test_that("error on decorating existing methods", {
  exists <- DecoratorClass("exists", public  = list(hello = function() "Oh no"))
  expect_error(exists$new(oopl, exists = "error"), "Fields/methods")
  expect_error(exists$new(oopl), "Fields/methods")
})

test_that("skip on decorating existing methods", {
  exists <- DecoratorClass("exists",
    public = list(hello = function() "Oh no")
  )
  expect_equal(exists$new(oopl, exists = "skip")$hello(),
               "Hello World, Ooplah!")
})

test_that("skip on decorating existing methods - switch to field", {
  exists <- DecoratorClass("exists", public  = list(hello = "Oh no"))
  expect_equal(exists$new(oopl, exists = "skip")$hello(),
               "Hello World, Ooplah!")
})

test_that("overwrite on decorating existing methods", {
  exists <- DecoratorClass("exists", public  = list(hello = "Oh no"))
  expect_equal(exists$new(oopl, exists = "overwrite")$hello, "Oh no")
})


test_that("can't clone a decorator", {
  oopl <- ooplah$new()

  expect_error(DecoratorClass("clone", cloneable = TRUE), "not cloneable")
  dec <- DecoratorClass("clone")$new(oopl)

  expect_equal(dec$clone, NULL)
  expect_error(dec$clone(), "non-function")

  # original clone still works
  oopl2 <- oopl$clone(deep = TRUE)
  oopl$logically <- FALSE
  expect_true(oopl2$logically)
  expect_false(oopl$logically)
  expect_false(dec$logically)
})
