env <- new.env()

test_that("can create a decorator class", {
  expect_equal(class(DecoratorClass("dec")$new(ooplah$new())),
               c("dec", "Decorator", "R6"))
})

test_that("cannot construct an abstractor decorator", {
  withr::with_environment(env, {
    oopl <- ooplah$new()
    dec <- DecoratorClass("dec", public = list(
      sleep = function() "Zzzz"
    ), abstract = TRUE)

    expect_error(dec$new(oopl), "abstract class")
  })
})

test_that("can construct a decorator's child", {
  withr::with_environment(env, {
    oopl <- ooplah$new()
    dec <- DecoratorClass("dec", public = list(
      sleep = function() "Zzzz"
    ), abstract = TRUE)
    dec_child <- DecoratorClass("dec_child", inherit = dec)
    obj_dec <- dec_child$new(oopl)

    expect_equal(class(obj_dec),
                  c("dec_child", "dec", "Decorator", "R6"))
    expect_equal(obj_dec$sleep(), "Zzzz")
    expect_equal(obj_dec[["sleep"]](), "Zzzz")
  })
})

test_that("can access original methods/fields", {
  withr::with_environment(env, {
    oopl <- ooplah$new()
    dec <- DecoratorClass("dec", public = list(
      sleep = function() "Zzzz"
    ), abstract = TRUE)
    dec_child <- DecoratorClass("dec_child", inherit = dec)
    obj_dec <- dec_child$new(oopl)

    expect_equal(obj_dec$hello(), "Hello World, Ooplah!")
    expect_equal(obj_dec$init, TRUE)
  })
})

test_that("active bindings work", {
  withr::with_environment(env, {
    oopl <- ooplah$new()
    dec <- DecoratorClass("dec", public = list(
      sleep = function() "Zzzz"
    ), abstract = TRUE)
    dec_child <- DecoratorClass("dec_child", inherit = dec)
    obj_dec <- dec_child$new(oopl)

    obj_dec$logically <- FALSE
    expect_false(obj_dec$logically)

    obj_dec[["logically"]] <- TRUE
    expect_true(obj_dec[["logically"]])
  })
})
