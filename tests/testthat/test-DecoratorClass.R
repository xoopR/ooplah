oopl <- ooplah$new()

test_that("can create a decorator class", {
  dec <- DecoratorClass("dec", public = list(
    sleep = function() "Zzzz"
  ))
  expect_equal(class(dec$new(oopl)), c("dec", "Decorator", "R6"))
})

test_that("cannot construct an abstractor decorator", {
  dec <- DecoratorClass("dec", public = list(
    sleep = function() "Zzzz"
  ), abstract = TRUE)
  expect_error(dec$new(oopl), "abstract class")

  dec_child <- DecoratorClass("dec_child", inherit = dec)
  dec_child$sleep()
})
  oopl <- ooplah$new()
  dobj <- dec$new(oopl)
  dobj$init
  dobj[["init"]]
  dobj$logically <- FALSE
  dobj[["logically"]] <- TRUE

  oopl$logically <- FALSE
  expect_false(dobj$logically)

  dobj$logically <- TRUE
  expect_true(oopl$logically)
})



test_that("DecoratorClass fails when expected", {
  expect_error(DecoratorClass("decorate"), "at least one")
})
