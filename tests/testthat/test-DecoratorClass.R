oopl <- ooplah$new()
dec <- DecoratorClass("dec",
  public = list(
    sleep = function() "Zzzz"
  ),
  active = list(test = function(x) {
    if (missing(x)) private$.test else private$.test <- x
  }),
  private = list(.test = FALSE), abstract = TRUE
)
dec_child <- DecoratorClass("dec_child", inherit = dec)
obj_dec <- dec_child$new(oopl)

test_that("can create a decorator class", {
  expect_equal(class(DecoratorClass("dec")$new(oopl)),
               c("dec", "ooplah", "OoplahParent", "Decorator", "R6"))
})

test_that("cannot construct an abstractor decorator", {
  expect_error(dec$new(oopl), "abstract class")
})


test_that("can construct a decorator's child", {
  expect_equal(class(obj_dec),
                c("dec_child", "dec", "ooplah", "OoplahParent", "Decorator",
                  "R6"))
  expect_equal(obj_dec$sleep(), "Zzzz")
  expect_equal(obj_dec[["sleep"]](), "Zzzz")
  expect_false(obj_dec$test)
  expect_false(obj_dec[["test"]])
  obj_dec[["test"]] <- TRUE
  expect_true(obj_dec$test)
  expect_true(obj_dec[["test"]])
  obj_dec$test <- FALSE
  expect_false(obj_dec$test)
  expect_false(obj_dec[["test"]])
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

test_that("skip on decorating existing methods - $", {
  exists <- DecoratorClass("exists",
    public = list(hello = function() "Oh no", goodbye = function() "Bye")
  )
  dec <- exists$new(oopl, exists = "skip")
  expect_equal(private(dec)$.exists, "skip")
  expect_equal(dec$hello(), "Hello World, Ooplah!")
  expect_equal(dec$goodbye(), "Bye")
  expect_equal(dec$oop, "oop")
})


test_that("skip on decorating existing methods - [[", {
  exists <- DecoratorClass("exists",
    public = list(hello = function() "Oh no", goodbye = function() "Bye")
  )
  dec <- exists$new(oopl, exists = "skip")
  expect_equal(private(dec)[[".exists"]], "skip")
  expect_equal(dec[["hello"]](), "Hello World, Ooplah!")
  expect_equal(dec[["goodbye"]](), "Bye")
  expect_equal(dec[["oop"]], "oop")
})

test_that("skip on decorating existing methods - switch to field", {
  exists <- DecoratorClass("exists", public  = list(hello = "Oh no"))
  expect_equal(exists$new(oopl, exists = "skip")$hello(),
               "Hello World, Ooplah!")
})

test_that("overwrite on decorating existing methods - $", {
  exists <- DecoratorClass("exists",
    public = list(hello = function() "Oh no", goodbye = function() "Bye")
  )
  dec <- exists$new(oopl, exists = "overwrite")
  expect_equal(private(dec)$.exists, "overwrite")
  expect_equal(dec$hello(), "Oh no")
  expect_equal(dec$goodbye(), "Bye")
  expect_equal(dec$oop, "oop")
})

test_that("overwrite on decorating existing methods - [[", {
  exists <- DecoratorClass("exists",
    public = list(hello = function() "Oh no", goodbye = function() "Bye")
  )
  dec <- exists$new(oopl, exists = "overwrite")
  expect_equal(private(dec)[[".exists"]], "overwrite")
  expect_equal(dec[["hello"]](), "Oh no")
  expect_equal(dec[["goodbye"]](), "Bye")
  expect_equal(dec[["oop"]], "oop")
})

test_that("can't clone a decorator", {
  oopl <- ooplah$new()

  expect_error(DecoratorClass("clone", cloneable = TRUE), "not cloneable")
  dec <- DecoratorClass("clone")$new(oopl)

  expect_equal(dec$clone, NULL)
  expect_error(dec$clone(), "attempt to apply")

  expect_equal(dec[["clone"]], NULL)
  expect_error(dec[["clone"]](), "attempt to apply")

  # original clone still works
  oopl2 <- oopl$clone(deep = TRUE)
  oopl$logically <- FALSE
  expect_true(oopl2$logically)
  expect_false(oopl$logically)
  expect_false(dec$logically)
})

test_that("can't decorate twice", {
  oop <- ooplah$new()

  dec1 <- DecoratorClass("dec1")
  dec2 <- DecoratorClass("dec2")

  oop_dec1 <- dec1$new(oop)

  expect_error(dec1$new(oop_dec1), "already decorated with")

  oop_dec2 <- dec2$new(oop_dec1)

  expect_error(dec1$new(oop_dec2), "already decorated with")
  expect_error(dec2$new(oop_dec2), "already decorated with")
})
