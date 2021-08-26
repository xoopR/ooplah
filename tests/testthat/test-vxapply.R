test_that("vxapply", {
 expect_equal(vlapply(logical(10), identity), logical(10))
 expect_equal(vzapply(complex(10), identity), complex(10))
 expect_error(vlapply(complex(1), identity), "logical")

 objs <- list(ooplah$new(), ooplah$new())
 expect_equal(vcapply(objs, "oop"), rep("oop", 2))

 # Public method
 expect_equal(vcapply(objs, "exclaim", "ARGH"), rep("ARGH!", 2))
 expect_equal(vcapply(objs, "hello"), rep("Hello World, Ooplah!", 2))
 expect_type(vnapply(objs, "generate", 1), "double")
 expect_equal(length(vnapply(objs, "generate", 1)), 2)

 # Active binding
 expect_equal(vlapply(objs, "logically"), rep(TRUE, 2))
})

test_that("loapply", {
  expect_equal(loapply(c(1, 2, 3), identity), list(1, 2, 3))

 ## For R6 objects
 objs <- list(ooplah$new(), ooplah$new())
 # Public field
 expect_equal(loapply(objs, "oop"), list("oop", "oop"))
 # Public method
 expect_equal(vcapply(loapply(objs, "hello"), do.call, list()),
              rep("Hello World, Ooplah!", 2))
 expect_equal(loapply(objs, "hello", NULL),
              rep(list("Hello World, Ooplah!"), 2))
 expect_equal(loapply(objs, "exclaim", "Lah"),
              rep(list("Lah!"), 2))
})

test_that(".vxapply", {
  expect_equal(.vxapply(integer(1))(1L, identity), 1L)
})

test_that("vxapply work and error as expected", {
  expect_equal(vlapply(TRUE, identity), TRUE)
  expect_error(vlapply("a", identity))

  expect_equal(viapply(1L, identity), 1L)
  expect_error(viapply("a", identity))

  expect_equal(vnapply(pi, identity), pi)
  expect_error(vnapply("a", identity))

  expect_equal(vcapply("a", identity, USE.NAMES = FALSE), "a")
  expect_error(vcapply(2, identity))

  expect_equal(vzapply(1i, identity), 1i)
  expect_error(vzapply("a", identity))

  expect_equal(vrapply(raw(1), identity), raw(1))
  expect_error(vrapply("a", identity))
})
