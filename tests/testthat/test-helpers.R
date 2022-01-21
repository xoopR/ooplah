test_that("%||% works", {
  expect_equal(NULL %||% "a", "a")
  expect_equal("b" %||% NULL, "b")
  expect_equal("c" %||% "d", "c")
})
