objah <- ooplah$new()

test_that("object_class", {
  expect_equal(object_class(objah), "ooplah")
  expect_equal(object_class(objah, 1), "OoplahParent")
  expect_equal(object_class(objah, 2), "R6")
})

test_that("get_object_class", {
  expect_equal(get_object_class(objah), ooplah)
  expect_equal(get_object_class(objah, 1)$classname, OoplahParent$classname)
})

test_that("object_classes", {
  expect_equal(object_classes(objah, objah), rep("ooplah", 2))
  expect_equal(object_classes(objects = list(objah, objah)), rep("ooplah", 2))
})
