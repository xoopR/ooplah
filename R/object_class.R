#' Find class of an object or an ancestor of the object. In contrast to `class`
#' which returns a class object and all its ancestors, this function returns
#' either the class of the object itself, or the class of one of its ancestors.
#'
#' `object_classes` is a stripped-down wrapper to get the class of multiple
#' objects
#' @title Get class of an object (possibly with inheritance)
#' @param object `ANY` \cr Object to get the class of
#' @param ancestor `(integer(1))` \cr If greater than 0 then the given ancestor
#' to get the class for, see examples
#' @export
#' @examples
#' library(R6)
#'
#' class_a <- R6Class("class_a")
#' class_b <- R6Class("class_b", inherit = class_a)
#' class(class_b$new())
#' object_class(class_b$new())
#' object_class(class_b$new(), 1)
object_class <- function(object, ancestor = 0) {
    class(object)[[ancestor + 1]]
}

#' @rdname object_class
#' @param ... `ANY` \cr Passed to [get]
#' @export
get_object_class <- function(object, ancestor = 0, ...) {
  get(object_class(object, ancestor), ...)
}

#' @rdname object_class
#' @param ... `ANY` \cr Objects to `vapply` over
#' @param objects `(list(1))` \cr Alternative constructor with `list` of objects
#' @export
object_classes <- function(..., objects = list(...)) {
  vcapply(objects, object_class)
}
