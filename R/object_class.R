#' Find class of an object or an ancestor of the object. In contrast to `class`
#' which returns a class object and all its ancestors, this function returns
#' either the class of the object itself, or the class of one of its ancestors.
#' @title Get class of an object (possibly with inheritance)
#' @param object `ANY` \cr Object to get the class of
#' @param ancestor `(integer(1))` \cr If greater than 0 then the given ancestor
#' to get the class for, see examples
#' @export
#' @examples
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
