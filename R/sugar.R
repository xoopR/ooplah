#' @title Sugar function for decoration
#' @description Simple wrapper around `decorator$new(object, exists)`
#'
#' @param object `[R6::R6Class]` \cr R6 class to decorate.
#' @param decorators `([DecorateClass]|character())` \cr
#' One or more decorators (by name or class) to decorate with.
#' @param exists `(character(1)` \cr Expected behaviour if method exists in
#' `object` and `decorator`. One of: 1. `exists = "error"` (default) - This
#' will throw an error and prevent the object being decorated.
#' 2. `exists = "skip"` - This will decorate the object with all fields/methods
#' that don't already exist. 3. `exists = "overwrite"` - This will decorate
#' the object with all fields/methods from the decorator and overwrite ones
#' with the same name if they already exist.
#' @param ... `ANY` \cr Additional arguments passed to [get].
#'
#' @seealso
#' [DecoratorClass]
#'
#' @examples
#' library(R6)
#'
#' ## Define decorators
#' dec1 <- DecoratorClass("dec1", public = list(goodbye = "Goodbye World"))
#' dec2 <- DecoratorClass("dec2", public = list(goodbye2 = "Goodbye World 2"))
#'
#' oop <- ooplah$new()
#' oop$goodbye
#' dec_oop <- decorate(oop, c(dec1, dec2))
#' dec_oop$goodbye
#' dec_oop$goodbye2
#'
#' ## Equivalently
#' oop <- ooplah$new()
#' decorate(oop, c("dec1", "dec2"))
#'
#' @export
decorate <- function(object, decorators,
                    exists = c("skip", "error", "overwrite"), ...) {
  for (decorator in decorators) {
    if (is.character(decorator)) {
      object <- get(decorator, ...)$new(object, exists) # nocov
    } else {
      object <- decorator$new(object, exists)
    }
  }

  object
}