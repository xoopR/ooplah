#' Creates an abstract R6 class by placing a thin wrapper around [R6::R6Class]
#' which causes an error to be thrown if the class is directly constructed
#' instead of one of its descendants.
#' @title Create an abstract R6 Class
#' @name AbstractClass
#' @details All arguments of [R6::R6Class] can be used as usual, see full
#' details at [R6::R6Class].
#' @export
NULL

AbstractClass <- function() {
  abstract <- function(self) {
    if (object_class(self) == classname) {
      stop(sprintf(
        "'%s' is an abstract class that can't be initialized.", classname),
        call. = FALSE
      )
    }
  }

  args <- as.list(match.call()[-1])
  public <- args$public

  if (length(public)) {
    if (is.null(init <- public$initialize)) {
      public$initialize <- function() abstract(self)
    } else {
      public$initialize <- function(...) {
        abstract(self)
        eval(init)(...)
      }
    }
  } else {
    public <- list(initialize = function() abstract(self))
  }


  args$public <- public
  do.call(R6::R6Class, args)
}
formals(AbstractClass) <- formals(R6::R6Class)