#' Creates an abstract R6 class by placing a thin wrapper around [R6::R6Class]
#' which causes an error to be thrown if the class is directly constructed
#' instead of one of its descendants.
#' @title Create an abstract R6 Class
#' @name AbstractClass
#' @details All arguments of [R6::R6Class] can be used as usual, see full
#' details at [R6::R6Class].
#' @export
NULL

DecoratorClass <- function() {

  init <- function(classname, self) {
    decorate <- function(object) {
      if (abstract) {
        .abstract(classname)(self)
      }

      parent.env(self) <- object
      ## Inherit from an abstract (in the truest sense of the word) 'Decorator'
      ##  class
      if ("Decorator" %nin% class(self)) {
        class(self) <- c(setdiff(class(self), "R6"), "Decorator", "R6")
      }

      invisible(self)
    }
  }

  args <- as.list(match.call()[-1])
  args$public$initialize <- init(classname, self)
  args$abstract <- NULL
  do.call(R6::R6Class, args)
}
formals(DecoratorClass) <- c(formals(R6::R6Class), alist(abstract = FALSE))

#' @export
`$.Decorator` <- function(x, name) {
  if (!identical(out <- get0(name, x, ifnotfound = NA), NA)) {
    out
  }
}

#' @export
`$<-.Decorator` <- function(x, i, j, ..., value) {
  if (exists(i, x, inherits = FALSE)) {
    NextMethod("$<-", object = x, name = i, value = value)
  } else {
    parent.env(x)[[i]] <- value
  }

  invisible(x)
}

#' @export
`[[.Decorator` <- function(x, i, ...) {
  if (!identical(out <- get0(i, x, ifnotfound = NA), NA)) {
    out
  }
}

#' @export
`[[<-.Decorator` <- function(x, i, j, ..., value) {
  if (exists(i, x, inherits = FALSE)) {
    NextMethod("[[<-", object = x, i = i, value = value)
  } else {
    parent.env(x)[[i]] <- value
  }

  invisible(x)
}
