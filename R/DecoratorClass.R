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
      class(self) <- c(class(self)[1], "Decorator",
                       class(self)[2:length(class(self))])
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
  if (!is.na(out <- get0(name, x, ifnotfound = NA))) {
    out
  }
}

#' @export
`$<-.Decorator` <- function(x, name, value, ...) {
  if (exists(name, x, inherits = FALSE)) {
    NextMethod("$", object = x, name = name, value = value)
  } else {
    parent.env(x)[[name]] <- value
  }

  invisible(x)
}

#' @export
`[[.Decorator` <- function(x, i, ...) {
  if (!is.na(out <- get0(i, x, ifnotfound = NA))) {
    out
  }
}

#' @export
`[[<-.Decorator` <- function(x, i, value, ...) {
  if (exists(i, x, inherits = FALSE)) {
    NextMethod("[[", object = x, i = i, value = value)
  } else {
    parent.env(x)[[i]] <- value
  }

  invisible(x)
}
