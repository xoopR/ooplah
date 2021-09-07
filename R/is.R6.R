#' Assert/test if 'x' is a R6 object or class
#' @title Is 'x' a R6 object or class?
#' @param x Object to test
#' @return Either TRUE/FALSE is testing if `x` inherits from
#' `R6` or `R6ClassGenerator`, otherwise returns `x` invisibly on assertion
#' if TRUE or returns an error if FALSE
#' @export
is.R6 <- function(x) { # nolint
  inherits(x, c("R6", "R6ClassGenerator"))
}

#' Assert/test if 'x' is a R6 class
#' @title Is 'x' a R6 class?
#' @param x Object to test
#' @return Either TRUE/FALSE is testing if `x` inherits from
#' `R6ClassGenerator`, otherwise returns `x` invisibly on assertion if TRUE or
#' returns an error if FALSE
#' @export
is.R6Class <- function(x) { # nolint
  inherits(x, "R6ClassGenerator")
}

#' Assert/test if 'x' is a R6 object
#' @title Is 'x' a R6 object?
#' @param x Object to test
#' @return Either TRUE/FALSE is testing if `x` inherits from `R6`, otherwise
#' returns `x` invisibly on assertion if TRUE or returns an error if FALSE
#' @export
is.R6Object <- function(x) { # nolint
  inherits(x, "R6")
}

#' @rdname is.R6
#' @export
assert_R6 <- function(x) { # nolint
  if (is.R6(x)) {
    invisible(x)
  } else {
    stop(sprintf("'%s' is not an R6 class or object", deparse(substitute(x))))
  }
}

#' @rdname is.R6Object
#' @export
assert_R6Object <- function(x) { # nolint
  if (is.R6Object(x)) {
    invisible(x)
  } else {
    stop(sprintf("'%s' is not an R6 object", deparse(substitute(x))))
  }
}

#' @rdname is.R6Class
#' @export
assert_R6Class <- function(x) { # nolint
  if (is.R6Class(x)) {
    invisible(x)
  } else {
    stop(sprintf("'%s' is not an R6 class", deparse(substitute(x))))
  }
}
