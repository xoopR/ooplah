.vxapply <- function(type) {
  function(X, FUN, ..., USE.NAMES = TRUE) { # nolint
    if (is.function(FUN)) {
      vapply(X, FUN, type, ..., USE.NAMES = USE.NAMES)
    } else {
      vapply(X, function(.x)  {
        .f <- .x[[FUN]]
        if (is.function(.f)) {
          .f(...)
        } else {
          .f
        }
      }, type, USE.NAMES = USE.NAMES)
    }
  }
}

#' Specialised `vapply` functions for scalars of each of the six atomic classes
#' in R:
#'
#' * logical (`vlapply`)
#' * integer (`viapply`)
#' * numeric/real (`vnapply`)
#' * character/string (`vcapply`)
#' * complex (`vzapply`)
#' * raw (`vrapply`)
#'
#' These are simply wrappers around [vapply] where `FUN.VALUE` is pre-filled
#' with a scalar of the given class.
#'
#' In addition these can be applied to pull-out fields or methods from R6 or
#' other OOP objects by supplying the field/method name to `FUN`. See examples.
#' @title Specialised vapply methods for atomic classes
#' @name vxapply
#' @param X,...,USE.NAMES See [vapply]
#' @param FUN Either a function to apply to each element of `X`, as in [vapply]
#' or the field/method name of an OOP object (see examples)
#' @examples
#' ## Specialised vapply
#' vlapply(logical(10), identity)
#' vzapply(complex(10), identity)
#'
#' ## For R6 objects
#' objs <- list(ooplah$new(), ooplah$new())
#'
#' # Public field
#' vcapply(objs, "oop")
#'
#' # Public method
#' vcapply(objs, "exclaim", "ARGH")
#' vcapply(objs, "hello")
#' vnapply(objs, "generate", 1)
#'
#' # Active binding
#' vlapply(objs, "logically")
NULL

#' @rdname vxapply
#' @export
vlapply <- .vxapply(logical(1))
#' @rdname vxapply
#' @export
viapply <- .vxapply(integer(1))
#' @rdname vxapply
#' @export
vnapply <- .vxapply(numeric(1))
#' @rdname vxapply
#' @export
vcapply <- .vxapply(character(1))
#' @rdname vxapply
#' @export
vzapply <- .vxapply(complex(1))
#' @rdname vxapply
#' @export
vrapply <- .vxapply(raw(1))


#' Specialised `lapply` functions for R6 or other OOP classes.
#' This is simply a wrapper that detects if `FUN` is a function, in which
#' case `lapply` is used as usual, or a string, in which case the given
#' field/method is returned as a list.
#' @title Specialised lapply for objects
#' @param X,... See [lapply]
#' @param FUN Either a function to apply to each element of `X`, as in [lapply]
#' or the field/method name of an OOP object (see examples)
#' @export
#' @examples
#' ## lapply as usual
#' loapply(c(1, 2, 3), identity)
#'
#' ## For R6 objects
#' objs <- list(ooplah$new(), ooplah$new())
#' # Public field
#' loapply(objs, "oop")
#' # Public method
#' loapply(objs, "hello")
loapply <- function(X, FUN, ...) {
  if (is.function(FUN)) {
    lapply(X, FUN, ...)
  } else {
    lapply(X, function(.x) {
      .f <- .x[[FUN]]
      if (is.function(.f) && ...length()) {
        if (is.null(formals(.f))) {
          .f()
        } else {
          .f(...)
        }
      } else {
        .f
      }
    })
  }
}
