#' R6 Class for testing and examples
#' @title R6 Class for testing and examples
#' @name ooplah
#' @export
NULL

ooplah <- R6Class("ooplah",
  public = list(
    hello = function() "Hello World, Ooplah!",
    oop = "oop",
    exclaim = function(x) sprintf("%s!", x),
    generate = function(n = 1) runif(n)
  ),
  active = list(
    logically = function(x) {
      if (missing(x)) {
        private$.lgl
      } else {
        stopifnot(is.logical(x))
        private$.lgl <- x
      }
    }
  ),
  private = list(
    .goodbye = function() cat("Goodbye World"),
    .lgl = TRUE
  )
)