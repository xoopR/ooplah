#' R6 Class for testing and examples
#' @title R6 Class for testing and examples
#' @name ooplah
#' @export
NULL

OoplahParent <- AbstractClass("OoplahParent",
  public = list(
    initialize = function(x) {
      private$.init <- TRUE
      invisible(self)
    }
  ),
  active = list(
    init = function() private$.init
  ),
  private = list(
    .init = FALSE
  )
)

ooplah <- R6Class("ooplah",
  inherit = OoplahParent,
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
    .goodbye = "Goodbye World",
    .lgl = TRUE
  )
)
