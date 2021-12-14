#' Creates an abstract R6 class by placing a thin wrapper around [R6::R6Class]
#' which causes an error to be thrown if the class is directly constructed
#' instead of one of its descendants.
#' @title Create an abstract R6 Class
#' @name AbstractClass
#' @details An abstract class is a class that cannot be constructed directly.
#' Instead they are used to define common fields/methods for child classes
#' that inherit from them.
#'
#' All arguments of [R6::R6Class] can be used as usual, see full
#' details at [R6::R6Class].
#' @examples
#' library(R6)
#'
#' ab <- AbstractClass("abstract", public = list(hello = "Hello World"))
#' \dontrun{
#' # errors
#' ab$new()
#' }
#' child <- R6Class("child", inherit = ab)
#' child$new()$hello
#'
#' @references
#' Gamma, E., Helm, R., Johnson, R., & Vlissides, J. (1996).
#' Design Patterns: Elements of Reusable Software.
#' Addison-Wesley Professional Computing Series (p. 395).
#'
#' @export
NULL

AbstractClass <- function() {

  args <- as.list(match.call()[-1])
  public <- args$public
  args$private$ooplah <- new.env()
  if (is.null(init <- public$initialize)) {
    public$initialize <- function() {
      private$ooplah$fabstract(self)
    }
  } else {
    args$private$ooplah$init <- init
    public$initialize <- function() {
      private$ooplah$fabstract(self)
      do.call(eval(private$ooplah$init), as.list(match.call()[-1]))
    }
    formals(public$initialize) <- formals(eval(init))
  }

  args$public <- public
  args$parent_env <- args$parent_env %||% parent.frame()
  args$private$ooplah$abstract <- classname
  args$private$ooplah$fabstract <- function(obj) {
    if (identical(classname <- object_class(obj),
                  private(obj)$ooplah$abstract)) {
      stop(sprintf(
          "'%s' is an abstract class that can't be initialized.",
          classname
        ),
        call. = FALSE
      )
    }
  }

  do.call(R6::R6Class, args)
}
formals(AbstractClass) <- formals(R6::R6Class)
