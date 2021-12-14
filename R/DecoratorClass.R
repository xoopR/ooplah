#' Creates a decorator R6 class by placing a thin wrapper around [R6::R6Class]
#' which allows the constructed class to inherit the fields and methods of
#' the given object.
#' @title Create an abstract R6 Class
#' @name DecoratorClass
#' @details
#' The decorator design pattern allows methods to be added to an
#' object without bloating the interface with too many methods on construction
#' and without causing large inheritance trees. A decorator class contains
#' fields/methods that are 'added' to the given object in construction, this
#' is made clearer in examples.
#'
#' There are three possibilities when trying to decorate an object with a
#' field/method that already exists:
#'
#' 1. `exists = "skip"` (default) - This will decorate the object with all
#' fields/methods that don't already exist
#' 2. `exists = "error"` - This will throw an error and prevent the
#' object being decorated
#' 3. `exists = "overwrite"` - This will decorate the object with all
#' fields/methods from the decorator and overwrite ones with the same name
#' if they already exist
#'
#' Decorators are currently not cloneable.
#'
#' All arguments of [R6::R6Class] can be used as usual, see full
#' details at [R6::R6Class].
#'
#' @references
#' Gamma, E., Helm, R., Johnson, R., & Vlissides, J. (1996).
#' Design Patterns: Elements of Reusable Software.
#' Addison-Wesley Professional Computing Series (p. 395).
#'
#' @seealso
#' [decorate]
#'
#' @export
#'
#' @examples
#' library(R6)
#'
#' ## Create two decorators
#' # Works with active bindings...
#' dec1 <- DecoratorClass("dec1", active = list(hi = function() "Hi World"))
#' # And public fields...
#' dec2 <- DecoratorClass("dec2", public = list(goodbye = "Goodbye World"))
#'
#' ## Create an object to decorate
#' oop <- ooplah$new()
#' oop$hello()
#'
#'
#' ## Decorate with dec1 by constructing dec1 with object oop:
#' dec_oop <- dec1$new(oop) # equiv `decorate(oop, dec1)`
#' ## We have all original methods from oop
#' dec_oop$hello()
#' # It's inherited methods
#' dec_oop$init
#' # And now decorated methods
#' dec_oop$hi
#'
#' ## We can decorate again
#' redec_oop <- dec2$new(dec_oop)
#' redec_oop$hello()
#' redec_oop$init
#' redec_oop$hi
#' # And now
#' redec_oop$goodbye
#'
#' # Notice the class reflects all decorators, the original object and parents,
#' #  and adds the 'Decorator' class
#' class(redec_oop)
#'
#' ## Decorators also work with inheritance
#' parent_dec <- DecoratorClass("parent_dec",
#'   public = list(hi = function() "Hi!"))
#' child_dec <- DecoratorClass("child_dec", inherit = parent_dec)
#' dec_oop <- child_dec$new(ooplah$new())
#' dec_oop$hi()
#'
#' ## Three possibilities if the method/field name already exists:
#' oop <- ooplah$new()
#' exists_dec <- DecoratorClass("exists_dec",
#'   public = list(hello = function() "Hi!"))
#'
#' # 1. skip (default)
#' oop$hello()
#' exists_dec$new(oop, exists = "skip")$hello()
#'
#' # 2. error
#' \dontrun{
#' exists_dec$new(oop)
#' exists_dec$new(oop, exists = "error")
#' }
#'
#' # 3. overwrite
#' oop$hello()
#' exists_dec$new(oop, exists = "overwrite")$hello()
#'
#' ## Cloning
#' # Note that by default the decorated object is not cloned
#' dec <- DecoratorClass("dec", active = list(hi = function() "Hi World"))
#' dec_oop <- dec$new(oop)
#' dec_oop$logically
#' oop$logically <- FALSE
#' dec_oop$logically
#'
NULL

DecoratorClass <- function() {

  init <- function(self) {
    function(object, exists = c("skip", "error", "overwrite")) {
      private$ooplah$fabstract(self)

      exists <- match.arg(exists)

      self_mf <- setdiff(ls(self), c("clone", "initialize"))
      which <- self_mf %in% setdiff(ls(object), c("clone", "initialize"))
      if (any(which)) {
        if (exists == "error") {
          stop(sprintf("Fields/methods already exist in %s: %s",
                      as.character(object), str_collapse(self_mf[which])))
        } else {
          private$ooplah$.exists <- exists
        }
      }

      parent.env(self) <- object

      private$ooplah$decorators <- c(private(object)$ooplah$decorators,
                                    object_class(self))

      if (object_class(self) %in% class(object)) {
        stop(sprintf("%s is already decorated with %s", as.character(object),
                    as.character(self)))
      }

      ## Inherit from an abstract (in the truest sense of the word) 'Decorator'
      ##  class
      class(self) <- c(setdiff(c(setdiff(class(self), "R6"), class(object)),
                              c("Decorator", "R6")),
                      c("Decorator", "R6"))

      for (nm in names(private(object))) {
        if (nm != "ooplah") {
          private[[nm]] <- private(object)[[nm]]
        }
      }

      invisible(self)
    }
  }

  args <- as.list(match.call()[-1])
  args$private$ooplah <- new.env()
  args$private$ooplah$.exists <- "error"
  args$public$initialize <- init(self)
  args$abstract <- NULL
  if (!is.null(args$cloneable) && args$cloneable) {
    stop("Decorators are currently not cloneable.")
  }
  args$parent_env <- args$parent_env %||% parent.frame()
  args$cloneable <- FALSE
  args$lock_objects <- FALSE
  args$lock_class <- FALSE
  if (abstract) {
    args$private$ooplah$abstract <- classname
  } else {
    args$private$ooplah$abstract <- NULL
  }
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
formals(DecoratorClass) <- c(formals(R6::R6Class), alist(abstract = FALSE))

#' @export
`$.Decorator` <- function(x, name) { # nolint

  if (name == ".__enclos_env__") {
    return(NextMethod("$"))
  } else if (name == "clone") {
    return(NULL)
  }

  ## if skipping then search in parent environment first
  if (private(x)$ooplah$.exists == "skip") {
    if (!identical(out <- get0(name, parent.env(x), ifnotfound = NA), NA)) {
      out
      ## then check main environment
    } else if (!identical(out <- get0(name, x, ifnotfound = NA), NA)) {
      out
    }
  } else {
    ## if overwriting then get0 prioritises main
    if (!identical(out <- get0(name, x, ifnotfound = NA), NA)) {
      out
    }
  }
}

#' @export
`$<-.Decorator` <- function(x, i, j, ..., value) { # nolint

  if (exists(i, x, inherits = FALSE)) {
    NextMethod("$<-")
  } else {
    parent.env(x)[[i]] <- value
  }

  invisible(x)
}

#' @export
`[[.Decorator` <- function(x, i, ...) { # nolint

  if (i == "clone") {
    return(NULL)
  }

  ## if skipping then search in parent environment first
  if (private(x)[["ooplah"]][[".exists"]] == "skip") {
    if (!identical(out <- get0(i, parent.env(x), ifnotfound = NA), NA)) {
      out
      ## then check main environment
    } else if (!identical(out <- get0(i, x, ifnotfound = NA), NA)) {
      out
    }
  } else {
    ## if overwriting then get0 prioritises main
    if (!identical(out <- get0(i, x, ifnotfound = NA), NA)) {
      out
    }
  }
}

#' @export
`[[<-.Decorator` <- function(x, i, j, ..., value) { # nolint
  if (exists(i, x, inherits = FALSE)) {
    NextMethod("[[<-")
  } else {
    parent.env(x)[[i]] <- value
  }

  invisible(x)
}
