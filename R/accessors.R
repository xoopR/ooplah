#' Access the private environment of an R6 object
#' @title Get R6 object private environment
#' @param x (`R6`) \cr R6 object to get environment from, errors if not R6
#' @export
private <- function(x) {
  stopifnot(is.R6Object(x))
  x$.__enclos_env__$private
}

#' Access the parent environment of an R6 object
#' @title Get R6 object parent environment
#' @param x (`R6`) \cr R6 object to get environment from, errors if not R6
#' @export
super <- function(x) {
  stopifnot(is.R6Object(x))
  x$.__enclos_env__$super
}
