private <- function(x) {
  stopifnot(is.R6(x))
  x$.__enclos_env__$private
}