#' @export
as.character.R6 <- function(x, ...) {
  sprintf("<%s>", object_class(x))
}
