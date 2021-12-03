str_collapse <- function(str) {
  sprintf("{%s}", paste0(str, collapse = ", "))
}

# With thanks to rlang
`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}