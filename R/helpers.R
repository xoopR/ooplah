str_collapse <- function(str) {
  sprintf("{%s}", paste0(str, collapse = ", "))
}
