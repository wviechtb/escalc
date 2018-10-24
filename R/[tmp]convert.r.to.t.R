#' @export
convert.r.to.t <- function(r, n) {
  return(r * sqrt((n - 2) / (1-r^2)));
}