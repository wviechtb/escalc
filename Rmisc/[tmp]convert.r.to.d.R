#' @export
convert.r.to.d <- function(r) {
  return( (r*2) / sqrt(1 - r^2));
}