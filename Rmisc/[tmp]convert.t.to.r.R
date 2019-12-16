#' @export
convert.t.to.r <- function(t, n) {
  return(t / (sqrt(n-2+t^2)));
}