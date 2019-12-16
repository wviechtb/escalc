#' @export
convert.r.to.p <- function(r, n) {
  t <- convert.r.to.t(r, n);
  return(convert.t.to.p(t, n - 2));
}
