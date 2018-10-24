#' @export
convert.t.to.p <- function(t, df) {
  return(2*stats::pt(-abs(t),df));
}