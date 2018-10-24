#' @export
convert.f.to.p <- function(f, df1, df2, lower.tail=FALSE) {
  return(2*stats::pf(f, df1, df2, lower.tail=lower.tail));
}