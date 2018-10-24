#' @export
convert.chisq.to.p <- function(chisq, df, lower.tail=FALSE) {
  return(2*stats::pchisq(chisq, df, lower.tail=lower.tail));
}