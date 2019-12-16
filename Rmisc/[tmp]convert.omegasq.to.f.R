#' @export
convert.omegasq.to.f <- function(omegasq, df1, df2) {
  return( (omegasq * ((df2 + 1) / df1) + 1) / (1 - omegasq) );
}