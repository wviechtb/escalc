#' @export
convert.cohensfsq.to.omegasq <- function(cohensfsq) {
  return(cohensfsq / (1 + cohensfsq));
}