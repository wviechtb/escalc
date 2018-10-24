#' @export
convert.omegasq.to.cohensfsq <- function(omegasq) {
  return(omegasq / (1 - omegasq));
}