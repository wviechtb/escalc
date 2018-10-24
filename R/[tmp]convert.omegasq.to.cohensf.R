#' @export
convert.omegasq.to.cohensf <- function(omegasq) {
  return(sqrt(omegasq / (1 - omegasq)));
}