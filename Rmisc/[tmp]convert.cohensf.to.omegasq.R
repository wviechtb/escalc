#' @export
convert.cohensf.to.omegasq <- function(cohensf) {
  return(cohensf^2 / (1 + cohensf^2));
}