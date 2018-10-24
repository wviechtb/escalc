#' @export
convert.fisherz.to.r <- function(z) {
  return((exp(2 * z) - 1) / (exp(2*z)+1));
}