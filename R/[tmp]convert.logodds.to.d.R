#' @export
convert.logodds.to.d <- function(logodds) {
  return(logodds * (sqrt(3) / pi));
}