#' @export
convert.logodds.to.r <- function(logodds) {
  return(convert.d.to.r(convert.logodds.to.d(logodds)));
}