#' @export
convert.or.to.r <- function(or) {
  return(convert.d.to.r(convert.logodds.to.d(log(or))));
}