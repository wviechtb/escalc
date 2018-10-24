#' @export
convert.or.to.d <- function(or) {
  return(log(or) * (sqrt(3) / pi));
}