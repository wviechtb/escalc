#' @export
convert.percentage.to.se <- function(p, n) {
  return(sqrt((p * (1-p)) / n));
}