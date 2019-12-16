#' @export
convert.r.to.fisherz <- function(r) {
  return(.5 * log((1+r) / (1-r)));
}