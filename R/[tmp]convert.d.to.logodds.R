#' @export
convert.d.to.logodds <- function(d) {
  if (!is.numeric(d) || (length(d) > 1)) {
    stop("The 'd' argument is not a single numeric value!");
  }
  return(d * (pi / sqrt(3)));
}