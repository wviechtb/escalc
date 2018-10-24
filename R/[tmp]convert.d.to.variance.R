#' @export
convert.d.to.variance <- function(d, n1, n2) {
  return( (((n1+n2) / (n1*n2)) + ((d^2) / (2*(n1+n2-2)))) * ((n1+n2) / (n1+n2-2)) );
}