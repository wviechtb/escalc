#' @export
convert.ncf.to.omegasq <- function(ncf, N) {
  return(ncf / (ncf + N));
}