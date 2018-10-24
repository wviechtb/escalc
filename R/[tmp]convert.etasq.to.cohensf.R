#' @export
convert.etasq.to.cohensf <- function(etasq) {
  return(sqrt(etasq / (1-etasq)));
}