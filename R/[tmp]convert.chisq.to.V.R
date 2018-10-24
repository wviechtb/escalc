#' @export
convert.chisq.to.V <- function(chisq, n, minDim) {
  if (!is.numeric(chisq) || (length(chisq) > 1)) {
    stop("The 'chisq' argument is not a single numeric value!");
  }
  if (!is.numeric(n) || (length(n) > 1)) {
    stop("The 'n' argument is not a single numeric value!");
  }
  if (!is.numeric(minDim) || (length(minDim) > 1)) {
    stop("The 'minDim' argument is not a single numeric value!");
  }
  res <- as.numeric(sqrt(chisq/(n*(minDim - 1))));
  return(ifelse(is.finite(res), res, NA));
}