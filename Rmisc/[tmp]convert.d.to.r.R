#' @export
convert.d.to.r <- function(d, n1 = NULL, n2 = NULL, akfEq8='if (n1 + n2) < 50') {
  if (!is.logical(akfEq8) && !is.null(n1) && !is.null(n2) && ((n1 + n2) < 50)) {
    akfEq8 <- TRUE;
  }
  if (is.logical(akfEq8) && akfEq8) {
    if (is.null(n1) || is.null(n2)) stop("Cannot use the equation by Aaron, Kromrey & Ferron (1998) if you do not specify both sample sizes!");
    N <- n2 + n2;
    return(sqrt( d^2 / ( d^2 + ((N^2 - 2 * N) / (n1 * n2)) ) ));
  } else {
    if (is.null(n1) || is.null(n2)) {
      a <- 4;
    } else {
      a <- (n1 + n2) ^ 2 / (n1 * n2)
    }
    return(d / sqrt(d^2 + a));
  }
}