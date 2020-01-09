#' Obtain Cohen's *d* from p-value of an independent samples t-test
#'
#' This function converts p-values from independent samples t-tests to Cohen's *d* values.
#'
#' @param p Numerical vector with the (one- or two-sided) p-values.
#' @param n1,n2 Numerical vectors with the sample sizes of the two groups.
#' @param side Numerical vector to indicate the sidedness of the p-values. (\code{1} for a one-sided and \code{2} for a two-sided p-value)
#' @param sign Numerical vector to indicate the sign of the d-values (\code{+1} or \code{-1}; only relevant for two-sided p-values; for one-sided p-values, this can be set to \code{NA}).
#' @param assumeHomoscedacity Logical to indicate if the p-values come from independent samples t-tests that have assumed homoscedacity (equal variances in the two groups) or not. Can also be a vector.
#' @param biasCorrect Logical to indicate if the *d*-values should be bias-corrected. Can also be a vector.
#'
#' @return A numeric vector of Cohen's `d` values.
#'
#' @examples
#' d_from_p_t_in(p = .038, n1=22, n2=25, side=2, sign=-1)
#'
#' @export

d_from_p_t_in <- function(p, n1, n2, side, sign,
                          assumeHomoscedacity = TRUE, biasCorrect = TRUE) {

  if (length(assumeHomoscedacity) == 1)
    assumeHomoscedacity <- rep(assumeHomoscedacity, length(p))

  if (length(biasCorrect) == 1)
    biasCorrect <- rep(biasCorrect, length(p))

  if (any(length(p) != c(length(n1), length(n2), length(side), length(sign), length(assumeHomoscedacity), length(biasCorrect))))
    stop("Length of 'p' argument does not match length of the other arguments.")

  if (any(!assumeHomoscedacity)) {
    stop(.functionalityNotImplementedMsg(conversion = "d from an independent t-test with Welch's t",
                                         reason = "nonexistent",
                                         callingFunction = .curfnfinder()))
  }

  if (any(!(side %in% c(1,2))))
    stop("Argument 'side' must be either 1 or 2.")

  if (any(!(sign %in% c(-1,+1,NA))))
    stop("Argument 'sign' must be either -1, +1, or NA.")

  if (any(side == 2 & is.na(sign)))
    stop("Value of 'sign' must be specified for all two-sided p-values.")

  sign[side==1] <- 1
  sign[is.na(sign)] <- 1

  m <- n1 + n2 - 2

  d <- sign * stats::qt(p/side, df=m, lower.tail=FALSE) * sqrt(1/n1 + 1/n2)

  d <- ifelse(biasCorrect, .cmicalc(m), 1) * d

  dVar <- 1/n1 + 1/n2 + d^2 / (2*(n1+n2))

  minimalMissingMessage <-
    minimalMissingMessage(d, dVar)
  
  return(stats::setNames(data.frame(d, dVar, minimalMissingMessage),
                         c(opts$get("EFFECTSIZE_POINTESTIMATE_NAME_IN_DF"),
                           opts$get("EFFECTSIZE_VARIANCE_NAME_IN_DF"),
                           opts$get("EFFECTSIZE_MISSING_MESSAGE_NAME_IN_DF"))))
  
}
