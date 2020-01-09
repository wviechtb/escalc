#' Obtain Cohen's *d* from an Odds Ratio and its Confidence Interval
#'
#' This function computes Cohen's *d* values based on an odds ratio using either the \sQuote{normal} or the \sQuote{logistic} transformation.
#'
#' @param or Numerical vector for the odds ratios.
#' @param lb Numerical vector for the lower bound of the confidence intervals.
#' @param ub Numerical vector for the upper bound of the confidence intervals.
#' @param ci Scalar between 0 and 100 to indicate the confidence interval width (default is 95). Can also be a vector.
#' @param dist Character string to indicate the distribution to assume for the underlying data. Either \code{"normal"} (the default) or \code{"logistic"}. Can also be a vector.
#'
#' @return A numeric vector of Cohen's `d` values.
#'
#' @references Chinn, S. (2000). A simple method for converting an odds ratio to effect size for use in meta-analysis. Statistics in Medicine, 19(22), 3127-3131.
#' @references Cox, D. R., & Snell, E. J. (1989). Analysis of binary data (2nd ed.). London: Chapman & Hall.
#'
#' @examples
#' d_from_or(or=c(1.29, 1.71), lb=c(0.32, 0.64), ub=c(5.19, 4.61))
#' d_from_or(or=c(1.29, 1.71), lb=c(0.32, 0.64), ub=c(5.19, 4.61), dist="logistic")
#'
#' @import stats
#'
#' @export

d_from_or <- function(or, lb, ub, ci=95, dist="normal") {

  if (any(ci < 0 | ci > 100))
    stop("Argument 'ci' must be between 0 and 100.")

  dist <- c("normal", "logistic")[charmatch(dist, c("normal", "logistic"))]

  if (any(!(dist %in% c("normal", "logistic"))))
    stop("Argument 'dist' must be either 'normal' or 'logistic'.")

  if (length(ci) == 1)
    ci <- rep(ci, length(or))

  if (length(dist) == 1)
    dist <- rep(dist, length(or))

  if (any(length(or) != c(length(lb), length(ub), length(ci), length(dist))))
    stop("Length of 'or' argument does not match length of the other arguments.")

  lor <- log(or)
  llb <- log(lb)
  lub <- log(ub)

  crit <- qnorm((100-ci)/(2*100), lower.tail=FALSE)
  selor <- (lub - llb) / (2*crit)

  d <- ifelse(dist == "logistic", lor * sqrt(3) / pi, lor * .607)
  v <- ifelse(dist == "logistic", selor^2 * 3 / pi^2, selor^2 * .607^2)

  minimalMissingMessage <-
    minimalMissingMessage(d, v)
  
  return(stats::setNames(data.frame(d, v, minimalMissingMessage),
                         c(opts$get("EFFECTSIZE_POINTESTIMATE_NAME_IN_DF"),
                           opts$get("EFFECTSIZE_VARIANCE_NAME_IN_DF"),
                           opts$get("EFFECTSIZE_MISSING_MESSAGE_NAME_IN_DF"))))

}
