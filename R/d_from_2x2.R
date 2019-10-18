#' Obtain Cohen's *d* from 2x2 Table Data (Probit Transformation)
#'
#' This function computes Cohen's *d* values based on 2x2 table data using the \sQuote{probit transformation}.
#'
#' @param a Numerical vector for the number of individuals in group 1 with outcome 1.
#' @param b Numerical vector for the number of individuals in group 1 with outcome 2.
#' @param c Numerical vector for the number of individuals in group 2 with outcome 1.
#' @param d Numerical vector for the number of individuals in group 2 with outcome 2.
#' @param biasCorrect Logical to indicate if the *d*-values should be bias-corrected. Can also be a vector.
#'
#' @return A numeric vector of Cohen's `d` values.
#'
#' @references Rosenthal, R. (1994). Parametric measures of effect size. In H. Cooper & L. V. Hedges (Eds.), The handbook of research synthesis (1st ed., pp. 231-244). New York: Russell Sage Foundation.
#' @references Sanchez-Meca, J., Marin-Martinez, F., & Chacon-Moscoso, S. (2003). Effect-size indices for dichotomized outcomes in meta-analysis. Psychological Methods, 8(4), 448-467.
#'
#' @examples
#' d_from_2x2(a=c(5,12), b=c(35,70), c=c(4,7), d=c(36,70))
#'
#' @import stats
#'
#' @export

d_from_2x2 <- function(a, b, c, d, biasCorrect = TRUE) {

  if (length(biasCorrect) == 1)
    biasCorrect <- rep(biasCorrect, length(a))

  if (any(length(a) != c(length(b), length(c), length(d), length(biasCorrect))))
    stop("Length of 'a' argument does not match length of the other arguments.")

  n1 <- a + b
  n2 <- c + d
  m <- n1 + n2 - 2

  p1 <- a/n1
  p2 <- c/n2

  z1 <- qnorm(p1)
  z2 <- qnorm(p2)

  d <- z1 - z2
  v <- 2*pi*p1*(1-p1)*exp(z1^2)/n1 + 2*pi*p2*(1-p2)*exp(z2^2)/n2

  cm <- ifelse(biasCorrect, .cmicalc(m), 1)
  d <- cm * d
  v <- cm^2 * v # FIXME: not sure if we should do this

  return(stats::setNames(data.frame(d, v),
                         c(.EFFECTSIZE_POINTESTIMATE_NAME_IN_DF,
                           .EFFECTSIZE_VARIANCE_NAME_IN_DF)))
}
