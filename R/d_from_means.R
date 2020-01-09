#' Obtain Cohen's *d* from means, standard deviations, and ns
#'
#' This function converts means, standard deviations, and sample sizes
#' to Cohen's *d*.
#'
#' The formula that is used is the following (see e.g. Lakens, 2013):
#'
#' \deqn{d= \frac{\bar{X}_1 - \bar{X}_1}
#' {\sqrt{\frac{(n_1 - 1)SD_1^2 + (n_2 - 1)SD_2^2}{n_1 + n_2 - 2}}}}
#'
#' @param m1,m2 A numerical vector with the means of the two groups formed
#' by the dichotomous variable.
#' @param sd1,sd2 A numerical vector with the standard deviations of the two
#' groups formed by the dichotomous variable. Note that the *n*th element of
#' these vectors must correspond to the *n*th elements of the `m1`, `m2`
#' vectors.
#' @param n1,n2 A numerical vector with the sample sizes of the two groups
#' formed by the dichotomous variable. Note that the *n*th element of these
#' vectors must correspond to the *n*th elements of the `m1`, `m2` vectors.
#' @param bias_correct Logical to indicate if the *d*-values should be
#' bias-corrected. Can also be a vector.
#'
#' @return A data frame with in the first column, Cohen's `d` values, and
#' in the second column, the corresponding variances.
#'
#' @references Lakens, D. (2013) Calculating and reporting effect sizes to
#' facilitate cumulative science: a practical primer for t-tests and ANOVAs.
#' *Frontiers in Psychology, 4*, p. 863. \doi{10.3389/fpsyg.2013.00863}
#'
#' @examples
#' escalc::d_from_means(m1 = 2.828427,
#'                      m2 = 2.123041,
#'                      sd1 = 0.230101,
#'                      sd2 = 0.259281,
#'                      n1 = 126,
#'                      n2 = 89);
#'
#' @export
d_from_means <- function(m1,
                        m2,
                        sd1,
                        sd2,
                        n1,
                        n2,
                        bias_correct = FALSE) {

  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------
  ###
  ### Argument checking
  ###
  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------

  ###------------------------------------------------- m1, m2, sd1, sd2, n1, n2
  ### Argument-checking - Check presence
  ###------------------------------------------------- m1, m2, sd1, sd2, n1, n2
  if (missing(m1)) {
    stop(.errmsg(missing='m1',
                 callingFunction = .curfnfinder()))
  }
  if (missing(m2)) {
    stop(.errmsg(missing='m2',
                 callingFunction = .curfnfinder()))
  }
  if (missing(sd1)) {
    stop(.errmsg(missing='sd1',
                 callingFunction = .curfnfinder()))
  }
  if (missing(sd2)) {
    stop(.errmsg(missing='sd2',
                 callingFunction = .curfnfinder()))
  }
  if (missing(n1)) {
    stop(.errmsg(missing='n1',
                 callingFunction = .curfnfinder()))
  }
  if (missing(n2)) {
    stop(.errmsg(missing='n2',
                 callingFunction = .curfnfinder()))
  }

  ###--------------------------------------------------------------- t, n1 & n2
  ### Argument checking: lengths
  ###--------------------------------------------------------------- t, n1 & n2

  #if (!missing(n1) && !missing(n2)) {
    argLengths <- c(length(m1), length(m2),
                    length(sd1), length(sd2),
                    length(n1), length(n2));
    if (length(unique(argLengths)) > 1) {
      stop(.errmsg(differentLengths =
                     list(argNames=c("m1", "m2", "sd1", "sd2", "n1", "n2"),
                          argLengths=argLengths),
                   callingFunction = .curfnfinder()))
    }

  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------
  ###
  ###  Actual functionality
  ###
  ###  At this point, we *must* have (with valid values):
  ###
  ###   - m1
  ###   - m2
  ###   - sd1
  ###   - sd2
  ###   - n1
  ###   - n2
  ###   ~ bias_correct (has a default value)
  ###
  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------
  ### Effect size point estimate
  ###--------------------------------------------------------------------------

  pooledsd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))

  d <- (m1 - m2) / pooledsd

  d <- ifelse(bias_correct, 1 - (3 / (4 * (n1 + n2) - 9)), 1) * d

  ###--------------------------------------------------------------------------
  ### Effect size variance
  ###--------------------------------------------------------------------------

  # https://stats.stackexchange.com/questions/144084/variance-of-cohens-d-statistic
  dVar <- ((n1 + n2) / (n1 * n2)) + ((d^2) / (2 * (n1 + n2)))

  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------
  ###
  ###  Prepare dataframe and return result
  ###
  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------

  minimalMissingMessage <-
    minimalMissingMessage(d, dVar)
  
  return(stats::setNames(data.frame(d, dVar, minimalMissingMessage),
                         c(opts$get("EFFECTSIZE_POINTESTIMATE_NAME_IN_DF"),
                           opts$get("EFFECTSIZE_VARIANCE_NAME_IN_DF"),
                           opts$get("EFFECTSIZE_MISSING_MESSAGE_NAME_IN_DF"))))
  
}
