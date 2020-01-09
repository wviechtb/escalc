#' Obtain Cohen's *d* from mean squared error
#'
#' This function converts mean squared error from one-way F tests to Cohen's
#' *d*.
#'
#' The formula that is used is the following (see e.g. Thalheimer & Cook,
#' 2002):
#'
#' \deqn{d= \frac{\hat{x}_1 - \hat{x}_2}
#' {MSE (\frac{n_1 + n_2 - 2}{n_1 + n_2})}}
#'
#' @param MSE A numerical vector with one or more mean squared error values.
#' @param m1,m2 A numerical vector with the means of the two groups formed by
#' the dichotomous variable. Note that the *n*th element of these vectors must
#' correspond to the *n*th elements of the `MSE` vector.
#' @param n1,n2 A numerical vector with the sample sizes of the two groups
#' formed by the dichotomous variable. Note that the *n*th element of these
#' vectors must correspond to the *n*th elements of the `MSE` vector.
#' @param biasCorrect Logical to indicate if the *d*-values should be
#' bias-corrected. Can also be a vector.
#' @param stopOnErrors On which errors to stop (see the manual page for [escalc::opts()] for more details).
#'
#' @return A data frame with in the first column, Cohen's `d` values, and
#' in the second column, the corresponding variances.
#'
#' @references Thalheimer, W., & Cook, S. (2002, August). *How to calculate effect sizes from published
#' research articles: A simplified methodology.*
#'
#' @examples
#' escalc::d_from_MSE(MSE = 2.046,
#'                    m1 = .024,
#'                    m2 = .301,
#'                    n1 = 50,
#'                    n2 = 50);
#'
#' @export
d_from_MSE <- function(MSE,
                       m1,
                       m2,
                       n1,
                       n2,
                       biasCorrect = FALSE,
                       stopOnErrors = opts$get(stopOnErrors)) {

  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------
  ###
  ### Argument checking
  ###
  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------

  ###--------------------------------------------------------------- F1, n1, n2
  ### Argument-checking - Check presence
  ###--------------------------------------------------------------- F1, n1, n2
  if (missing(MSE)) {
    stop(.errmsg(missing='MSE',
                 callingFunction = .curfnfinder()))
  }
  if (missing(m1)) {
    stop(.errmsg(missing='m1',
                 callingFunction = .curfnfinder()))
  }
  if (missing(m2)) {
    stop(.errmsg(missing='m2',
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
  argLengths <- c(length(MSE),
                  length(m1), length(m2),
                  length(n1), length(n2));
  if (length(unique(argLengths)) > 1) {
    stop(.errmsg(differentLengths =
                   list(argNames=c("F1", "m1", "m2", "n1", "n2"),
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
  ###   - MSE
  ###   - m1
  ###   - m2
  ###   - n1
  ###   - n2
  ###   ~ biasCorrect (has a default value)
  ###
  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------
  ### Effect size point estimate
  ###--------------------------------------------------------------------------

  d <- (m1 - m2)/sqrt(MSE * ((n1 + n2 - 2) / (n1 + n2)))

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

  .minimalMissingMessage <-
    .minimalMissingMessage(d, dVar,
                           callingFunction = .curfnfinder(),
                           stopOnErrors=stopOnErrors)
  
  return(stats::setNames(data.frame(d, dVar, .minimalMissingMessage),
                         c(opts$get("EFFECTSIZE_POINTESTIMATE_NAME_IN_DF"),
                           opts$get("EFFECTSIZE_VARIANCE_NAME_IN_DF"),
                           opts$get("EFFECTSIZE_MISSING_MESSAGE_NAME_IN_DF"))))
  
}