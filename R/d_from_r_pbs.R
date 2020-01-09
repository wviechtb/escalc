#' Obtain Cohen's *d* from the point biserial correlation *r*
#'
#' This function converts the point biserial correlation *r* to Cohen's *d*.
#'
#' The formula that is used is the following (see e.g. Borenstein et al., 2009):
#'
#' \deqn{d= \frac{2 r_{pbs}}{\sqrt{1 - r^2}} }
#'
#' @param r_pb A numerical vector with one or more point biserial *r* values.
#' @param n1,n2 A numerical vector with the sample sizes of the two groups
#' formed by the dichotomous variable. Note that the *n*th element of these
#' vectors must correspond to the *n*th element of the `r_pbs` vector.
#' @param baseRateSensitive Whether to compute tyhe base rate sensitive
#' Cohen's d or not (see McGrath & Meyer, 2006).
#' @param biasCorrect Logical to indicate if the *d*-values should be
#' bias-corrected. Can also be a vector.
#' @param crosssectionalSampling Logical ...
#'
#' @return A data frame with in the first column, Cohen's `d` values, and
#' in the second column, the corresponding variances.
#'
#' @references Borenstein, M., Hedges, L. V., Higgins, J. P. T., &
#' Rothstein, H. R. (2009) *Introduction to Meta-Analysis*, Chichester,
#' UK: John Wiley & Sons, Ltd.
#'
#' Also see:
#'
#' McGrath, R. E. & Meyer, G. J. (2006) When Effect Sizes Disagree:
#' The Case of *r* and *d*. *Psychological Methods, 11*, 386-401,
#' doi:\doi{10.1037/1082-989X.11.4.386386}
#'
#' @examples
#' escalc::d_from_r_pb(r_pb = .3,
#'                     n1 = 50, n2 =50)
#'
#' @export
d_from_r_pb <- function(r_pb,
                        n1,
                        n2,
                        baseRateSensitive = FALSE,
                        biasCorrect = FALSE,
                        crosssectionalSampling = FALSE) {

  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------
  ###
  ### Argument checking
  ###
  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------

  ###------------------------------------------------------------- r_pb, n1, n2
  ### Argument-checking - Check presence
  ###------------------------------------------------------------- r_pb, n1, n2
  if (missing(r_pb)) {
    stop(.errmsg(missing='r_pb',
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

  ###------------------------------------------------------------ r_pb, n1 & n2
  ### Argument checking: lengths
  ###------------------------------------------------------------ r_pb, n1 & n2

  argLengths <- c(length(r_pb), length(n2), length(n2));
  if (length(unique(argLengths)) > 1) {
    stop(.errmsg(differentLengths =
                   list(argNames=c("r_pb", "n1", "n2"),
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
  ###   - r_pbs
  ###   - n1
  ###   - n2
  ###   ~ baseRateSensitive (has a default value)
  ###   ~ biasCorrected (has a default value)
  ###
  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------

  if (baseRateSensitive) {
    stop(.functionalityNotImplementedMsg(conversion = paste0("d from a ",
                                                             "point biserial correlation ",
                                                             "in a base rate sensitive way"),
                                         reason = "notyet",
                                         callingFunction = .curfnfinder()))
  }

  ###--------------------------------------------------------------------------
  ### Effect size point estimate
  ###--------------------------------------------------------------------------

  d = (2 * r_pb) / sqrt( 1 - r_pb^2)

  ##################################
  ################################## See meeting minutes 2019-09-27 and r_pb_from_d.R
  ##################################

  d <- ifelse(biasCorrect, cmicalc(n1+n2-2), 1) * d

  ###--------------------------------------------------------------------------
  ### Effect size variance
  ###--------------------------------------------------------------------------

  # https://stats.stackexchange.com/questions/144084/variance-of-cohens-d-statistic
  dVar <- ((n1 + n2) / (n1 * n2)) + ((d^2) / (2*(n1+n2)))


  ###--------------------------------------------------------------------------
  ### Specify reason for missing values, if any
  ###--------------------------------------------------------------------------

  naReasons <- (rep(NA, length(d)))

  naReasons <-
    ifelse(is.na(r_pb) | is.na(n1) | is.na(n2),
           paste0("When running `d_from_r_pb`, arguments ",
                  "`r_pb`, `n1` and/or `n`2 ",
                  "had missing values (i.e. NA)."),
           naReasons);

  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------
  ###
  ###  Prepare dataframe and return result
  ###
  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------

  return(stats::setNames(data.frame(d, dVar, naReasons),
                         c(opts$get("EFFECTSIZE_POINTESTIMATE_NAME_IN_DF"),
                           opts$get("EFFECTSIZE_VARIANCE_NAME_IN_DF"),
                           opts$get("EFFECTSIZE_MISSING_MESSAGE_NAME_IN_DF"))))
}
