#' Obtain Cohen's *d* from the Pearson correlation *r*
#'
#' This function does not convert the Pearson correlation *r* to Cohen's *d*,
#' because this is not possible. If two variables are continuous, it is not
#' possible to compute a value of Cohen's *d*.
#'
#' This function is included in the `escalc` package for two reasons. First,
#' it provides a consistent API for users (i.e. every conversion function
#' exists). Second, it allows informing users that they try to do something
#' that isn't sensible.
#'
#' @param r_pearson A numerical vector with one or more Pearson *r* values.
#' @param n A numerical vector with the sample sizes of each Pearson *r*
#' value. Note that the *n*th element of these vectors must correspond to
#' the *n*th element of the `r_pearson` vector.
#' @param baseRateSensitive Whether to compute the base rate sensitive
#' Cohen's d or not (see McGrath & Meyer, 2006).
#' @param biasCorrect Logical to indicate if the *d*-values should be
#' bias-corrected. Can also be a vector.
#' @param crosssectionalSampling Logical ...
#'
#' @return Invisibly, a data frame with three columns, as many rows as
#' `r_pearson` and `n` are long, and filled with `NA`s for all cells except
#' those in the last column.
#'
#' @references McGrath, R. E. & Meyer, G. J. (2006) When Effect Sizes Disagree:
#' The Case of *r* and *d*. *Psychological Methods, 11*, 386-401,
#' doi:\doi{10.1037/1082-989X.11.4.386386}
#'
#' @examples
#' escalc::d_from_r_pearson(r_pearson = .3, n = 100)
#'
#' @export
d_from_r_pearson <- function(r_pearson,
                             n,
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

  ###------------------------------------------------------------ r_pearson & n
  ### Argument checking: lengths
  ###------------------------------------------------------------ r_pearson & n

  argLengths <- c(length(r_pearson), length(n));
  if (length(unique(argLengths)) > 1) {
    stop(.errmsg(differentLengths =
                   list(argNames=c("r_pearson", "n"),
                        argLengths=argLengths),
                 callingFunction = .curfnfinder()))
  }

  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------
  ###
  ###  Actual functionality
  ###
  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------

  errorMsg <-
    .functionalityNotImplementedMsg(conversion = "Cohen's d from Pearson's r",
                                    reason = "nonexistent",
                                    callingFunction = "d_from_r_pearson");

  return(stats::setNames(data.frame(rep(NA, length(r_pearson)),
                                    rep(NA, length(r_pearson)),
                                    rep(errorMsg, length(r_pearson))),
                         c(opts$get("EFFECTSIZE_POINTESTIMATE_NAME_IN_DF"),
                           opts$get("EFFECTSIZE_VARIANCE_NAME_IN_DF"),
                           opts$get("EFFECTSIZE_MISSING_MESSAGE_NAME_IN_DF"))))
}
