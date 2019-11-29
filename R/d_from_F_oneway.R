#' Obtain Cohen's *d* from one-way F tests
#' 
#' This function converts F statistics from one-way tests to Cohen's *d*.
#' 
#' The formula that is used is the following (see e.g. Thalheimer & Cook, 
#' 2002):
#' 
#' \deqn{d= \sqrt{F (\frac{n_1 + n_2}{n_1 n_2}) 
#' (\frac{n_1 + n_2}{n_1 + n_2 - 2})}}
#' 
#' @param F1 A numerical vector with one or more *F* values.
#' @param n1,n2 A numerical vector with the sample sizes of the two groups
#' formed by the dichotomous variable. Note that the *n*th element of these
#' vectors must correspond to the *n*th elements of the `F1` vector.
#' @param df2 A numerical vector with one or more values of degrees of freedom
#' 2.
#' @param sign Numerical vector to indicate the sign of the d-values 
#' (\code{+1} or \code{-1}).
#' @param bias_correct Logical to indicate if the *d*-values should be
#' bias-corrected. Can also be a vector.
#' 
#' @return A data frame with in the first column, Cohen's `d` values, and
#' in the second column, the corresponding variances.
#' 
#' @references Thalheimer, W., & Cook, S. (2002, August). *How to calculate 
#' effect sizes from published research articles: A simplified methodology.*
#' @references Borenstein, M., Hedges, L. V., Higgins, J. P. T., & Rothstein,
#'  H. R. (2009). *Introduction to Meta-Analysis*. Chichester, UK: John Wiley 
#'  & Sons.
#' 
#' @examples
#' escalc::d_from_F_oneway(F1 = 2.828427,
#'                         n1 = 126,
#'                         n2 = 89,
#'                         sign = +1);
#' escalc::d_from_F_oneway
#'
#' @export
d_from_F_oneway <- function(F1,
                            n1,
                            n2,
                            df2,
                            sign,
                            bias_correct = FALSE) {
  
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
  if (missing(F1)) {
    stop(.errmsg(missing='F1',
                 callingFunction = .curfnfinder()))
  }
  if (missing(n1) & missing(df2)) {
    stop(.errmsg(missing='n1',
                 callingFunction = .curfnfinder()))
  }
  if (missing(n2) & missing(df2)) {
    stop(.errmsg(missing='n2',
                 callingFunction = .curfnfinder()))
  }
  if (missing(n1) | missing(n2)) {
    n1 <- (df2+2)/2
    n2 <- (df2+2)/2
  }
  
  ###--------------------------------------------------------------- t, n1 & n2
  ### Argument checking: lengths
  ###--------------------------------------------------------------- t, n1 & n2
  
  #if (!missing(n1) && !missing(n2)) {
  argLengths <- c(length(F1),
                  length(n1), length(n2));
  if (length(unique(argLengths)) > 1) {
    stop(.errmsg(differentLengths =
                   list(argNames=c("F1", "n1", "n2"),
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
  ###   - F1
  ###   - n1
  ###   - n2
  ###   - sign
  ###   ~ bias_correct (has a default value)
  ###
  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------
  ### Effect size point estimate
  ###--------------------------------------------------------------------------
  
  d <- sqrt(F * ((n1 + n2) / (n1 * n2)) * ((n1 + n2) / (n1 + n2 - 2)))
  
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
  
  return(stats::setNames(data.frame(d, dVar),
                         c(.EFFECTSIZE_POINTESTIMATE_NAME_IN_DF,
                           .EFFECTSIZE_VARIANCE_NAME_IN_DF)))
}
