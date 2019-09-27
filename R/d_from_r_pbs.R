#' Obtain Cohen's *d* from the point biserial correlation *r*
#' 
#' This function converts the point biserial correlation *r* to Cohen's *d*.
#' 
#' The formula that is used is the following (see e.g. Borenstein et al., 2009):
#' 
#' \deqn{d= \frac{2 r_{pbs}}{\sqrt{1 - r^2}} }
#' 
#' @param r_pbs A numerical vector with one or more point biserial *r* values.
#' @param df,n A numerical vector with the degrees of freedom (`df`) of `r_pbs` or
#' the total sample size (`n`), which is \eqn{df + 2}. Either provide exactly
#' one of `df` or `n`, and corresponding `proportion`s; *or* provide `n1`
#' and `n1`. Note that the *n*th element of the `df` and `n` vectors must
#' correspond to the *n*th element of the `r_pbs` vector.
#' @param n1,n2 A numerical vector with the sample sizes of the two groups
#' formed by the dichotomous variable. Note that the *n*th element of these
#' vectors must correspond to the *n*th element of the `r_pbs` vector.
#' @param proportion A numerical vector with the proportion of participants
#' in the first (or therefore, implicitly, second) group; must be specified
#' if `df` or `n` is specified.  Note that the *n*th element of this vector
#' must correspond to the *n*th element of the `r_pbs` vector.
#' @param baseRateSensitive Whether to compute 
#' @param biasCorrected Whether to ------ Wolfgang, Simon, just to check--- 
#' are we talking whether to _deliver_ d or Hedges' g?
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
#' escalc::d_from_r_pbs(r_pbs = .3,
#'                      n = 100,
#'                      proportion=.5);
#'
#' @export
d_from_r_pbs <- function(r_pbs,
                         df = NULL,
                         n = NULL,
                         proportion = NULL,
                         n1 = NULL,
                         n2 = NULL,
                         baseRateSensitive = FALSE,
                         biasCorrected = FALSE) {

  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------
  ###
  ### Argument checking
  ###
  ###--------------------------------------------------------------------------
  ###--------------------------------------------------------------------------
  
  ###-------------------------------------------------------------------- r_pbs
  ### Argument-checking - Check presence
  ###-------------------------------------------------------------------- r_pbs
  if (missing(r_pbs)) {
    stop(.errmsg(missing='r_pbs',
                 callingFunction = .curfnfinder()))
  }
  
  ###-------------------------------------------------------------------- df, n
  ### Argument checking - Check redundancy
  ###-------------------------------------------------------------------- df, n
  
  if (!missing(df) && (!missing(n))) {
    stop(.errmsg(argumentRedundancy=list(argNames1='df',
                                         argNames2='n'),
                 callingFunction = .curfnfinder()))
  }
  
  ###---------------------------------------------------------------- n, n1 & n2
  ### Argument checking - Check redundancy
  ###---------------------------------------------------------------- n, n1 & n2
  
  if (!missing(n) && (!missing(n1) || !missing(n2))) {
    stop(.errmsg(argumentRedundancy=list(argNames1='n',
                                         argNames2=c('n1', 'n2')),
                 callingFunction = .curfnfinder()))
  }
  
  ###-------------------------------------------------------------- df, n1 & n2
  ### Argument checking - Check redundancy
  ###-------------------------------------------------------------- df, n1 & n2
  
  if (!missing(df) && (!missing(n1) || !missing(n2))) {
    stop(.errmsg(argumentRedundancy=list(argNames1='df',
                                         argNames2=c('n1', 'n2')),
                 callingFunction = .curfnfinder()))
  }
  
  
  ###-------------------------------------------------------------------- r_pbs
  ### Argument-checking - Check NA, NULL, and class
  ###-------------------------------------------------------------------- r_pbs
  if (is.null(r_pbs) || is.na(r_pbs)) {
    stop(.errmsg(cantBeNullOrNA=list(argName='r_pbs',
                                     argVal = r_pbs),
                 callingFunction = .curfnfinder()))
  } else if (!is.numeric(r_pbs)) {
    stop(.errmsg(wrongType=list(argName='r_pbs',
                                providedType=class(r_pbs),
                                requiredType='numeric'),
                 callingFunction = .curfnfinder()))
  }
  
  ###--------------------------------------------------------------- proportion
  ### Argument-checking - Check presence
  ###--------------------------------------------------------------- proportion
  if (!missing(df) || !missing(n)) {
    ### Check whether `proportion` was provided
    if (missing(proportion) || is.null(proportion) || is.na(proportion)) {
      stop(.errmsg(conditionalMissing=list(provided='df or n',
                                           missing='proportion'),
                   callingFunction = .curfnfinder()))
    }
  } else {
    ### df and n are missing
    if (missing(n1) || missing(n2)) {
      stop(.errmsg(conditionalMissing=list(provided=c('r_pbs'),
                                           missing=list('df',
                                                        'n',
                                                        c('n1', 'n2'))),
                   callingFunction = .curfnfinder()))
    }
  }
  
  ###----------------------------------------------------------------------- df
  ### Argument checking - Check valid values
  ###----------------------------------------------------------------------- df
  
  if (!missing(df)) {
    if (!(is.numeric(df) && (all(df > 2)))) {
      stop(.errmsg(invalidValue=list(argName="df",
                                     argVal=df,
                                     validValues="higher than 2"),
                   callingFunction = .curfnfinder()))
    }
  }
  
  ###--------------------------------------------------------------- proportion
  ### Argument checking - Check valid values
  ###--------------------------------------------------------------- proportion
  
  if (!missing(proportion)) {
    if (any((proportion * (df + 2)) < 2)) {
      stop(.errmsg(invalidValueCombo=
                     list(argName=c("df", "proportion"),
                          argVal=c(df, proportion),
                          validValues=paste0("the product of ",
                                             "proportion and (df+2) ",
                                              "must be larger than 2")),
                   callingFunction = .curfnfinder()))
    }
  }

  ###----------------------------------------------------------- r_pbs, n1 & n2
  ### Argument checking: lengths
  ###----------------------------------------------------------- r_pbs, n1 & n2
  
  if (!missing(n1) && !missing(n2)) {
    argLengths <- c(length(t), length(n2), length(n2));
    if (length(unique(argLengths)) > 1) {
      stop(.errmsg(differentLengths =
                     list(argNames=c("r_pbs", "n1", "n2"),
                          argLengths=argLengths),
                   callingFunction = .curfnfinder()))
    }
  }
    
  ###--------------------------------------------------- r_pbs, df & proportion
  ### Argument checking: lengths
  ###--------------------------------------------------- r_pbs, df & proportion
  
  if (!missing(df)) {
    argLengths <- c(length(t), length(df), length(proportion));
    if (length(unique(argLengths)) > 1) {
      stop(.errmsg(differentLengths =
                     list(argNames=c("r_pbs", "df", "proportion"),
                          argLengths=argLengths),
                   callingFunction = .curfnfinder()))
    }
  }
    
  ###---------------------------------------------------------- df & proportion
  ### Argument preprocessing
  ###---------------------------------------------------------- df & proportion
  
  if (!missing(df)) {
    n1 <-      proportion  * (df + 2);
    n2 <- (1 - proportion) * (df + 2);
  }

  ###----------------------------------------------------------- n & proportion
  ### Argument preprocessing
  ###----------------------------------------------------------- n & proportion
  
  if (!missing(n)) {
    n1 <-      proportion  * n;
    n2 <- (1 - proportion) * n;
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
    stop(.functionalityNotImplementedMsg(conversion = paste0("converting d from a
                                                             point biserial correlation
                                                             in a base rate sensitive way"),
                                         reason = "notyet",
                                         callingFunction = .curfnfinder()))
  }

  ###--------------------------------------------------------------------------
  ### Effect size point estimate
  ###--------------------------------------------------------------------------

  if (biasCorrected) {
    stop(.functionalityNotImplementedMsg(conversion = "d from a bias corrected point biserial r",
                                         reason = "notyet",
                                         callingFunction = .curfnfinder()))
  } else {
    
    d = (2 * r_pbs) / sqrt( 1 - r_pbs^2)
    
  }

  ###--------------------------------------------------------------------------
  ### Effect size variance
  ###--------------------------------------------------------------------------
  
  if (biasCorrected) {
    stop(.functionalityNotImplementedMsg(conversion = "d from a bias corrected point biserial r",
                                         reason = "notyet",
                                         callingFunction = .curfnfinder()))
  } else {
    # https://stats.stackexchange.com/questions/144084/variance-of-cohens-d-statistic
    dVar <- ((n1 + n2) / (n1 * n2)) + ((d^2) / (2*(n1+n2)))
  }
  
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
