#' Obtain Cohen's *d* from Student's *t*
#' 
#' This function converts Student's *t* to Cohen's *d*.
#' 
#' The formula that is used is the following (see e.g. Lakens, 2013):
#' 
### Wolfgang & Simon:
### So, LaTeX math can just be used, like in regular manual files, see:
###
### https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Mathematics
###
### So: use \eqn or \dqn depending on whether you want inline or 'display';
### and specify two arguments, where the first is LaTeX for processing into
### PDF for the manual, and the second one ASCII for contexts that don't
### support e.g. MathJax:
#' 
#' \deqn{d= t \sqrt{(\frac{1}{n_1} + \frac{1}{n_2})}}{d=t*sqrt(1/n1 + 1/n2))}
#' 
#' @param t The *t* value.
#' @param df The degrees of freedom of `t`; either provide this `proportion`,
#' or `n1` and `n1`.
#' @param n1,n2 The sample sizes of the two groups formed by the dichotomous
#' variable.
#' @param proportion The proportion of participants in the first (or second)
#' group; must be specified if `df` is specified.
#' 
#' @return A numeric vector of Cohen's `d` values.
#' 
#' @references Lakens, D. (2013) Calculating and reporting effect sizes to
#' facilitate cumulative science: a practical primer for t-tests and ANOVAs.
#' *Frontiers in Psychology, 4*, p. 863. \doi{10.3389/fpsyg.2013.00863}
#' 
#' @examples escalc::d_from_t_in(t = 2.85, df = 128);
#'
#' @export
d_from_t_in <- function(t,
                        df,
                        n,
                        proportion,
                        n1,
                        n2,
                        assumeHomoscedacity = TRUE,
                        biasCorrected = FALSE) {

  ###------------------------------------------------------------------------ t
  ### Argument-checking - Check presence
  ###------------------------------------------------------------------------ t
  if (missing(t)) {
    stop(.errmsg(missing='t',
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
  
  
  ###------------------------------------------------------------------------ t
  ### Argument-checking - Check NA, NULL, and class
  ###------------------------------------------------------------------------ t
  if (is.null(t) || is.na(t)) {
    stop(.errmsg(cantBeNullOrNA=list(argName='t',
                                     argVal = t),
                 callingFunction = .curfnfinder()))
  } else if (!is.numeric(t)) {
    stop(.errmsg(wrongType=list(argName='t',
                                providedType=class(t),
                                requiredType='numeric'),
                 callingFunction = .curfnfinder()))
  }
  
  ###--------------------------------------------------------------- proportion
  ### Argument-checking - Check presence
  ###--------------------------------------------------------------- proportion
  if (!missing(df) || !missing(n)) {
    ### Check whether `proportion` was provided
    if (missing(proportion) || is.null(proportion) || is.na(proportion)) {
      stop(.errmsg(conditionalMissing=list(provided='df',
                                           missing='proportion'),
                   callingFunction = .curfnfinder()))
    }
  } else {
    ### df and n are missing
    if (missing(n1) || missing(n2)) {
      stop(.errmsg(conditionalMissing=list(provided=c('t'),
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

  ###--------------------------------------------------------------- t, n1 & n2
  ### Argument checking: lengths
  ###--------------------------------------------------------------- t, n1 & n2
  
  if (!missing(n1) && !missing(n2)) {
    argLengths <- c(length(t), length(n2), length(n2));
    if (length(unique(argLengths)) > 1) {
      stop(.errmsg(differentLengths =
                     list(argNames=c("t", "n1", "n2"),
                          argLengths=argLengths),
                   callingFunction = .curfnfinder()))
    }
  }
    
  ###------------------------------------------------------- t, df & proportion
  ### Argument checking: lengths
  ###------------------------------------------------------- t, df & proportion
  
  if (!missing(df)) {
    argLengths <- c(length(t), length(df), length(proportion));
    if (length(unique(argLengths)) > 1) {
      stop(.errmsg(differentLengths =
                     list(argNames=c("t", "df", "proportion"),
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

  ###--------------------------------------------------------------------------
  ###
  ###  Actual functionality
  ###
  ###--------------------------------------------------------------------------
  
  if (!assumeHomoscedacity) {
    stop(.functionalityNotImplementedMsg(conversion = "d from Welch's t",
                                         reason = "nonexistent",
                                         callingFunction = .curfnfinder()))
  }

  ###--------------------------------------------------------------------------
  ### Effect size point estimate
  ###--------------------------------------------------------------------------

  ### Updated to reflect http://journal.frontiersin.org/article/10.3389/fpsyg.2013.00863/full
  #   multiplier <- sqrt(((groupSize1 + groupSize2) / (groupSize1 * groupSize2)) *
  #                        ((groupSize1 + groupSize2) / (groupSize1 + groupSize2 - 2)))
  
  multiplier <- sqrt((1 / n1) + (1 / n2))
  
  d <- t * multiplier

  ###--------------------------------------------------------------------------
  ### Effect size variance
  ###--------------------------------------------------------------------------
  
  # https://stats.stackexchange.com/questions/144084/variance-of-cohens-d-statistic
  
  dVar <- ((n1 + n2) / (n1 * n2)) + ((d^2) / (2*(n1+n2)))
  
  ###--------------------------------------------------------------------------
  ###
  ###  Prepare dataframe and return result
  ###
  ###--------------------------------------------------------------------------
  
  return(setNames(data.frame(d, dVar),
                  c(.EFFECTSIZE_POINTESTIMATE_NAME_IN_DF,
                    .EFFECTSIZE_VARIANCE_NAME_IN_DF)));
}
