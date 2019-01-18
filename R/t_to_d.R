#' Student's *t* to Cohen's *d*
#' 
#' This function converts Student's *t* to Cohen's *d*.
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
#' @examples transformer::t_to_d(t = 2.85, df = 128);
#'
#' @export
t_to_d <- function(t, df, n1=NULL, n2=NULL, proportion=.5) {

  if (missing(t)) {
    stop(.transformer_errmsg(missing='t',
                             callingFunction = .curfnfinder()))
  } else {
    
    if (!is.numeric(t)) {
      stop(.transformer_errmsg(wrongType=list(argName='t',
                                              providedType=class(t),
                                              requiredType='numeric'),
                               callingFunction = .curfnfinder()))
      
    }
    
    if (missing(df) && (missing(n1) && missing(n2))) {
      stop(.transformer_errmsg(conditionalMissing=list(provided=c('t'),
                                                       missing=list('df',
                                                                    c('n1', 'n2'))),
                               callingFunction = .curfnfinder()))
    } else if (!missing(df) && (is.null(proportion) || is.na(proportion))) {
      stop(.transformer_errmsg(conditionalMissing=list(provided='df',
                                                       missing='proportion'),
                               callingFunction = .curfnfinder()))
    }
  }
  
  
  

  if (missing(t) || missing(df)) {
    #if (missing(
  }
  
  
  # assertthat::are_equal(length(t),
  #                       length(df)
  #                       length(n1)
  
  
  if (is.null(df) && !is.null(n1) && !is.null(n2)) {
    groupSize1 <- n1;
    groupSize2 <- n2;
  }
  else if (!is.null(df) && is.null(n1) && is.null(n2)) {
    groupSize1 <-      proportion  * (df + 2);
    groupSize2 <- (1 - proportion) * (df + 2);
  }
  else {
    warning("Specify either df (and ideally proportion) or n1 and n2! Returning NA.");
    return(NA);
  }
  
  ### Updated to reflect http://journal.frontiersin.org/article/10.3389/fpsyg.2013.00863/full
  #   multiplier <- sqrt(((groupSize1 + groupSize2) / (groupSize1 * groupSize2)) *
  #                        ((groupSize1 + groupSize2) / (groupSize1 + groupSize2 - 2)));
  multiplier <- sqrt((1 / groupSize1) + (1 / groupSize2));
  
  d <- t * multiplier;
  
  return(d);
}