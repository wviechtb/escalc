#' @export
convert.t.to.d <- function(t, df=NULL, n1=NULL, n2=NULL, proportion=.5) {
  
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