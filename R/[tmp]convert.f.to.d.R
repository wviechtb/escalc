#' @export
convert.f.to.d <- function(f, df1, df2 = NULL, n1=NULL, n2=NULL, proportion=.5) {
  if (df1 != 1) {
    warning("You can only convert an F value for the comparison of two groups to Cohen's d, ",
            "and you specified a df1 of ", df1, ", which means this F value concerns the comparison ",
            "of ", df1 + 1, " groups. Returning NA.");
    return(NA);
  }
  else if (is.null(df2) && !is.null(n1) && !is.null(n2)) {
    groupSize1 <- n1;
    groupSize2 <- n2;
  }
  else if (!is.null(df2) && is.null(n1) && is.null(n2)) {
    groupSize1 <- proportion * (df1 + df2 + 1);
    groupSize2 <- (1 - proportion) * (df1 + df2 + 1);
  }
  else {
    warning("Specify either df2 (and ideally proportion) or n1 and n2! Returning NA.");
    return(NA);
  }
  
  d <- sqrt(f * ((groupSize1 + groupSize2) / (groupSize1 * groupSize2)) *
              ((groupSize1 + groupSize2) / (groupSize1 + groupSize2 - 2)));
  
  return(d);
}