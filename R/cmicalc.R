#' Bias-correction factor for Cohen's d
#'
#' @param mi Cohen's *d*s degrees of freedom (i.e. $n_1+n_2-2$).
#'
#' @return The bias-correction factor.
#' @export
#'
#' @examples cmicalc(18);
cmicalc <-
  function(mi) {
    ifelse(mi <= 1,
           NA,
           exp(lgamma(mi/2) - log(sqrt(mi/2)) - lgamma((mi-1)/2)))
  }
