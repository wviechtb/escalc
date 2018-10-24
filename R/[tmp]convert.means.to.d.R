#' @export
convert.means.to.d <- function(means, sds, ns = NULL, var.equal=NULL) {
  if (is.null(ns)) {
    var <- mean(sds);
  } else {
    if (is.null(var.equal)) {
      ### Try to establish equality ourselves
      if (max(sds) < (3 * min(sds))) {
        ### Consider then equal
        var.equal <- TRUE;
      } else {
        ### Consider them different
        var.equal <- FALSE;
      }
    }
    if (var.equal) {
      ss1 <- sds[1]^2 * (ns[1] - 1);
      ss2 <- sds[2]^2 * (ns[2] - 1);
      var <- (ss1 + ss2) / (ns[1] + ns[2] - 2);
    } else {
      ### Take variance of smallest group
      var <- sds[ns == min(ns)] ^ 2;
    }
  }
  ### Compute difference between means and divide
  ### by standard deviation
  return((means[2] - means[1]) / sqrt(var));
}