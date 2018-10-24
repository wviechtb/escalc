#' @export
convert.f.to.omegasq <- function(f, df1, df2) {
  return( (f - 1) / (f + (df2 + 1) / (df1)) );
}