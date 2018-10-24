#' @export
convert.f.to.etasq <- function(f, df1, df2) {
  return( (f * df1) / ((f * df1) + df2) );
}