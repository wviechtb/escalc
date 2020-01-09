### This is here mostly as documentation for later use; see meeting minutes from
### 2019-09-27 at https://docs.google.com/document/d/1Si3vgfwdosusvUde4-rZRo7yUTKSJEhCXetO0jz9AYk/edit

r_pb_from_d <- function(d, n1, n2,
                        stopOnErrors = opts$get(stopOnErrors)) {
  
  n = n1 + n2
  m = n - 2
  h <- m/n1 + m/n2
  r_PB <- d / sqrt(d^2 + h)
  #reverse to get:
    d = f(r_PB)


  ### The variance computation depends on the type of sampling. If both variables are measured
  ### (i.e. cross-sectional as opposed to stratified sampling as in the case of experiments),
  ### `h` is also a random variable, whereas in experiments, it’s a constant.
  
  ### For stratified sampling
  Var(r_PB) <- h^2 / (h + d^2)^3 * (1/n1 + 1/n2 + d^2/(2*n))
  
  # (which is equal to: Var(r_PB) <- h^2 / (h + d^2)^3 * Var(d))
  
  
  ### For cross-sectional sampling
  Var(r_PB) <- (1-r^2)^2 * (n*r^2 / (4*n1*n2) + (2-3*r^2)/(2*n)) # from Tate (1954, 1955b)
  
  
}
