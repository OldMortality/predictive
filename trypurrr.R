library(purrr)
square <- function(x) {
  return(x^2)
}
v <- seq(1:5)
names(v) <- letters[1:5]
map_dfr(v,square)
