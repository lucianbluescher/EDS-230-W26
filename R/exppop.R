#'  Simple population growth
#' @param T  period of growth
#' @param P initial population
#' @param r intrinsic growth rate
#' @return population at time T
#'
exppop <- function(T, P0, r) {
  # analytical calculation of population
  P <- P0 * exp(r * T)
   return(P)
}
