#' Photovoltaic energy output
#' 
#'This function computes energy produced from a photovoltaic system given the average annual solar radiation
#' @param A Solar panel area (m^2)
#' @param r Panel yield (0–1, default = 0.2)
#' @param H Annual average solar radiation (kWh m^-2)
#' @param PR Performance ratio (0-1, default = 0.75)
#'
#' @returns E is the energy produced (kWh)
#' @export
#'
#' @examples
#' energy_function(10, 30)
#' returns 45
energy_function <- function(A, r = 0.2, H, PR = 0.75) {

  E <- A * r * H * PR
  
  return(E)
}
