#' Calculating the cumulative distribution function of Vasicek
#'
#' The function \code{vsk_cdf} calculates the cumulative distribution 
#' function of Vasicek.
#'
#' @param x   A numeric vector in the [0, 1] interval that is supposed to
#'            follow the Vasicek distribution
#' @param Rho The Rho parameter in the Vasicek distribution
#' @param P   The P parameter in the Vasicek distribution
#'
#' @return A numeric vector with the corresponding cdf.
#'
#' @examples
#' vsk_cdf(c(0.278837772815679, 0.5217229060260343), Rho = 0.2, P = 0.3)
#' # [1] 0.5 0.9

vsk_cdf <- function(x, Rho, P) {
  x_ <- x[x >= 0 & x <= 1 & !is.na(x)]
  return(pnorm((sqrt(1 - Rho) * qnorm(x_) - qnorm(P)) / sqrt(Rho)))
}
