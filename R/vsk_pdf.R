#' Calculating the probability density function of Vasicek
#'
#' The function \code{vsk_pdf} calculates the probability density
#' function of Vasicek.
#'
#' @param x   A numeric vector in the (0, 1) interval that is supposed to
#'            follow the Vasicek distribution
#' @param Rho The Rho parameter in the Vasicek distribution
#' @param P   The P parameter in the Vasicek distribution
#'
#' @return A numeric vector with the corresponding pdf.
#'
#' @examples
#' vsk_pdf(c(0.01, 0.02), Rho = 0.2, P = 0.3)
#' # [1] 0.07019659 0.22207564

vsk_pdf <- function(x, Rho, P) {
  x_ <- x[x > 0 & x < 1 & !is.na(x)]
  return(sqrt((1 - Rho) / Rho) * exp(-1 / (2 * Rho) 
         * (sqrt(1 - Rho) * qnorm(x_) - qnorm(P)) ^ 2 
         + 1 / 2 * (qnorm(x_)) ^ 2))
}
