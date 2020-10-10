#' Calculating the percentile point function of Vasicek
#'
#' The function \code{vsk_ppf} calculates the percentile point
#' function of Vasicek.
#'
#' @param Alpha A numeric vector of probabilities
#' @param Rho   The Rho parameter in the Vasicek distribution
#' @param P     The P parameter in the Vasicek distribution
#'
#' @return A numeric vector with the corresponding ppf.
#'
#' @examples
#' vsk_ppf(c(0.5, 0.9), Rho = 0.2, P = 0.3)
#' # [1] 0.2788378 0.5217229

vsk_ppf <- function(Alpha, Rho, P) {
  a_ <- Alpha[Alpha >= 0 & Alpha <= 1 & !is.na(Alpha)]
  return(pnorm((qnorm(P) + sqrt(Rho) * qnorm(a_)) / sqrt(1 - Rho)))
}
