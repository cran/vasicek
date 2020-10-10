#' Generating random numbers for the Vasicek distribution 
#'
#' The function \code{vsk_rvs} generates random numbers for the Vasicek
#' distribution.
#'
#' @param n    An integer for the number of observations.
#' @param Rho  The Rho parameter in the Vasicek distribution. It is in the 
#'             range of (0, 1).
#' @param P    The P parameter in the Vasicek distribution. It is in the 
#'             range of (0, 1).
#' @param seed An integer that is used as the seed value to generate 
#'             random numbers.
#'
#' @return A list of random number that follows the Vasicek distribution.
#'
#' @examples
#' vsk_rvs(10, Rho = 0.2, P = 0.1)

vsk_rvs <- function(n, Rho, P, seed = 1) {
  set.seed(seed)
  rn <- stats::rnorm(n)
  rv <- stats::pnorm((stats::qnorm(P) - sqrt(Rho) * rn) / sqrt(1 - Rho))
  return(rv)
}
