#' Estimating Vasicek parameters by using indirect moment matching
#'
#' The function \code{vsk_imm} estimates parameters in the Vasicek
#' distribution by using indirect moment matching.
#'
#' @param x A numeric vector in the (0, 1) interval that is supposed to
#'          follow the Vasicek distribution
#'
#' @return A list with Vasicek parameters, namely Rho and P.
#'
#' @examples
#' vsk_imm(vsk_rvs(1000, Rho = 0.2, P = 0.1))
#' # $Rho
#' # [1] 0.2110422
#' # $P
#' # [1] 0.1024877

vsk_imm <- function(x) {
  x_ <- x[x > 0 & x < 1 & !is.na(x)]
  mu <- mean(qnorm(x_))
  s2 <- mean((qnorm(x_)) ** 2) - mu * mu
  p_ <- pnorm(mu / sqrt(1 + s2))
  r_ <- s2 / (1 + s2)
  return(list(Rho = r_, P = p_))
}
