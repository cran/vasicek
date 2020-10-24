#' Estimating Vasicek parameters by using direct moment matching
#'
#' The function \code{vsk_mle} estimates parameters in the Vasicek
#' distribution by using direct moment matching.
#'
#' @param x A numeric vector in the (0, 1) interval that is supposed to
#'          follow the Vasicek distribution
#'
#' @return A list with Vasicek parameters, namely Rho and P.
#'
#' @examples
#' vsk_dmm(vsk_rvs(1000, Rho = 0.2, P = 0.1))
#' # $Rho
#' # [1] 0.2135844
#' # $P
#' # [1] 0.1025469

vsk_dmm <- function(x) {
  x_ <- x[x > 0 & x < 1 & !is.na(x)]
  p_ <- mean(x_)
  xx <- mean(x_ ^ 2)
  mu <- c(0, 0)
  fn <- function(r) {
    abs(mvtnorm::pmvnorm(mean = mu, sigma = matrix(c(1, r, r, 1), ncol = 2),
                         lower = rep(-Inf, 2), upper = rep(qnorm(p_), 2)) - xx)
  }
  r_ <- optimize(fn, c(0, 1))$minimum
  return(list(Rho = r_, P = p_))
}
