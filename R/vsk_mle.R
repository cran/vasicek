#' Estimating Vasicek parameters by using maximum likelihood estimator
#'
#' The function \code{vsk_mle} estimates parameters in the Vasicek
#' distribution by using maximum likelihood estimator.
#'
#' @param x A numeric vector in the (0, 1) interval that is supposed to
#'          follow the Vasicek distribution
#'
#' @return A list with Vasicek parameters, namely Rho and P.
#'
#' @examples
#' vsk_mle(vsk_rvs(1000, Rho = 0.2, P = 0.1))
#' # $Rho
#' # [1] 0.2110976
#' # $P
#' # [1] 0.1025469

vsk_mle <- function(x) {
  x_ <- x[x > 0 & x < 1 & !is.na(x)]
  p_ <- mean(x_)
  fn <- function(r_) -sum(log(vsk_pdf(x_, Rho = r_, P = p_)))
  r_ <- optimize(fn, c(0, 1))$minimum
  return(list(Rho = r_, P = p_))
}
