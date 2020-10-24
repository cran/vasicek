#' Estimating Vasicek parameters by using quantile-based estimator
#'
#' The function \code{vsk_qbe} estimates parameters in the Vasicek
#' distribution by using quantile-based estimator. It is not recommended
#' for small sample size.
#'
#' @param x A numeric vector in the (0, 1) interval that is supposed to
#'          follow the Vasicek distribution
#'
#' @return A list with Vasicek parameters, namely Rho and P.
#'
#' @examples
#' vsk_qbe(vsk_rvs(1000, Rho = 0.2, P = 0.1))
#' # $Rho
#' # [1] 0.1941091
#' # $P
#' # [1] 0.1019701

vsk_qbe <- function(x) {
  x_ <- stats::qnorm(x[x > 0 & x < 1 & !is.na(x)])
  mu <- stats::quantile(x_, 0.50, names = FALSE)
  s2 <- ((stats::quantile(x_, 0.75, names = FALSE) - mu) / stats::qnorm(0.75)) ^ 2
  r_ <- s2 / (1 + s2)
  p_ <- pnorm(mu / (sqrt(1 + s2)))
  return(list(Rho = r_, P = p_))
}
