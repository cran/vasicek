#' Estimating Vasicek Rho parameter by assuming the know P parameter
#'
#' The function \code{vsk_Rho} estimates Rho parameter in the Vasicek
#' distribution by using maximum likelihood estimator, assuming the
#' known P parameter.
#'
#' @param x A numeric vector in the (0, 1) interval that is supposed to
#'          follow the Vasicek distribution
#' @param p A numeric vector in the (0, 1) interval. p has the same length
#'          as x. Each value of p can be a constant or varying. 
#'
#' @return A scalar representing the Rho parameter in the Vasicek distribution.
#'
#' @examples
#' x <- vsk_rvs(1000, Rho = 0.2, P = 0.1)
#' p <- rep(mean(x), length(x))
#' vsk_Rho(x, p)
#' # 0.2110976

vsk_Rho <- function(x, p) {
  x_ <- x[x > 0 & x < 1 & !is.na(x)]
  p_ <- p[p > 0 & p < 1 & !is.na(p)]
  if (length(x_) != length(p_)) stop("x and p have different lengths!", call. = F)
 
  fn <- function(r_) -sum(log(Reduce(c, lapply(1:length(x), 
                                               function(i) vsk_pdf(x_[i], Rho = r_, P = p_[i])))))
  r_ <- optimize(fn, c(0, 1))$minimum
  return(r_)
}
