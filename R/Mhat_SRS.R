#' Estimation of Mean Residual Lifetime under SRS
#'
#' Estimation of Mean Residual Lifetime Based on the Simple Random Sampling Scheme (EMRL_SRS)
#' @param t A numeric vector
#' @param data Data generated from Simple random sampling
#' @return The Estimation of MRL based on the SRS
#' @examples
#' data <- rgamma(10000, shape = 2, scale = 1)
#' t <- seq(0, 10, 0.1)
#' Mhat_SRS(t, data)
#' plot(t, Mhat_SRS(t, data), type = "l", col = 2, ylim = c(0, 3))
#' @export
Mhat_SRS <- function(t, data) {
  n <- length(data)
  out <- sapply(t, function(x) (Int(x, data) / n) / Shat(x, data))
  return(out)
}
