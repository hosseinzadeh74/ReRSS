#' Estimation of Mean Past Lifetime under SRS
#'
#' Estimation of Mean Past Lifetime Based on the Simple Random Sampling Scheme (EMPL_SRS)
#' @name EMPL_SRS
#' @param t A numeric vector
#' @param data Data generated from Simple random sampling
#' @return The Estimation of MPL based on the SRS
#' @examples
#' data <- rgamma(10000, shape = 2, scale = 1)
#' t <- seq(0, 10, 0.1)
#' Khat_SRS(t, data)
#' plot(t, Khat_SRS(t, data), type = "l", col = 2)
#' @export Khat_SRS
Khat_SRS <- function(t, data) {
  n <- length(data)
  out <- sapply(t, function(x) (Int_1(x, data) / n) / Fhat(x, data))
  return(out)
}
