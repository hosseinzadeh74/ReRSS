#' Estimation of Mean Past Lifetime under RSS
#'
#' Estimation of Mean Past Lifetime Based on the Ranked Set Sampling Scheme (EMPL_RSS)
#' @name EMPL_RSS
#' @param RSSdata Data generated from Ranked Set Sampling Scheme
#' @param t A numeric vector
#' @param k  Collection size
#' @param m Number of cycles
#' @return The Estimation of MPL based on the RSS
#' @examples
#' k <- 5
#' m <- 5
#' lambda <- 1
#' RSSdata <- matrix(0, nrow = k, ncol = m)
#' for (j in 1:m) {
#'   for (i in 1:k) {
#'     x <- rgamma(k, shape = 2, scale = 1)
#'     y <- lambda * ((x - mean(x)) / sd(x)) + sqrt((1 - lambda^2)) * rnorm(k, 0, 1)
#'     sy <- sort(y)
#'     syindex <- sort(y, index.return = TRUE)$ix
#'     RSSdata[i, j] <- x[syindex][i]
#'   }
#' }
#' Khat_RSS(c(1, 2, 4, 5), RSSdata, k = 5, m = 5)
#' @export Khat_RSS
Khat_RSS <- function(t, RSSdata, k, m) {
  out <- sapply(t, function(x) 1 / k * colSums((Fhat_i(x, RSSdata) / Fhat(x, c(RSSdata))) * KhatRSS_i(x, RSSdata, k, m)))
  return(out)
}
