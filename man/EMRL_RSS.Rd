% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Mhat_RSS.R
\name{EMRL_RSS}
\alias{EMRL_RSS}
\alias{Mhat_RSS}
\title{Estimation of Mean Residual Lifetime under RSS}
\usage{
Mhat_RSS(t, RSSdata, k, m)
}
\arguments{
\item{t}{A numeric vector}

\item{RSSdata}{Data generated from Ranked Set Sampling Scheme}

\item{k}{Collection size}

\item{m}{Number of cycles}
}
\value{
The Estimation of MRL based on the RSS
}
\description{
Estimation of Mean Residual Lifetime Based on the Ranked Set Sampling Scheme (EMRL_RSS)
}
\examples{
k <- 5
m <- 5
lambda <- 1
RSSdata <- matrix(0, nrow = k, ncol = m)
for (j in 1:m) {
  for (i in 1:k) {
    x <- rgamma(k, shape = 2, scale = 1)
    y <- lambda * ((x - mean(x)) / sd(x)) + sqrt((1 - lambda^2)) * rnorm(k, 0, 1)
    sy <- sort(y)
    syindex <- sort(y, index.return = TRUE)$ix
    RSSdata[i, j] <- x[syindex][i]
  }
}
Mhat_RSS(seq(0, 10, 0.1), RSSdata, k = 5, m = 5)
}
