% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Khat_SRS.R
\name{EMPL_SRS}
\alias{EMPL_SRS}
\alias{Khat_SRS}
\title{Estimation of Mean Past Lifetime under SRS}
\usage{
Khat_SRS(t, data)
}
\arguments{
\item{t}{A numeric vector}

\item{data}{Data generated from Simple random sampling}
}
\value{
The Estimation of MPL based on the SRS
}
\description{
Estimation of Mean Past Lifetime Based on the Simple Random Sampling Scheme (EMPL_SRS)
}
\examples{
data <- rgamma(10000, shape = 2, scale = 1)
t <- seq(0, 10, 0.1)
Khat_SRS(t, data)
plot(t, Khat_SRS(t, data), type = "l", col = 2)
}
