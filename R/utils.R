Shat <- function(x, data) {
  y <- sapply(x, function(t) mean(data > t))
  return(y)
}
Int <- function(x, data) {
  y <- sapply(x, function(t) sum((data - t) * (data > t)))
  return(y)
}
Shat_i <- function(x, RSSdata) {
  f <- function(t0) {
    g <- function(x0) {
      return(mean(x0 > t0))
    }
    return(apply(RSSdata, 1, g))
  }
  return(sapply(x, f))
}
IntRSS_i <- function(x, RSSdata) {
  f <- function(t0) {
    g <- function(x0) {
      return(sum((x0 - t0) * (x0 > t0)))
    }
    return(apply(RSSdata, 1, g))
  }
  return(sapply(x, f))
}
MhatRSS_i <- function(t, RSSdata, k, m) {
  out_hat <- matrix(0, nrow = k, ncol = length(t))
  M1 <- IntRSS_i(t, RSSdata)
  M2 <- Shat_i(t, RSSdata)
  for (i in 1:k) {
    out_hat[i, ] <- (M1[i, ] / m) / M2[i, ]
  }
  return(out_hat)
}

## K hat ####
Fhat <- function(x, data) {
  y <- sapply(x, function(t) mean(data <= t))
  return(y)
}
Int_1 <- function(x, data) {
  y <- sapply(x, function(t) sum((t - data) * (data <= t)))
  return(y)
}

Fhat_i <- function(x, RSSdata) {
  f <- function(t0) {
    g <- function(x0) {
      return(mean(x0 <= t0))
    }
    return(apply(RSSdata, 1, g))
  }
  return(sapply(x, f))
}

IntRSS1_i <- function(x, RSSdata) {
  f <- function(t0) {
    g <- function(x0) {
      return(sum((t0 - x0) * (x0 <= t0)))
    }
    return(apply(RSSdata, 1, g))
  }
  return(sapply(x, f))
}
KhatRSS_i <- function(t, RSSdata, k, m) {
  out_hat <- matrix(0, nrow = k, ncol = length(t))
  M1 <- IntRSS1_i(t, RSSdata)
  M2 <- Fhat_i(t, RSSdata)
  for (i in 1:k) {
    out_hat[i, ] <- (M1[i, ] / m) / M2[i, ]
  }
  return(out_hat)
}
