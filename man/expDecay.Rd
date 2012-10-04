\name{expDecay}
\alias{expDecay}
\title{four-parameter distance-decay function}
\usage{
  expDecay(f, fixed = NULL, starting = NULL,
    names = c("nu", "phi", "rho", "kappa"))
}
\arguments{
  \item{f}{a formula on the data to return a 2-column
  matrix}

  \item{fixed}{named vector of fixed parameters}

  \item{starting}{named vector of starting parameters}

  \item{names}{vector of length 4 of alternate variable
  names}
}
\description{
  this returns a function that when bound to data returns a
  function of i and a vector of length 4 minus number of
  fixed params
}
\details{
  note this is the:

  f = nu*exp(-(d/phi)^kappa + rho

  formula used in the FnM paper. Hence f(0)->rho+1
}

