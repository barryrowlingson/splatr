\name{powerSum}
\alias{powerSum}
\title{a powered sum covariate}
\usage{
  powerSum(f, fixed = NULL, starting = NULL,
    prefixes = c("beta", "gamma"))
}
\arguments{
  \item{f}{formula - first covariate is baseline}

  \item{fixed}{list of fixed parameter values}

  \item{starting}{starting values for optimiser}

  \item{prefixes}{names to give the parameters}
}
\value{
  a function of a data frame. Calling this creates a
  function of the parameters.
}
\description{
  a powered sum covariate
}

