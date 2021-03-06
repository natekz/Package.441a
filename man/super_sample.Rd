% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/super_sample.R
\name{super_sample}
\alias{super_sample}
\title{Simulated Random Samples}
\usage{
super_sample(
  dist,
  n,
  k = 1,
  mu = NULL,
  sd = NULL,
  var = NULL,
  rate = NULL,
  trials = NULL,
  prob = NULL,
  min = NULL,
  max = NULL
)
}
\arguments{
\item{dist}{theoretical distribution, "normal", "exp", "binom" or "unif"}

\item{n}{number in each sample}

\item{k}{number of samples}

\item{mu}{mean for normal distribution}

\item{sd}{standard deviation for normal distribution}

\item{var}{variance}

\item{rate}{rate for exponential distribution}

\item{trials}{number of trials for binomial}

\item{prob}{probability of success for binomial}

\item{min}{minimum value for uniform}

\item{max}{maximum value for uniform}
}
\value{
returns k samples of n size from a given theoretical distribution
}
\description{
Simulated Random Samples
}
\examples{
super_sample(dist = "unif", 50,min = 0, max =1)
}
