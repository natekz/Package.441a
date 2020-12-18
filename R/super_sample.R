#' Simulated Random Samples
#'
#' @param dist theoretical distribution, "normal", "exp", "binom" or "unif"
#' @param n number in each sample
#' @param k number of samples
#' @param mu mean for normal distribution
#' @param sd standard deviation for normal distribution
#' @param var variance
#' @param rate rate for exponential distribution
#' @param trials number of trials for binomial
#' @param prob probability of success for binomial
#' @param min minimum value for uniform
#' @param max maximum value for uniform
#'
#' @return returns k samples of n size from a given theoretical distribution
#' @export
#'
#' @examples super_sample(dist = "unif", 50,min = 0, max =1)
super_sample = function(dist, n, k=1, mu = NULL, sd = NULL, var = NULL, rate = NULL, trials = NULL, prob = NULL, min = NULL, max = NULL){
  if (dist == "normal"){
    return(replicate(k, rnorm(n, mu, sd)))
  }else if (dist == "exp"){
    return(replicate(k, rexp(n, rate)))
  }else if (dist == "binom"){
    return(replicate(k, rbinom(n, trials, prob)))
  }else{
    return(replicate(k, runif(n, min, max)))
  }
}
