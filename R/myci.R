#' Uses a given sample to find the Confidence Interval at a 95% confidence
#'
#' @param x vector of sample being tested
#' @param myci Function
#'
#' @return Confidence interval with 95% confidence
#'
#' @export
#'
#' @example
#' data = 1:20
#' function(x = data)
myci = function(x){
  n = length(x)
  std <- sd(x) # sample standard deviation
  t <-qt(1-0.05/2, n-1) # t multiplier
  mp <- c(-1,1) # -/+
  mean(x)+ mp*t*std/sqrt(n) # formula
}
