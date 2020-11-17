#' Create Normal Probability Distribution
#'
#' @param a,mu,sigma Data Input
#' @param myncurve Function
#'
#' @return Distribution
#'
#' @export
#'
#' @example
#' data = 1:20
#' meandata = mean(data)
#' sddata = sd(data)
#' myncurve(a = data, mu = meandata, sigma = sddata)
myncurve = function(a, mu, sigma){
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3*sigma, mu + 3*sigma))
  polygon(c(-10000, seq(-10000, a, length = 1000000), a), c(0, dnorm(seq(-10000, a, length = 1000000), mean = mu, sd = sigma), 0), col = "Light Blue")
  area = pnorm(a, mu, sigma)
  area = round(area, 4)
  text(a+2, dnorm(3, mu, sigma), paste0("Area = ", area))
}
