#' Creates Barplot of Successes in Binomial Experiment
#'
#' @param iter,n,p Data Input
#' @param binx Function
#'
#' @return Barplot
#'
#' @export
#'
#' @examples
#' binx(n = 5, p = 0.5, iter = 10000)
#'
binx = function(iter = 1000, n = 10, p = 0.5){
  sam.mat = matrix(NA, nrow = n, ncol = iter, byrow = TRUE)
  succ = c()
  for (i in 1:iter){
    sam.mat[,i] = sample(c(1,0), n, replace = TRUE, prob = c(p, 1-p))
    succ[i] = sum(sam.mat[,i])
  }

  succ.tab = table(factor(succ, levels = 0:n))

  iter.lab = paste0("iter = ", iter)
  n.lab = paste0("n = ", n)
  p.lab = paste0("p = ", p)
  lab = paste(iter.lab, n.lab, p.lab, sep = ", ")
  barplot(succ.tab / (iter), col = rainbow(n+1), main = "Binomial Simulation", sub = lab, xlab = "The Number of Successes")
  succ.tab/iter
}
