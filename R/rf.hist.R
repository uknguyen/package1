#' Create Relative Frequency Histogram
#'
#' @param v,bins,main Data Input
#' @param rf.hist Function
#'
#' @return Histogram
#'
#' @export
#'
#' @example
#' data = 1:20
#' rf.hist(v = data, bins = 8)
rf.hist = function(v, bins = 9, main = "Relative Frequency Histogram"){
  left.stop = min(v) - 0.05
  right.stop = max(v) + 0.05
  range = right.stop - left.stop
  delta = range / bins
  s = seq(left.stop, right.stop, by = delta)
  cuts = cut(v, breaks = s)
  tab = table(cuts)
  barplot(tab/sum(tab), space = 0, main = main, las = 2, ylab = "Relative Frequency")
}
