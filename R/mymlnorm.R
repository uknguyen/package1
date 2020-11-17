#' mymlNorm
#'
#' @param x a vector, the trials
#' @param mu an integer, the mean
#' @param sig an integer, the standard deviation
#'
#' @return graph which will show the MLE of population mean and standard deviation
#'
#' @export
#'
#' @example
#' data = 1:20
#' meandata = mean(data)
#' sddata = sd(data)
#' mymlnorm(x = data, mu = meandata, sig = sddata)
mymlnorm=function(x,mu,sig,...){
  nmu=length(mu)
  nsig=length(sig)
  n=length(x)
  zz=c()
  lfun=function(x,m,p) log(dnorm(x,mean=m,sd=p))   # log lik for normal
  for(j in 1:nsig){
    z=outer(x,mu,lfun,p=sig[j])
    y=apply(z,2,sum)
    zz=cbind(zz,y)
  }
  maxl=max(exp(zz))
  coord=which(exp(zz)==maxl,arr.ind=TRUE)
  maxlsig=apply(zz,1,max)
  contour(mu,sig,exp(zz),las=3,xlab=expression(mu),ylab=expression(sigma),axes=TRUE,
          main=expression(paste("L(",mu,",",sigma,")",sep="")),...)
  mlx=round(mean(x),2)  # theoretical
  mly=round(sqrt((n-1)/n)*sd(x),2)
  #axis(1,at=c(0:20,mlx),labels=sort(c(0:20,mlx)))
  #axis(2,at=c(0:20,mly),labels=TRUE)
  abline(v=mean(x),lwd=2,col="Green")
  abline(h=sqrt((n-1)/n)*sd(x),lwd=2,col="Red")

  # Now find the estimates from the co-ords
  muest=mu[coord[1]]
  sigest=sig[coord[2]]

  abline(v=muest, h=sigest)
  return(list(x=x,coord=coord,maxl=maxl))
}
