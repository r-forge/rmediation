medciMC <-
function(mu.x,mu.y,se.x,se.y,rho=0,alpha=.05, error=FALSE, plot=FALSE, n.mc=1e7){
    mean.v <- c(mu.x,mu.y)
    var.mat <- matrix(c(se.x^2,se.x*se.y*rho,se.x*se.y*rho,se.y^2),2)
    a_b <- matrix(rnorm(2*n.mc),ncol=n.mc)
    a_b <- crossprod(chol(var.mat),a_b)+mean.v
    a_b <- t(a_b)
    ab <- a_b[,1]*a_b[,2]
    if (plot==TRUE)
      plot(density(ab))
    CI <- (quantile(ab,c(alpha/2,1-alpha/2)))
    if (error==TRUE){
        x <- ab[which(ab> CI[1] & ab<CI[2])]
        mean.x <- sum(x)/n.mc
        error.mean.x <- sqrt(sum((x-mean.x)^2))/n.mc
        }
    else error.mean.x=NA
    return(list(CI=CI, error=error.mean.x))
}

