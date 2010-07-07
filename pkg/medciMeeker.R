medciMeeker <-
function(mu.x,mu.y,se.x,se.y,rho=0,alpha=.05,root.eps=1e-8,...){
##############################################################################
## This function computes (1-alpha)% equitail confidence interval for product
## of two random normal variables. The arguments for the functions are: a: mean
## for the first variable, b: mean for the second variable
## sea: standard deviation for the first variable, seb: standard deviation for
## the second variable, rho: correlation between the two variables,
## alpha: significance level, and root.eps: precision for bisection
## root finding algorithm.
##############################################################################
  ##defining the upper and lower limits of the support of integral for bisection
  ## rootfinding algorithm
    alpha <- alpha/2
    q.l <- qprodnormal(p=alpha, mu.x=mu.x, mu.y=mu.y, se.x=se.x, se.y=se.y, rho=rho, lower.tail=TRUE, root.eps=root.eps)
    q.u <- qprodnormal(p=alpha, mu.x=mu.x, mu.y=mu.y, se.x=se.x, se.y=se.y, rho=rho, lower.tail=FALSE, root.eps=root.eps)
    CI <- c(q.l,q.u) #confidence interval
    names(CI) <- c(paste((alpha*100),"%"),paste((1-alpha)*100,"%"))
    return(CI)
}

