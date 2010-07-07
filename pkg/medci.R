medci <-
function(mu.x,mu.y,se.x,se.y,rho=0,alpha=.05,type="all",n.mc=1e5,...)
{
    a=mu.x;b=mu.y;sea=se.x;seb=se.y
    if(!is.numeric(mu.x))
      stop("Argument mu.x must be numeric!")
    if(!is.numeric(mu.y))
      stop("Argument mu.y must be numeric!")
    if(!is.numeric(se.x))
      stop("Argument se.x must be numeric!")
    if(!is.numeric(se.y))
      stop("Argument se.y must be numeric!")
    if(!is.numeric(alpha))
      stop("Argument alpha  must be numeric!")
    if(!is.numeric(rho))
      stop("Argument rho  must be numeric!")
    if(alpha<=0 || alpha >=1)
      stop("alpha must be between 0 and 1!")
    if(rho<=-1 || rho>=1)
      stop("rho must be between -1 and 1!")
    if(!is.numeric(n.mc) || is.null(n.mc))
      n.mc=1e5 # sets n.mc to default

    if (type=="all" || type=="All" || type=="ALL")
        {
            MeekerCI=medciMeeker(mu.x, mu.y, se.x, se.y, rho, alpha)
            names(MeekerCI) <- c("Lower","Upper")
            prodclinCI=medciProdclin(mu.x, mu.y, se.x, se.y, rho, alpha)$CI
            names(prodclinCI) <- c("Lower","Upper")
            MCCI= medciMC(mu.x, mu.y, se.x, se.y, rho , alpha , error = F, plot = FALSE, n.mc = n.mc)$CI
            names(MCCI) <- names(MeekerCI) <- c("Lower","Upper")
            res <- list(MeekerCI=MeekerCI, prodclinCI=prodclinCI, MCCI=MCCI)
            names(res) <- paste(100-100*alpha,"% CI for X*Y using ", c("Meeker","PRODCLIN","Monte Carlo"),sep="")
            return(res)

         }
    else if (type=="prodclin" || type=="PRODCLIN" || type=="Prodclin")
        {
            prodclinCI=medciProdclin(mu.x, mu.y, se.x, se.y, rho, alpha)$CI
            cat((100-alpha*100),"% PRODCLIN confidence interval for X*Y : \n", sep="")
            names(prodclinCI) <- c("Lower","Upper")
            return(prodclinCI)

            }
    else if (type=="Meeker" || type=="meeker")
        {
            MeekerCI=medciMeeker(mu.x, mu.y, se.x, se.y, rho, alpha)
            cat((100-alpha*100),"% confidence interval for X*Y : \n", sep="")
            names(MeekerCI) <- c("Lower","Upper")
            return(MeekerCI)
            }
    else if (type=="MC" || type=="mc" || type=="Mc")
        {
            MCCI= medciMC(mu.x, mu.y, se.x, se.y, rho, alpha , error = F, plot = FALSE, n.mc = n.mc)$CI
            cat((100-alpha*100),"% Monte Carlo confidence interval for X*Y : \n", sep="")
            names(MCCI) <- c("Lower","Upper")
            return(MCCI)
            }
    else stop("Wrong type! please specify type=\"all\", \"Meeker\", \"prodclin\", or \"MC\" ")
}

