medciProdclin <-
    function(a, b, sea, seb, rho=0, alpha=.05) {
        library.dynam("RMediation",PACKAGE="RMediation")
 ##   dyn.load("RMediation",PACKAGE="RMediation",type="Fortran")

    ##lowz, highz, ier, abserr, last )
    ## If the library hasn't been loaded yet, load it
    ## Call the subroutine, giving n as a Fortran integer, and giving # result as an array with space for 1 integer. # A list of parameters values after the function is called # is returned, assigned to the same names as are given before the # = signs in the arguments. # NOTE: factorial_R_wrapper has its R in lower-case now. This # is because Fortran is case insensitive, so usually names are # all converted to lower-case.
    ans <- .Fortran('test_fnprod', a=as.numeric(a), b=as.numeric(b), sea=as.numeric(sea), seb=as.numeric(seb), rho=as.numeric(rho), alpha=as.numeric(alpha), lowz=numeric(1), highz=numeric(1), ier=integer(1), abserr=numeric(1), last=integer(1)) # Return the value of the result parameter
    CI <- c(ans$lowz, ans$highz)
    res <- list(CI=CI, error=ans$abserr)
    return(res)
}

