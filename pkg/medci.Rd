\name{medci}
\alias{medci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ produces confidence interval for the product of
  two random normal variables.
}
\description{
   produces confidence interval for the product of
  two random normal variables.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
medci(mu.x, mu.y, se.x, se.y, rho = 0, alpha = 0.05, type = "all", n.mc = 1e+05, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu.x}{ mean
%%     ~~Describe \code{a} here~~
}
  \item{mu.y}{ mean
%%     ~~Describe \code{b} here~~
}
  \item{se.x}{ stadard deviation
%%     ~~Describe \code{sea} here~~
}
  \item{se.y}{ standard deviation
%%     ~~Describe \code{seb} here~~
}
  \item{rho}{=0.
%%     ~~Describe \code{rho} here~~
}
  \item{alpha}{=.05
%%     ~~Describe \code{alpha} here~~
}
  \item{type}{="MC"
%%     ~~Describe \code{type} here~~
}
  \item{n.mc}{ When type="MC", n.mc determines the sample size of Monte Carlo method.
%%     ~~Describe \code{n.mc} here~~
}
  \item{\dots}{ optional
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  return a vector of lower bound and upper bound for the confidence interval.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Davood Tofighi
}
\note{
further notes
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%objects to See Also as \code{\link{help}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
## The function is currently defined as
medci(mu.x=.2,mu.y=.4,se.x=.1,se.y=.05,rho=0,alpha=.05,type="all")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ }
\keyword{  }% __ONLY ONE__ keyword per line
