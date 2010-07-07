\name{qprodnormal}
\alias{qprodnormal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generates quantile for the distribution of product of two random normal variables
}
\description{
  Generates quantile for the distribution of product of two random normal variables
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
qprodnormal(p, mu.x, mu.y, se.x, se.y, rho, lower.tail = TRUE, root.eps = 1e-08, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{ probability
%%     ~~Describe \code{p} here~~
}
  \item{mu.x}{ mean of the first variable
%%     ~~Describe \code{a} here~~
}
  \item{mu.y}{ mean of the second variable
%%     ~~Describe \code{b} here~~
}
  \item{se.x}{ standard deviation (error) of the first variable
%%     ~~Describe \code{sea} here~~
}
  \item{se.y}{ standard deviation (error) of the second variable
%%     ~~Describe \code{seb} here~~
}
  \item{rho}{ correlation between two random normal variable. This
    correlation must be between -1 and 1.
%%     ~~Describe \code{rho} here~~
}
\item{lower.tail}{logical; if \code{TRUE} (default), the probability is
  \eqn{P[X*Y < q]}; otherwise, \eqn{P[X*Y > q]}
%%     ~~Describe \code{lower.tail} here~~
}
  \item{root.eps}{ error for finding the quantile function using
    nisection method. The default value is 1E-8.
%%     ~~Describe \code{root.eps} here~~
}
  \item{\dots}{ other optional arguments.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{ This function produces a scalar value for the quantile of the
  distribution of product of two random normal variables.
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

qprodnormal(p=.1,mu.x=.5,mu.y=.3,se.x=.03,se.y=.8,rho=0)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ }
\keyword{ }% __ONLY ONE__ keyword per line
