\name{l_met}
\alias{l_met}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Log-Metropolis Function
}
\description{
A function to sample from a target function (on the log scale) using the metropolis algorithm.
}
\usage{
l_met(n.iter, theta.start, t.fun, t.fun.arg, j.fun, j.fun.arg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n.iter}{Number of Iterations}
  \item{theta.start}{Starting Value}
  \item{t.fun}{Target Function}
  \item{t.fun.arg}{Arguments for Target Function}
  \item{j.fun}{Jumping Distribution Function}
  \item{j.fun.arg}{Jumping Distribution Arguments}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{theta.vec }{Vector of samples from metropolis algorithm}
\item{r.vals }{Vector of 'r' Values}
\item{efficiency }{Proportion of accepted samples}

}
\references{
BDA3 -- Gelman et. al.
}
\author{
Jordan Schupbach
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

## Data
y.vec <- c(23,24,25,26.5,27.5)

## Create Compiled Version of Log-Metropolis Function

c.l.met.func <- cmpfun(l.met.func)

## Create Unnormalized Posterior Function
l.post <- function(thetas, y = y_vec){
  return(- sum(log(1 + (y - thetas)^2)) - ((thetas - 20)^2)/10)
}

## Create Uniform Jumping Distribution
unif.j <- function(curr, a){
  runif(1, curr - a, curr + a)
}

## Create Normal Jumping Distribution
norm.j <- function(curr, a){
  rnorm(1, curr, a)
}

## Run Metropolis algorithm for both Jumping Distributions
u.met <- c.l.met.func(n.iter = 1000000, theta.start = mean(y.vec),  "l.post", 
		      t.fun.arg = list(y.vec), "unif.j", j.fun.arg = list(2.4))

u.met$efficiency

hist(u.met$theta.vec, breaks = 100, main = "Posterior Using Uniform Jumping Distribution")


n.met <- c.l.met.func(n.iter = 1000000, theta.start = mean(y.vec),  "l.post", 
		      t.fun.arg = list(y.vec), "norm.j", j.fun.arg = list(1.57))

n.met$efficiency

hist(n.met$theta.vec, breaks = 100, main = "Posterior Using Normal Jumping Distribution")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
