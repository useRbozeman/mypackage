\name{m_hast}
\alias{m_hast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Metropolis Hastings Function
}
\description{
A generic function to do Metropolis Hastings sampling
}
\usage{
m_hast(n.iter, theta.start, t.fun, t.arg, j.s.fun, j.s.arg, j.d.fun, 
                   j.d.arg1, j.d.arg2, center)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n.iter}{Number of Iterations}
  \item{theta.start}{Starting Value for Center of Jumping Distribution}
  \item{t.fun}{Target Distribution}
  \item{t.arg}{Arguments for Target Function}
  \item{j.s.fun}{Sampling Function for Jumping Distribution}
  \item{j.s.arg}{Arguments for Jumping Sampler}
  \item{j.d.fun}{Jumping Distribution Density Function}
  \item{j.d.arg1}{1st Set of Jumping Dist. arguments}
  \item{j.d.arg2}{2nd Set of Jumping Dist. arguements}
  \item{center}{Name of Jumping Distribution's Center (i.e. mu)}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{theta.vec }{Vector of samples from target function}
\item{r.vals }{Vector of 'r' values from each iteration}
\item{efficiency}{Percent of 'accepted' samples}
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
\code{\link{l_met}}
}
\examples{
tuning <- 3.1
targs <- list(8)
names(targs) <- "lambda"
jsarg <- list(tuning, 8)
names(jsarg) <- c("size", "mu")
jdarg1 <- list(tuning, 8)
names(jdarg1) <- c("size", "mu")
jdarg2 <- list(tuning, 8)
names(jdarg2) <- c("size", "mu")

mhast1 <- m_hast(n.iter = 10000, theta.start = 8, t.fun = "dpois", t.arg = list(8), 
                 j.s.fun = "rnbinom", j.s.arg = jsarg, j.d.fun = "dnbinom", 
                 j.d.arg1 = jdarg1, j.d.arg2 = jdarg2, center = "mu")

mhast1$efficiency

plot(as.numeric(names(table(mhast1$theta))), as.numeric(table(mhast1$theta)/10000), type = "h", xlim = c(0, 23))
points(as.numeric(names(table(mhast1$theta))), as.numeric(table(mhast1$theta)/10000),
       col = "#2ca25f", pch = 20, cex = 1 )
points(1:23, dpois(1:23, 8), pch = 3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
