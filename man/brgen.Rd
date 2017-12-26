\name{brgen}
\alias{brgen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
generate bernoulli distribution
}
\description{
This function get a float number between 0 and 1, then generate a bernoulli distribution.
This function use cugen function. This function generate 0 if the number that cugen generated is bigger than p, Otherwise it generate 1.
}
\usage{
myvar <- brgen(0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
The probability for bernoulli distribution.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
This function return 0 or 1.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
HamidReza Zare (hzare@ce.sharif.ir)
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with %\section{Warning }{....} ~

\seealso{
See Also as \code{\link{cugen}%}, ~~~
}
