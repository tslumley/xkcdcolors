\name{nearest_named}
\alias{nearest_named}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find nearest named color to a specified color.
}
\description{
Looks up the nearest (sufficiently popular) named color to the specified color
}
\usage{
nearest_named(color, hex_only = FALSE, max_rank = -1, Lab=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{color}{
Vector of character strings or matrix of RGB values
}
  \item{hex_only}{
if \code{TRUE} return only the hex string, otherwise all the data
}
  \item{max_rank}{
Consider only the \code{max_rank} most popular colors.
}
\item{Lab}{
 Use the 'Lab' color space if \code{TRUE}, otherwise RGB
}
}

\value{
A hex string with the color, or a data frame
}
\references{
\url{http://blog.xkcd.com/2010/05/03/color-survey-results/}}

\examples{
nearest_named("#8f7303")
nearest_named("#8f7303",max_rank=100)
nearest_named("#8f7303",max_rank=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}

