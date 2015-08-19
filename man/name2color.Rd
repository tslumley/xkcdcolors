\name{name2color}
\alias{name2color}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Look up a color by name
}
\description{
Look up a color by exact or partial name.
}
\usage{
name2color(name, exact = TRUE, hex_only = TRUE, n = -1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
Character string giving color name
}
  \item{exact}{
If \code{TRUE} return only exact matches to the name, otherwise use \code{grep} to find partial matches
}
  \item{hex_only}{
If \code{TRUE} return only the color as a hex string, otherwise return the name and RGB as well
}
  \item{n}{
For partial matching, limit the results to the \code{n} highest-frequency (in the survey) colors.
}
}
\value{
A character string with a hex color code, or a data frame
}
\references{
\url{http://blog.xkcd.com/2010/05/03/color-survey-results/}}
\author{
Thomas Lumley}


\examples{
name2color("green")

name2color("green",exact=FALSE,hex_only=FALSE,n=10)

barplot(rep(1,10), col=name2color("green",exact=FALSE,n=10))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}
