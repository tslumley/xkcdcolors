\name{xcolors}
\alias{xcolors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
List the XKCD survey colors
}
\description{
Lists (a subset of) the 949 colors consistently identified in the XKCD web color survey.
}
\usage{
xcolors(max_rank = -1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{max_rank}{
If positive, restrict to the top (most frequently named) \code{max_rank} colors.
}
}
\value{
Vector of character strings
}
\references{
\url{http://blog.xkcd.com/2010/05/03/color-survey-results/}}


\examples{
xcolors(10)
name2color(xcolors(10))
barplot(rep(1,10),col=name2color(xcolors(10)))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}
