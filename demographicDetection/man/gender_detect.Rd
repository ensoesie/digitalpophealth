% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gender_detect.R
\name{gender_detect}
\alias{gender_detect}
\title{Return the gender estimate for a name}
\usage{
gender_detect(name, method = "ssa")
}
\arguments{
\item{name}{A character value containing a name}
}
\value{
gend_value A string value containing the estimated gender of a name
}
\description{
Uses the gender package, with US Social Security Administration Data, to estimate gender based on a first name
}
\examples{
gender_detect("Jessica")
}
\keyword{gender,}
\keyword{text}
