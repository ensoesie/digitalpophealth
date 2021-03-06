% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gender_method_1_function.R
\name{gender_method_1}
\alias{gender_method_1}
\title{Predict gender with first names, US Social Security Administration Data}
\usage{
gender_method_1(input, ncores, chunk = FALSE, action = "append",
  dir = NULL)
}
\arguments{
\item{input}{A data frame containing Twitter profile information. Created using the function get_user_info}

\item{ncores}{Number of cores across which the process is distributed. Note: Windows users cannot request more than two.}

\item{chunk}{If TRUE, features and predictions are generated for 10,000 user chunks. Recommended for unlabeled data with >10,000 users.}

\item{action}{If chunk==TRUE, whether the data should be appended and returned as one file, or saved as chunks. Options are append and save.}

\item{dir}{Where should output files be saved? Used only if action==save. Defaults to NULL}
}
\value{
A two-column data frame containing Twitter user IDs and gender predictions.
}
\description{
This function does the following 1.) Take input data (as a data frame) 2.) Parse first names from user names 3.) match these user names to US SSA data
}
\examples{
## df_list from function get_user_info
## df <- do.call("rbind", df_list)
#' gender_method_1(df, 5, chunk=TRUE, action="save", dir="/home/User/workingDir")
}
\keyword{demography}
\keyword{gender,}
\keyword{twitter,}
