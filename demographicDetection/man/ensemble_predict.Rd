% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gender_ensemble_predict.R
\name{ensemble_predict}
\alias{ensemble_predict}
\title{Ensemble gender prediction}
\usage{
ensemble_predict(input)
}
\arguments{
\item{input}{A four column data frame that contains user ID values, predictions from gender_method_1, predictions from gender_method_2, and predictions from gender_method_3.}
}
\value{
A data frame containing your input data and the final predictions.
}
\description{
This function builds a logistic regression classifier that weights gender predictions across three methods and uses this model to predict a final gender output.
}
\examples{
## Organize results 
method_1_results <- method_1_results[order(method_1_results$id_str),]
method_2_results <- method_2_results[order(method_2_results$id_str),]
method_3_results <- method_3_results[order(method_3_results$id_str),]
input <- cbind(method_1_results, method_2_results[,-1], method_3_results[,-1])
results <- ensemble_predict(input)
}
\keyword{demography}
\keyword{gender,}
\keyword{twitter,}
