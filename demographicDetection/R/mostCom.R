#' Find most frequently occuring value
#'
#' Which value occurs most often, omitting NA values?
#' @param vect A vector of string or numeric values
#' @keywords summary statistics
#' @return result The most frequently occuring value, omitting NA values
#' @export
#' @examples 
#' mostCom(c(NA, 3, 4, 3, 5, NA))
#' @seealso Function author: Gilad Amitai

mostCom <- function(vect) {
	x <- which(table(vect)==max(table(vect)))
	if(length(x)!=1) {
	  result <- NA
		return(result)
	} else {
	  result <- names(x)
		return(result)
	}
}
