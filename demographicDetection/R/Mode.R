#' Mode
#'
#' Find modal value, excluding NA values
#' @param x A vector of string or numeric values
#' @keywords summary statistics
#' @return The most frequently occuring value
#' @export
#' @examples 
#' Mode(c(NA, 3, 4, 3, 5, NA))
#' @seealso Originally posted on StackOverflow by user Ken Williams on November 18, 2011

Mode <- function(x) {
  ux <- unique(x)
  if(length(unique(tabulate(match(x, ux))))==1 & length(x)!=1){
    mode<-NA
  }
  else{
    mode<-ux[which.max(tabulate(match(x, ux)))]
  }
  return(mode)
}