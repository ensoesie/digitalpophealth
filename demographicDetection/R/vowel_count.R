#' Count vowels in a string
#'
#' Count the number of vowels in a string
#' @param name A string value
#' @keywords text, language
#' @return vowel_count A numeric value representing the number of vowels
#' @export
#' @examples 
#' vowel_count("Jim")

vowel_count <- function(name){
  x  <-  tolower(strsplit(name, "")[[1]])
  x  <-  x[x %in% letters]
  a_count <- length(which(names(table(x))=="a"))
  e_count <- length(which(names(table(x))=="e")) 
  i_count <- length(which(names(table(x))=="i"))
  o_count <- length(which(names(table(x))=="o"))
  u_count <- length(which(names(table(x))=="u")) 
  vowel_count <- sum(a_count, e_count, i_count, o_count, u_count)
 return(vowel_count)
}
      

