#' Detect upper case letters
#'
#' Is the letter upper case?
#' @param vect A single letter
#' @keywords text
#' @return result A TRUE/FALSE value designating whether the letter is upper case
#' @export
#' @examples 
#' isUpperMethod1("H")
#' @seealso This method originally proposed by StackOverflow user topchef

isUpperMethod1<-function(s) {
  return (all(grepl("[[:upper:]]", strsplit(s, "")[[1]])))
}