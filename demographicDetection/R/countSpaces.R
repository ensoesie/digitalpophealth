#' Count spaces in a string
#'
#' Count the number of spaces in a string
#' @param s A string value or vector or strings
#' @keywords text, language
#' @return results A numeric value representing the number of spaces
#' @export
#' @examples 
#' countSpaces("Check how many spaces are in this sentence.")
#' @seealso This solution first posted on StackOverflow by user MvG

countSpaces <- function(s) { sapply(gregexpr(" ", s), function(p) { sum(p>=0) } ) }
