#' Count consonants in a string
#'
#' Count the number of consonants in a string
#' @param name A string value
#' @keywords text, language
#' @return results A numeric value representing the number of consonants
#' @export
#' @examples 
#' consonant_count("Jim")

consonant_count <- function(name){
      x  <-  tolower(strsplit(name, "")[[1]])
      x  <-  x[x %in% letters]
      a_count <- length(which(names(table(x))=="a"))
      e_count <- length(which(names(table(x))=="e")) 
      i_count <- length(which(names(table(x))=="i"))
      o_count <- length(which(names(table(x))=="o"))
      u_count <- length(which(names(table(x))=="u")) 
      vowel_count <- sum(a_count, e_count, i_count, o_count, u_count)
      consonant_count <- sum(table(x))-vowel_count
      return(consonant_count)
    }