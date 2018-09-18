#' Vowel or consonant as last character
#'
#' This function evaluates characters and termines whether the last term is a vowel or consonant.
#' @param input A character value
#' @keywords character, language
#' @return A value of "consonant" or "vowel"
#' @export
#' @examples 
#' end_char("jim")


end_char <- function(name){
      if(name=="" | is.na(name)){
        return(NA)
      }
      char <- tail(unlist(strsplit(name, "")),1)
      if(isVowel(char)==TRUE){
        return("vowel")
      }
      if(isVowel(char)==FALSE){
        return("consonant")
      }
      else{
        return(NA)
      }
}