#' Is it a vowel?
#'
#' Evaluates whether a letter is a vowel
#' @param char A single letter
#' @keywords text, language
#' @return A TRUE/FALSE value indicating whether the letter is a vowel
#' @export
#' @examples 
#' isVowel("d")

isVowel  <-  function(char) char %in% c('a', 'e', 'i', 'o', 'u')