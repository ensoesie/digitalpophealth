#' Trim trailing and leading whitespace from a string
#'
#' Trim trailing and leading whitespace from a string
#' @param x a string value
#' @keywords text
#' @return A trimmed sentence
#' @export 
#' @examples 
#' trim(" Trim this sentence ")

trim <- function (x) gsub("^\\s+|\\s+$", "", x)