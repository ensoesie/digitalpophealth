#' Return the gender estimate for a name
#'
#' Uses the gender package, with US Social Security Administration Data, to estimate gender based on a first name
#' @param name A character value containing a name
#' @keywords gender, text
#' @return gend_value A string value containing the estimated gender of a name
#' @export
#' @examples 
#' gender_detect("Jessica")

gender_detect<-function(name, method="ssa"){
  gend<-gender(as.character(name), method=method)
  if(dim(gend)[1]==0){
    gend_value<-NA
  }
  else{
    gend_value<-gend$gender
  }
  return(gend_value)
}