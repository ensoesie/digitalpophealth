#' Convert date formats
#'
#' Convert the created_at timestamp on a tweet into a usable Date object
#' @param x a string value with a time stamp formatted "%a %b %d %h:%m:%s %z %Y"
#' @keywords date, TWitter
#' @return newDate A Date object formatted "%Y-%M-%D"
#' @export
#' @examples 
#' dateConvert("Fri Jan 20 21:54:23 +0000 2012")

dateConvert<-function(date){
  dateParts<-unlist(strsplit(date, " "))
  if(dateParts[2]=="Jan"){
    mon<-"1"
  }
  if(dateParts[2]=="Feb"){
    mon<-"2"
  }
  if(dateParts[2]=="Mar"){
    mon<-"3"
  }
  if(dateParts[2]=="Apr"){
    mon<-"4"
  }
  if(dateParts[2]=="May"){
    mon<-"5"
  }
  if(dateParts[2]=="Jun"){
    mon<-"6"
  }
  if(dateParts[2]=="Jul"){
    mon<-"7"
  }
  if(dateParts[2]=="Aug"){
    mon<-"8"
  }
  if(dateParts[2]=="Sep"){
    mon<-"9"
  }
  if(dateParts[2]=="Oct"){
    mon<-"10"
  }
  if(dateParts[2]=="Nov"){
    mon<-"11"
  }
  if(dateParts[2]=="Dec"){
    mon<-"12"
  }
  year<-dateParts[6]
  day<-dateParts[3]
  newDate<-as.Date(paste(year, "-", mon, "-", day, sep=""))
  return(newDate)
}

