#' Predict gender using name word and character n-grams
#'
#' This function does the following  1.) Take input data (as a data frame) 2.) Train a classifier using Kaggle data  3.) Generates predictions from the input data and classifier
#' @param user_ids A vector of Twitter user ID values or screen names 
#' @param credential Your API authentication token
#' @param rawdata Whether you want to store the raw .json output
#' @param datadir Where you would like hte raw .json output to be stored
#' @param is_id TRUE if your input is ID values. FALSE if your input is screen names
#' @keywords twitter 
#' @return uInfo A data frame containing detailed profile metadata
#' @export
#' @examples 
#' get_user_info(df$id_str, credential=user.signature, rawdata=FALSE, datadir="", is_id=TRUE)


#############  Information from individual accounts  ###############

get_user_info <- function(user_ids, credential=NULL, rawdata=FALSE, datadir="", is_id=""){
  cat("WORKING TO GET USER DATA...")
  runs <- ceiling(length(user_ids)/100) # num of queries needed 
  start <- 1
  end <- 100
  
  # Note roauth credential
  cred <- credential
  # Note directory to store raw json
  rawData <- rawdata
  dataDir <- datadir
  is_ID <- is_id

  
  if (runs==1)
    if(length(user_ids)<end)
      end <- length(user_ids)
  
  uInfo <- vector("list", runs) # to store data
  for (r in 1:runs){
    
    # Check to see how many queries remain
    queries_left <- checkRL("users", credential=cred)
    queries_left <- queries_left$resources$users$'/users/lookup'[2]
    
    # Check to make sure we are not adding NAs at the end
    if (end > length(user_ids))
      end <- length(user_ids)
    
    if (queries_left>0){
      u <- paste(user_ids[start:end], collapse=",") # put usernames in correct format
      uInfo[[r]] <- getUserInfo(u, is.ID=is_ID, bulk=TRUE, credential=cred, rawdata=rawData, datadir=dataDir)
      
    }
    else {
      cat("...Rate limit exceeded - sleeping for 15 minutes... \n")
      Sys.sleep(900)
    }
    
    start <- end + 1  
    end <- start + 99

    cat(".")
  }
  cat("done.\n")
  return(uInfo)
}



