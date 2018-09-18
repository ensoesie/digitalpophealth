#' Predict gender with first names, US Social Security Administration Data 
#'
#' This function does the following 1.) Take input data (as a data frame) 2.) Parse first names from user names 3.) match these user names to US SSA data 
#' @param input A data frame containing Twitter profile information. Created using the function get_user_info
#' @param ncores Number of cores across which the process is distributed. Note: Windows users cannot request more than two.
#' @param chunk If TRUE, features and predictions are generated for 10,000 user chunks. Recommended for unlabeled data with >10,000 users.
#' @param action If chunk==TRUE, whether the data should be appended and returned as one file, or saved as chunks. Options are append and save.
#' @param dir Where should output files be saved? Used only if action==save. Defaults to NULL
#' @keywords twitter, gender, demography
#' @return A two-column data frame containing Twitter user IDs and gender predictions.
#' @export
#' @examples 
#' ## df_list from function get_user_info
#' ## df <- do.call("rbind", df_list)
#' #' gender_method_1(df, 5, chunk=TRUE, action="save", dir="/home/User/workingDir")


gender_method_1<-function(input, ncores, chunk=FALSE, action="append", dir=NULL){

      df<-input
      
      if(tail(unlist(strsplit(dir, "")), 1) !="/"){
        dir <- paste0(dir, "/")
      }

      if(is.null(df$name) | is.null(df$id_str)){
        stop("Your input data is missing relevant fields(s)")
      }
      
      if(chunk==FALSE){
        df$name<-as.character(df$name)
        df$name<-tolower(df$name)
        df$name<-str_replace_all(df$name, "[[:punct:]]", "")
        df$name<-unlist(lapply(df$name, function(x) iconv(x, "UTF-8", "ASCII", sub=""))) #these two lines remove unicode
        df$name<-unlist(lapply(df$name, function(x) gsub("<.*>", "", x)))
        
        df$name_new<-unlist(lapply(df$name, function(x) convert_names(x)))
        
        df$name_new<-trim(df$name_new)
        print("Names are prepared!")
        
        df$gender_estimate_R <- unlist(mclapply(X=df$name_new, FUN=gender_detect, mc.cores = ncores))
        print("predictions are done!")
        
        results <- data.frame(id_str =as.character(df$id_str), gender_prediction=df$gender_estimate_R)

        return(results)
      }

      if(chunk==TRUE & action=="append"){
        
        results<- NULL
        seq <- seq(1, length(df$name), by=10000)
        iter <- 0
        
        for(i in seq){
          sub <- df[c(i:(i+9999)),]
          iter <- iter+1
          
          if(length(which(is.na(sub$name))>0)){
            sub <- sub[-which(is.na(sub$name)),]
          }
          
          sub$name<-as.character(sub$name)
          sub$name<-str_replace_all(sub$name, "[[:punct:]]", "")
          sub$name<-unlist(lapply(sub$name, function(x) iconv(x, "UTF-8", "ASCII", sub=""))) #these two lines remove unicode
          sub$name<-unlist(lapply(sub$name, function(x) gsub("<.*>", "", x)))

          sub$name_new<-unlist(lapply(sub$name, function(x) convert_names(x)))
          
          sub$name_new<-trim(sub$name_new)
          print(paste0("Names for chunk ", iter, " are prepared!"))
          
          sub$gender_estimate_R <- unlist(mclapply(X=sub$name_new, FUN=gender_detect, mc.cores = ncores))
          print(paste0("Predictions for chunk ", iter, " are prepared!"))
          
          results_part<-data.frame(id_str =as.character(sub$id_str), gender_prediction=sub$gender_estimate_R)

          results <- rbind(results, results_part)
        }
         return(results)
      }

      if(chunk==TRUE & action=="save"){
        
        results<- NULL
        seq <- seq(1, length(df$name), by=10000)
        iter <- 0
        
        for(i in seq){
          sub <- df[c(i:(i+9999)),]
          iter <- iter+1
          
          if(length(which(is.na(sub$name))>0)){
            sub <- sub[-which(is.na(sub$name)),]
          }
          
          sub$name<-as.character(sub$name)
          sub$name<-str_replace_all(sub$name, "[[:punct:]]", "")
          sub$name<-unlist(lapply(sub$name, function(x) iconv(x, "UTF-8", "ASCII", sub=""))) #these two lines remove unicode
          sub$name<-unlist(lapply(sub$name, function(x) gsub("<.*>", "", x)))
          
          sub$name_new<-unlist(lapply(sub$name, function(x) convert_names(x)))
          
          sub$name_new<-trim(sub$name_new)
          print(paste0("Names for chunk ", iter, " are prepared!"))
          
          sub$gender_estimate_R <- unlist(mclapply(X=sub$name_new, FUN=gender_detect, mc.cores = ncores))
          print(paste0("Predictions for chunk ", iter, " are prepared!"))
          
          results_part<-data.frame(id_str =as.character(sub$id_str), gender_prediction=sub$gender_estimate_R)


          write.csv(results_part, paste0(dir, "results_part_", iter, ".csv"), row.names=FALSE)
          print(paste0("Saved chunk ", iter, "!"))
        }
      }     
}