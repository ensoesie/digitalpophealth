#' Ensemble gender prediction
#'
#' This function builds a logistic regression classifier that weights gender predictions across three methods and uses this model to predict a final gender output.
#' @param input A four column data frame that contains user ID values, predictions from gender_method_1, predictions from gender_method_2, and predictions from gender_method_3.
#' @keywords twitter, gender, demography
#' @return A data frame containing your input data and the final predictions.
#' @export
#' @examples
#' ## Organize results
#' method_1_results <- method_1_results[order(method_1_results$id_str),]
#' method_2_results <- method_2_results[order(method_2_results$id_str),]
#' method_3_results <- method_3_results[order(method_3_results$id_str),]
#' input <- cbind(method_1_results, method_2_results[,-1], method_3_results[,-1])
#' results <- ensemble_predict(input)


ensemble_predict<-function(input){

    #load("DATA/gender_ensemble_mod_data.rda")

    ground_truth$gender_new_2<-ground_truth$gender_new
    ground_truth$gender_new_2[ground_truth$gender_new=="female"]<-1
    ground_truth$gender_new_2[ground_truth$gender_new=="male"]<-0

    ground_truth$method_1_results_2<-ground_truth$method_1_results
    ground_truth$method_1_results_2[ground_truth$method_1_results=="female"]<-1
    ground_truth$method_1_results_2[ground_truth$method_1_results=="male"]<-0

    ground_truth$method_2_results_2<-ground_truth$method_2_results
    ground_truth$method_2_results_2[ground_truth$method_2_results=="female"]<-1
    ground_truth$method_2_results_2[ground_truth$method_2_results=="male"]<-0

    ground_truth$method_3_results_2<-ground_truth$method_3_results
    ground_truth$method_3_results_2[ground_truth$method_3_results=="female"]<-1
    ground_truth$method_3_results_2[ground_truth$method_3_results=="male"]<-0



    mod1<-glm(as.factor(gender_new_2)~method_1_results_2+method_2_results_2+method_3_results_2, data=ground_truth[which(!is.na(ground_truth$method_1_results)),], family="binomial")
    mod2<-glm(as.factor(gender_new_2)~method_2_results_2+method_3_results_2, data=ground_truth[which(is.na(ground_truth$method_1_results)),], family="binomial")


    names(input) <- c("id_str", "method_1_results", "method_2_results", "method_3_results")
    input$gender_new_2 <- NA

    input$method_1_results_2<-input$method_1_results
    input$method_1_results_2[input$method_1_results=="female"]<-1
    input$method_1_results_2[input$method_1_results=="male"]<-0

    input$method_2_results_2<-input$method_2_results
    input$method_2_results_2[input$method_2_results=="female"]<-1
    input$method_2_results_2[input$method_2_results=="male"]<-0

    input$method_3_results_2<-input$method_3_results
    input$method_3_results_2[input$method_3_results=="female"]<-1
    input$method_3_results_2[input$method_3_results=="male"]<-0


    predictions1 <- predict(mod1, input[which(!is.na(input$method_1_results)),])
    prob1 <- exp(predictions1)/(1+exp(predictions1))
    predictions1[prob1>0.50]<-"female"
    predictions1[prob1<0.50]<-"male"


    predictions2 <- predict(mod2, input[which(is.na(input$method_1_results)),])
    prob2 <- exp(predictions2)/(1+exp(predictions2))
    predictions2[prob2>0.50]<-"female"
    predictions2[prob2<0.50]<-"male"

    predictions_all<-c(predictions1, predictions2)

    ids<-c(input$id_str[which(!is.na(input$method_1_results))], input$id_str[which(is.na(input$method_1_results))])

    true_all<-data.frame(id_str = ids, gender_predictions=predictions_all)

    return(true_all)

  }

