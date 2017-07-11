rfOutliers <- function(formula, data, t, ...){
  #' Remove outliers based on a randomForest model
  #'
  #' @description The algorithm remove outliers based on the random forest's oulier score.
  #'
  #' @template xyParam
  #' @param formula a formula
  #' @param data a data frame with the input data
  #' @param t the maximum tolerated outlier score 
  #'
  #' @return a data frame without the outliers.
  
  require(randomForest)
  data <- as.data.frame(data)
  rf.mdl <- randomForest(as.formula(formula), data= data, proximity=T, ...)
  data <- data[which(outlier(rf.mdl)<=t), ]
  return(data)
}