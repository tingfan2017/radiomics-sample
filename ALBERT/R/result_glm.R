

# glm result
result_glm <- function(model,train_data,test_data,level_train,level_test,
                   threshold){

  # model: from glm function
  # no level column in train_data or test_data
  library(pROC)
  options(digits = 3)
  train_data <- as.data.frame(train_data)
  test_data  <- as.data.frame(test_data)
  level_train <- as.factor(level_train)
  level_test  <- as.factor(level_test)
  # - outcome:predict/auc
  pred_train <- predict(model, train_data, type = "response")
  pred_test  <- predict(model, test_data,  type = "response")
  roc_train  <- roc(level_train,pred_train)
  roc_test   <- roc(level_test ,pred_test)
  auc_train  <- roc_train$auc
  auc_test   <- roc_test$auc
  # threshold/sen/spe
  c_train <- coords(roc_train, x = 'best')
  if(missing(threshold))
    threshold <- c_train[1]
  c_test  <- coords(roc_test , x = threshold)

  T_train <- table( Actual  = level_train,
                    Predict = ifelse( pred_train  < threshold,0,1))
  T_test  <- table( Actual  = level_test,
                    Predict = ifelse( pred_test   < threshold,0,1))

  result_df <- data.frame(Train_set = c(roc_train$auc[1],
                                        sum(diag(T_train))/sum(T_train),
                                        T_train[2,2]/(T_train[2,1]+T_train[2,2]),
                                        T_train[1,1]/(T_train[1,1]+T_train[1,2]),
                                        threshold,
                                        length(model$coefficients)-1),
                          Test_set = c(roc_test$auc[1],
                                       sum(diag(T_test))/sum(T_test),
                                       T_test[2,2]/(T_test[2,1]+T_test[2,2]),
                                       T_test[1,1]/(T_test[1,1]+T_test[1,2]),
                                       threshold,
                                       length(model$coefficients)-1),
                          row.names = c("AUC",
                                        "acc",
                                        "sensitivity",
                                        "specificity",
                                        "threshold",
                                        "feature num"))
  Confusion_Matrix_train <- T_train
  Confusion_Matrix_test  <- T_test
  out <- list(predict_train = pred_train,
              predict_test  = pred_test,
              CM_Train = T_train,
              CM_Test  = T_test,
              More_information = result_df)

  return(out)
}
