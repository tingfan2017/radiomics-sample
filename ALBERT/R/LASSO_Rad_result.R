

LASSO_Rad_result <-  function(lasso_model,
                              train_data,  test_data,
                              level_train, level_test,
                              threshold  ){

  # lasso_model: come from cv.glmnet

  cv_x = train_data
  coefPara = coef(lasso_model,s="lambda.min")
  lasso_values = as.data.frame( which(coefPara !=0,arr.ind = T) )
  lasso_values_spm = rownames(lasso_values)[-1]
  model_coef = data.frame(feature = rownames(lasso_values),
                          coef = coefPara[which(coefPara != 0,arr.ind = T)])
  train_glm_lasso = data.frame(cv_x)[lasso_values_spm]

  # normalization
  train_set <- train_glm_lasso
  test_set  <- test_data[ names(train_set) ]

  # # test_set <- data.frame(scale(test_data[ names(train_set) ]))
  #
  # for (m in 1:ncol(test_set) ) {
  #   test_set[,m] <- (test_set[,m] - mean(train_set[,m]))/sd(train_set[,m])
  # }
  #
  # train_set <- data.frame(scale(train_glm_lasso))
  Data  = data.frame(rbind(train_set, test_set))

  xn = nrow(Data)     # row
  yn = ncol(Data)     # column


  beta           = as.matrix(  coefPara[ which(coefPara != 0), ] )  # get beta = Coefficients
  betai_Matrix   = as.matrix( beta[-1] )      # get betai
  beta0_Matrix   = matrix(beta[1], xn, 1 )    # get beta0
  Rad_Matrix     = as.matrix( Data )          # get X
  Radcore_Matrix = Rad_Matrix %*% betai_Matrix + beta0_Matrix  # get Radscore
  radscore_all   = as.numeric(Radcore_Matrix)
  Radscore_train = radscore_all[ 1:nrow(train_set) ]    # train rads
  Radscore_test  = radscore_all[ (nrow(train_set)+1) : xn ] # test rads

  pred_train  = exp(Radscore_train)/(1+exp(Radscore_train))
  pred_test   = exp(Radscore_test) /(1+exp(Radscore_test))

  roc_train = roc(train_label, pred_train)
  roc_test  = roc(test_label,  pred_test)

  auc_train = roc_train$auc[1]
  auc_test  = roc_test$auc[1]

  # threshold/sen/spe
  c_train = coords(roc_train,x = 'best')
  if(missing(threshold))
    threshold = c_train[1]
  c_test  = coords(roc_test ,x = threshold)

  T_train = table( Actual  = level_train,
                   Predict = ifelse( pred_train  < threshold, 0, 1) )
  T_test  = table( Actual  = level_test,
                   Predict = ifelse( pred_test   < threshold, 0, 1) )

  result_df = data.frame(Train_set = c(auc_train,
                                       sum(diag(T_train))/sum(T_train),
                                       T_train[2,2]/(T_train[2,1]+T_train[2,2]),
                                       T_train[1,1]/(T_train[1,1]+T_train[1,2]),
                                       threshold,
                                       length(lasso_values_spm)),
                         Test_set = c(auc_test,
                                      sum(diag(T_test))/sum(T_test),
                                      T_test[2,2]/(T_test[2,1]+T_test[2,2]),
                                      T_test[1,1]/(T_test[1,1]+T_test[1,2]),
                                      threshold,
                                      length(lasso_values_spm)),
                         row.names = c("AUC",
                                       "Accuracy",
                                       "Sensitivity",
                                       "Specificity",
                                       "Threshold",
                                       "Feature Num"))
  return(list(train_set = train_set,
              test_set = test_set,
              model_coef = model_coef,
              Radscore_train = Radscore_train,
              Radscore_test  = Radscore_test,
              pred_train = pred_train,
              pred_test  = pred_test,
              T_train = T_train,
              T_test  = T_test,
              feature_name = lasso_values_spm,
              result_df = result_df))

}
