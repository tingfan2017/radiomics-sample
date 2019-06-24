# Radiomics score
radscore <- function(model, train_set, test_set){
  
  # model : logistic features
  # train_set:  features
  test_set1 <- test_set[names(train_set)]
  Data  <- data.frame(rbind(train_set,test_set1))
  xn <- dim(Data)[1]      # row
  yn <- dim(Data)[2]      # column
  coefPara       <- coef(model, s="lambda.min")
  beta           <- as.matrix(  coefPara[ which(coefPara !=0), ] )   # get beta = Coefficients
  betai_Matrix   <- as.matrix( beta[-1] )      # get betai
  beta0_Matrix   <- matrix(beta[1], xn, 1 )    # get beta0
  Rad_Matrix     <- as.matrix( Data )          # get X
  Radcore_Matrix <- Rad_Matrix %*% betai_Matrix + beta0_Matrix  # get Radscore
  radscore_all   <- as.numeric(Radcore_Matrix)
  Radscore_train <- radscore_all[1:dim(train_set)[1]]    # train rads
  Radscore_test  <- radscore_all[c( (dim(train_set)[1]+1) :xn)] # test rads
  
  return(list(Radscore_train = Radscore_train,
              Radscore_test  = Radscore_test))
  
  
}