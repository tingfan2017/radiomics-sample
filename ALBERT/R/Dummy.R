

    Dummy <- function(data){
  # data need to change

  library(caret)
  dmy = dummyVars(formula = " ~ .", data = data)
  data_trsf = data.frame(predict(dmy, newdata = data))

  out = list(data_trsf = data_trsf)

  return(out)

}



