

# 1. Nan
NA_replace <-  function(Data){

  # Data: data without label

  options(digits = 4)
  Data = as.data.frame(Data)
  #Determine any NA value
  if ( anyNA(Data) == F){

    stop("No NAN value in this data! R U SO HAPPY?")

  }

  #find num of NA

  row_allindex = apply(Data, 1, function(x) length(which(is.na(x == T ))))
  col_allindex = apply(Data, 2, function(x) length(which(is.na(x == T ))))

  row_NAindex = which(row_allindex != 0 )
  col_NAindex = which(col_allindex != 0 )

  if ( length(row_NAindex) != 0){

    NAN_index = data.frame(index  = row_NAindex,
                           number =  row_allindex[row_NAindex],
                           proportion = c(row_allindex[row_NAindex]/length(col_allindex)))

  }

  if ( length(col_NAindex) != 0){

    NAN_feature = data.frame(feature_name = names(Data)[col_NAindex],
                             number = col_allindex[col_NAindex],
                             row.names = 1:length(col_NAindex),
                             proportion = c( col_allindex[col_NAindex]/length(row_allindex)))

  }
  #  replace(d0, is.na(d0), median(d0,na.rm = T) )
  # replace by median
  data_replace = apply(Data, 2, function(x) replace(x, is.na(x), median(x,na.rm = T) ))

  NAN_feature
  return(list(data_replace = data.frame(data_replace),
              NAN_index = NAN_index,
              NAN_feature = NAN_feature))
}

NA_replace_train_test <-  function(data_training, data_test){

  # data without label
  # data_training:
  # data_test

  options(digits = 4)
  data_training = as.data.frame(data_training)
  data_test     = as.data.frame(data_test)
  data = rbind(data_training,data_test)

  #Determine any NA value
  if ( anyNA(data_training) == F){

    stop("No NAN value in this data! R U SO HAPPY?")

  }

  #find num of NA

  row_allindex_train = apply(data_training, 1, function(x) length(which(is.na(x == T ))))
  col_allindex_train = apply(data_training, 2, function(x) length(which(is.na(x == T ))))

  row_allindex  = apply(data, 1, function(x) length(which(is.na(x == T ))))
  col_allindex  = apply(data, 2, function(x) length(which(is.na(x == T ))))

  row_NAindex = which(row_allindex != 0 )
  col_NAindex = which(col_allindex != 0 )


  if ( length(row_NAindex) != 0){

    NAN_index = data.frame(index  = row_NAindex,
                           number =  row_allindex[row_NAindex],
                           proportion = c(row_allindex[row_NAindex]/length(col_allindex)))

  }

  if ( length(col_NAindex) != 0){

    NAN_feature = data.frame(feature_name = names(data)[col_NAindex],
                             number = col_allindex[col_NAindex],
                             row.names = 1:length(col_NAindex),
                             proportion = c( col_allindex[col_NAindex]/length(row_allindex)))

  }

  #  replace(d0, is.na(d0), median(d0,na.rm = T) )
  #+ replace by median
  train_test <- rbind(data_training,data_test)
  data_replace_training = apply(data_training, 2,
                                function(x) replace(x, is.na(x), median(x, na.rm = T) ))
  data_replace_train_test = apply(train_test, 2,
                                  function(x) replace(x, is.na(x), median(x[1:nrow(data_training)],na.rm = T) ))
  data_replace_test <- data_replace_train_test[-c(1:nrow(data_training)), ]

  # NAN_feature
  return(list(data_replace_training = data.frame(data_replace_training),
              data_replace_test = data.frame(data_replace_test),
              all_data_NAN_index = NAN_index,
              all_data_NAN_feature = NAN_feature))

}
