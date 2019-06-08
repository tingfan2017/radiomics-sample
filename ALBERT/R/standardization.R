

#  Standardization
standardization <- function(data0, data1 = NULL, data2 = NULL,
                            type = c("minmax", "zscore")){

  # data0: training data
  # data1: validation data
  # data2: test data
  # type: standardization type(minmax or zscore)


  if ( class(data1) == "NULL" & class(data2) == "NULL") {
    if( type == "minmax" ){

      max_col = apply(data0, 2, function(x) max(x))
      min_col = apply(data0, 2, function(x) min(x))
      max_min = max_col - min_col

      sub_col0 = sweep(data0, 2, min_col, "-")
      mer_col0 = sweep(sub_col0, 2, max_min, "/")

      data_result = list(data0 = mer_col0)
    }
    if( type == "zscore" ){

      mean_col = apply(data0, 2, function(x) mean(x))
      sd_col   = apply(data0, 2, function(x) sd(x))

      sub_col0 = sweep(data0, 2, mean_col, "-")
      mer_col0 = sweep(sub_col0, 2, sd_col, "/")

      data_result = list(data0 = mer_col0)

    }
  }

  if ( class(data1) != "NULL" & class(data2) == "NULL" ) {
    if( type == "minmax" ){

      max_col = apply(data0, 2, function(x) max(x))
      min_col = apply(data0, 2, function(x) min(x))
      max_min = max_col - min_col

      sub_col0 = sweep(data0, 2, min_col, "-")
      sub_col1 = sweep(data1, 2, min_col, "-")

      mer_col0 = sweep(sub_col0, 2, max_min, "/")
      mer_col1 = sweep(sub_col1, 2, max_min, "/")

      data_result = list(data0 = mer_col0,
                         data1 = mer_col1)
    }
    if( type == "zscore" ){

      mean_col = apply(data0, 2, function(x) mean(x))
      sd_col   = apply(data0, 2, function(x) sd(x))

      sub_col0 = sweep(data0, 2, mean_col, "-")
      sub_col1 = sweep(data1, 2, mean_col, "-")

      mer_col0 = sweep(sub_col0, 2, sd_col, "/")
      mer_col1 = sweep(sub_col1, 2, sd_col, "/")

      data_result = list(data0 = mer_col0,
                         data1 = mer_col1)

    }
  }

  if ( class(data2) != "NULL" ){
    if( type == "minmax" ){

      max_col = apply(data0, 2, function(x) max(x))
      min_col = apply(data0, 2, function(x) min(x))
      max_min = max_col - min_col

      sub_col0 = sweep(data0, 2, min_col, "-")
      sub_col1 = sweep(data1, 2, min_col, "-")
      sub_col2 = sweep(data2, 2, min_col, "-")

      mer_col0 = sweep(sub_col0, 2, max_min, "/")
      mer_col1 = sweep(sub_col1, 2, max_min, "/")
      mer_col2 = sweep(sub_col2, 2, max_min, "/")

      data_result = list(data0 = mer_col0,
                         data1 = mer_col1,
                         data2 = mer_col2)
    }
    if( type == "zscore" ){

      mean_col = apply(data0, 2, function(x) mean(x))
      sd_col   = apply(data0, 2, function(x) sd(x))

      sub_col0 = sweep(data0, 2, mean_col, "-")
      sub_col1 = sweep(data1, 2, mean_col, "-")
      sub_col2 = sweep(data2, 2, mean_col, "-")

      mer_col0 = sweep(sub_col0, 2, sd_col, "/")
      mer_col1 = sweep(sub_col1, 2, sd_col, "/")
      mer_col2 = sweep(sub_col2, 2, sd_col, "/")

      data_result = list(data0 = mer_col0,
                         data1 = mer_col1,
                         data2 = mer_col2)

    }
  }


  return(data_result)

}
