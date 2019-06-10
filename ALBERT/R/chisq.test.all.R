

chisq.test.all <- function(data, label_col_index, start_col_index){
  
  # chisq test between label & discrete feature 
  # data: csv, including label/ID/...
  # 
  
  label = data[, label_col_index]
  data_chisq = data[, start_col_index:ncol(data)]
  num_f = ncol(data_chisq)
  all_feature_p = data.frame(fname = names(data_chisq),  p_value = 1:num_f)
  for (i in 1:num_f) {
    
    table_c = table(label, data_chisq[, i])
    c_p = chisq.test(table_c )$p.value
    all_feature_p[i, 2] = c_p
  }
  
  any_feature_p = all_feature_p[which(all_feature_p[,2] >= 0.05), ]
  return(list(all_feature_p = all_feature_p,
              any_feature_p = any_feature_p))
  
}