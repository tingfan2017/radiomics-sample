

# reduce_redundency
reduce_redundency <- function (dat, threshold = 0.9, method = "spearman")
{

  if (!("data.frame" %in% class(dat))) {
    stop("Input data must be class data.frame")
  }
  if (sum(is.na(dat)) != 0) {
    stop("Input data contain missing values")
  }
  feature_names = colnames(dat)
  types = sapply(dat, class)
  dataIndex = which(types == "numeric" | types == "integer")
  despIndex = which(types != "numeric" & types != "integer")
  data_numeric = dat[, dataIndex]
  if (length(despIndex) > 0) {
    desp = dat[, despIndex]
  } else {
    desp = NULL
  }
  cor = cor(data_numeric, method = "spearman")
  cor[upper.tri(cor)] = 0
  diag(cor) = 0
  dat.redd = data_numeric[, !apply(cor, 2, function(x) any(abs(x) >
                                                             threshold))]
  features_selected = colnames(dat.redd)
  if (length(despIndex) > 0) {
    dat.redd = data.frame(desp, dat.redd)
  }
  return(list(names = features_selected, dat.redd = dat.redd))
}
