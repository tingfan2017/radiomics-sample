

# 2. train_partition
train_partition <- function (Data, proportion){

  # please set the seed first!
  # label + data

  label = Data[,1]
  label_name = names(Data[1])
  data  = Data[-1]

  label   = as.factor( label )
  classes = lapply(levels(label), function(x) which(label == x))
  classes_index   = lapply(classes,  function(class) sample(class,
                                                            proportion * length(class),
                                                            replace = F))
  train_index = unlist(classes_index)
  test_index  = (1:nrow(data))[-train_index]

  Train_set = data[train_index, ]
  Test_set     = data[test_index,  ]

  train_label = as.factor(label[train_index])
  test_label     = as.factor(label[test_index])

  if(length(table(train_label)) == 2 ){

    cs <- data.frame(SUM = c(nrow(data),
                             length(train_label),
                             length(test_label)),
                     num_0  = c(length(which( label == 0)),
                                length(which(train_label == 0)),
                                length(which(test_label  == 0))),
                     num_1  = c(length(which( label == 1)),
                                length(which(train_label == 1)),
                                length(which(test_label  == 1))),

                     row.names = c("Data ","Train ","Test ") )
  }

  if(length(table(train_label)) == 3 ){

    cs <- data.frame(SUM = c(nrow(data),
                             length(train_label),
                             length(test_label)),
                     num_0  = c(length(which( label == 0)),
                                length(which(train_label == 0)),
                                length(which(test_label  == 0))),
                     num_1  = c(length(which( label == 1)),
                                length(which(train_label == 1)),
                                length(which(test_label  == 1))),
                     num_2  = c(length(which( label == 2)),
                                length(which(train_label == 2)),
                                length(which(test_label  == 2))),

                     row.names = c("Data ","Train ","Test ") )
  }

  out_come_list = list(Train_set = Train_set,
                       Test_set = Test_set,
                       Train_index = train_index,
                       Test_index  = test_index,
                       Train_label = train_label,
                       Test_label = test_label,
                       Data_classification_matrix = cs,
                       proportion = prop.table(table(train_label)))


  return(out_come_list)

}
