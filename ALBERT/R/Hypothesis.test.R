# Author: Tingfan WU, Albert


## Parameter Description:

# Data_complete: Complete data set, including none level/ID/none-usefull column
# label_index: Number of column in which the label is located
# label: 2 levels

# Data0: dataframe,data with continuous features
# label: numerical value, level in data, 0/1





Hypothesis.test <-  function(Data_complete, label_index  ){

  label = as.factor(Data_complete[, label_index])
  Data0 = Data_complete[, c(1:label_index) ]

  # only value only in each label
  data0 = Data0[which(label == "0"), ]
  data1 = Data0[which(label == "1"), ]

  data0_no_onevalue = data0[, which(diag( var(data0) ) != 0) ]
  data1_no_onevalue = data1[, which(diag( var(data1) ) != 0) ]

  fs0 = names(data0_no_onevalue)
  fs1 = names(data1_no_onevalue)
  same_names =  intersect(fs0, fs1)  # jiao ji to t test
  same_diff_names = union(fs0, fs1)  # bing ji to MWU test

  diff_names_index = which(same_diff_names %in% same_names == FALSE)


  Data = Data0[same_names]

  # length(same_names)

  # Normality test
  ps0  = apply(Data[which(label == "0"), ], 2,
               function(x)  shapiro.test(   x  )$p.value)
  ps1  = apply(Data[which(label == "1"), ], 2,
               function(x)  shapiro.test(   x  )$p.value)
  w01  = which(ps0 >= 0.05 & ps1 >= 0.05)  # both 0/1


  Data_Nor = Data[ w01]    # get Normality Data

  # Homogeneity of variance test
  pb  = apply(Data_Nor, 2, function(x) bartlett.test(x,label)$p.value)

  Data_Homo   = Data_Nor[which( pb >= 0.05)]

  Data_inHomo = Data_Nor[which( pb <  0.05)]

  # T test
  # T test 1 : Equality of variance
  t_Equa = apply(Data_Homo, 2,
                 function(x) t.test(x = x[which(label == "0")],
                                    y = x[which(label == "1")],
                                    alternative = "two.sided",
                                    var.equal = T)$p.value)

  # T test 2 : inEquality of variance
  t_inEqua = apply(Data_inHomo, 2,
                   function(x) t.test(x = x[which(label == "0")],
                                      y = x[which(label == "1")],
                                      alternative = "two.sided",
                                      var.equal = F)$p.value)

  # oucome
  Data_TTest = cbind(Data_Homo[which(t_Equa <= 0.05)],
                     Data_inHomo[which(t_inEqua <= 0.05)])

  if ( length(w01) == 0){
    # MW U test
    Data_inNor = Data
    pw = apply(Data_inNor, 2,
               function(x) wilcox.test(x[which(label == "0")],
                                       x[which(label == "1")],
                                       alternative = "two.sided")$p.value)
    Data_UTest = Data_inNor[which(pw <= 0.05)]
    selection_infor = data.frame(Test = c("Normality",
                                          "Homogeneity",
                                          "TTest",
                                          "UTest",
                                          "T&U"),
                                 Number = c(0,
                                            0,
                                            0,
                                            ncol(Data_UTest),
                                            ncol(Data_TTest)+ncol(Data_UTest)))
    Pvalue_infor = data.frame(ftName = c(names(Data_UTest)),
                              Pvalue = c(pw[which(pw <= 0.05)]),
                              row.names = 1:ncol(Data_UTest) )

  }


  if ( length(w01) != 0 ){
    # MW U test
    Data_inNor0 = Data[ -w01]

    if( length(diff_names_index) != 0 ){
      same_names1 = union(names(Data_inNor0), names(Data0[diff_names_index]) )  # jiaoji
    }else{
      same_names1 = names(Data_inNor0)   # jiaoji
    }

    Data_inNor = Data[same_names1]
    pw = apply(Data_inNor, 2,
               function(x) wilcox.test(x[which(label == "0")],
                                       x[which(label == "1")],
                                       alternative = "two.sided")$p.value)
    Data_UTest = Data_inNor[which(pw <= 0.05)]
    selection_infor = data.frame(Test = c("Normality",
                                          "Homogeneity",
                                          "TTest",
                                          "UTest",
                                          "T&U"),
                                 Number = c(ncol(Data_Homo),
                                            ncol(Data_Homo),
                                            ncol(Data_TTest),
                                            ncol(Data_UTest),
                                            ncol(Data_TTest)+ncol(Data_UTest)))

    Pvalue_infor = data.frame(ftName = c(names(Data_TTest), names(Data_UTest)),
                              Pvalue = c(t_Equa[which(t_Equa <= 0.05)],
                                         t_inEqua[which(t_inEqua <= 0.05)],
                                         pw[which(pw <= 0.05)]),
                              row.names = 1:(ncol(Data_TTest) + ncol(Data_UTest)) )

  }


  Data_FS = cbind(Data_TTest,Data_UTest)

  return(list(Data_Feature_Selection = Data_FS,
              P_value_information = Pvalue_infor,
              selection_information = selection_infor))
}

