# plot box and whiskers ---------------------------------------------------

plt_data <- function(data){
  data[, 7:28] <- scale(data[,7:28], center = TRUE, scale = TRUE)
  fmt_long_data <- pivot_longer(data, 
                                7:28, 
                                names_to = "Biomarkers",
                                values_to = "Observations")
  
  print(ggplot(fmt_long_data, aes(x=TB_status, y=log10(Observations))) +
          geom_boxplot() +
          facet_wrap("Biomarkers") + 
          ylab("log10"))  
  return(data)
}


# impute missing (N/A) values in data --------------------------------------

imp_data <- function(data){
  plt_data <- data[,4:28]
  plt_data <- missForest(plt_data, ntree=200, decreasing=TRUE)$ximp
}


# check for a value that is zero in the imputed data -----------------------

check_data <- function(data){
  lst <- c() #empty list
  for (k in 4:ncol(data)) {
    for (l in 1:nrow(data)) {
      if (data[l,k]==0.0){
        lst <- append(lst, c(colnames(plt_data[k]),l))
      }
    }
  }
  return(lst)  #returns the column name and row number
}

