# impute missing (N/A) values in data -----------------------------------------

imp_data <- function(data){
  plt_data <- data[,4:28]
  plt_data <- missForest(plt_data, ntree=200, decreasing=TRUE)$ximp
}


# check for a value that is zero in data -----------------------------------

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


# plot box and whiskers graph ----------------------------------------------

graph_plt <- function(data){
  for (i in 2:ncol(data)) {
    print(ggplot(data,aes(x=TB_status, y=data[,i])) +
            geom_boxplot() +
            ggtitle(colnames(data[i])) + 
            ggeasy::easy_center_title() )
  }
}


# split data into training and test set -----------------------------------

split_data <- function(data){
  sample_data <- sample(data, nrow(data)) #shuffle data
  sample_data <- sample(2, nrow(sample_data), replace = T, prob = c(0.7,0.3))
  return(sample_data)
}


