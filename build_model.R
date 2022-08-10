# split data into training and test set -----------------------------------

split_data <- function(data){
  sample_data <- sample(data, nrow(data)) #shuffle data
  sample_data <- sample(2, nrow(sample_data), replace = T, prob = c(0.7,0.3))
  return(sample_data)
}


