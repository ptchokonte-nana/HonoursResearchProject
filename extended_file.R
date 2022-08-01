# import and clean (column names) the data ---------------------------------

import_data <- function(my_file){
  my_data <- read.csv(my_file, header = TRUE, sep = ",") %>% 
    janitor::clean_names(.) %>%
    rename(., hiv_status = hi_vstatus)
    return(my_data)
}


# rename data type from character to factor --------------------------------

type_data <- function(column_names){
  column_name <- column_names[4:6]
  for (i in 1:3) {
    my_data <- my_data %>% mutate_at(column_name[i], as.factor)
    return(str(my_data))
  }
}


# split data into training and test set -----------------------------------

split_data <- function(data){
  sample_data <- sample(2, nrow(my_data), replace = T, prob = c(0.7,0.3))
  train <- my_data[sample_data==1, ]
  test <- my_data[sample_data==2, ]
  return(train, test)
}


