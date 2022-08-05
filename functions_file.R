# import and clean (column names) the data ---------------------------------

import_data <- function(my_file){
  my_data <- read.csv(my_file, header = TRUE, sep = ",") %>% 
    janitor::clean_names(.) %>%
    rename(., hiv_status = hi_vstatus) %>% #did not name hiv_status properly
    unique()
  return(my_data)
}


# remove unwanted columns -------------------------------------------------

remove_data <- function(data){
  new_data <- data[,-1] #barcode 
  new_data <- new_data[,-7] #timepoint_id
  return(new_data)
}



# create new column: TB_status ---------------------------------------------
# first remove possible
TB_data1 <- function(data, my_lst){
  for (i in 1:nrow(data)) {
    if (data[i,5]=="Possible"){
      my_lst <- append(my_lst, i) #list of row numbers
    }
  }
  return(my_lst)
}

#separate into positives and negatives
TB_data2 <- function(data){
  for (j in 1:nrow(data)) {
    if (data[j,5]=="NoTB" || data[j,5]=="Non-TB"){
      data[j,5] <- "Negative"
    }
    else {
      data[j,5] <- "Positive"  
    }
  }
  data <- rename(data, TB_status = final_class)
  return(data)
}


# rename data type from character to factor --------------------------------

type_data <- function(data, column_names){
  column_name <- column_names[3:5]
  for (i in 1:3) {
    data <- data %>% mutate_at(column_name[i], as.factor)
  }
  return(data)
}



# impute missing (N/A) values in data -----------------------------------------

imp_data <- function(data){
  plt_data <- data[,5:28]
  plt_data <- plt_data[-2] #remove hiv_status column
  plt_data <- missForest(plt_data, ntree=200, decreasing=TRUE)$ximp
}


# check for a value that is zero in data -----------------------------------

check_data <- function(data){
  lst <- c() #empty list
  for (k in 2:ncol(data)) {
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
            ggeasy::easy_center_title() +
            )
  }
}


# split data into training and test set -----------------------------------

split_data <- function(data){
  sample_data <- sample(data, nrow(data)) #shuffle data
  sample_data <- sample(2, nrow(sample_data), replace = T, prob = c(0.7,0.3))
  return(sample_data)
}


