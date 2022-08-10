data_prep <- function(file) {
  my_data <- read.csv(my_file, header = TRUE, sep = ",") %>% 
  janitor::clean_names(.) %>%
  rename(., hiv_status = hi_vstatus) %>% #did not name hiv_status properly
  unique()

#remove columns
  data <- my_data[,-1] #barcode 
  data <- data[,-7] #timepoint_id

#remove "possible" from data 
  my_list <- c() #empty list
  for (i in 1:nrow(data)) {
    if (data[i,5]=="Possible"){
      my_list <- append(my_list, i) #list of row numbers
    }
  }
  data <- data[-my_list, ] #remove the rows

#TB_status: positive or negative
  for (j in 1:nrow(data)) {
    if (data[j,5]=="NoTB" || data[j,5]=="Non-TB"){
      data[j,5] <- "Negative"
    }
    else {
      data[j,5] <- "Positive"  
    }
  }
  data <- rename(data, TB_status = final_class)
  
#change data type to factor
  column_names <- colnames(data[-4])
  column_name <- column_names[2:5]
  for (i in 1:4) {
    data <- data %>% mutate_at(column_name[i], as.factor)
  }

  return(data)

}


