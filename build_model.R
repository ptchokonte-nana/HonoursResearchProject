#impute missing data
model_data <- data[,4:28]
model_data <- missForest(model_data, ntree=200, decreasing=TRUE)$ximp

#split data into training and test sets 
set.seed(1000)
  #70% of the data in the training set
sample_data <- initial_split(model_data, prop=0.7, strata = TB_status) 
  #create data frames for the two sets
train_data <- training(sample_data) 
test_data <- testing(sample_data)

#create recipe and roles
data_rec <- recipe(TB_status ~., data = train_data) %>%
    #keep the variables listed but do not use them as ither outcomes or predictors
  update_role(enrolment_age, new_role = "ID")


#fit a model with a recipe and tune hyperparameters
rf_mod <- decision_tree(cost_complexity = tune(),
                        tree_depth = tune()) %>%
    #engine to fit model
  set_engine("rpart") %>% 
    #predicts the type of outcome: qualitative
  set_mode("classification") 

rf_grid <- grid_regular(cost_complexity(), 
                          tree_depth(),
                          levels = 5)

folds <- vfold_cv(train_data)

  #model workflow
rf_workflow <- workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(data_rec)

rf_workflow
  #tuning results
rf_res <- rf_workflow %>%
  tune_grid(resamples = folds, 
            grid = rf_grid)
  #visualize the best top 5 results
best_tree <- rf_res %>% select_best("accuracy")

final_workflow <- rf_workflow %>% finalize_workflow(best_tree)

rf_fit <- final_workflow %>% 
    #this function fits the finalized model on the full training data set and 
    #evaluates the finalized model on the testing data.
  last_fit(sample_data)

final_tree <- extract_workflow(rf_fit)


