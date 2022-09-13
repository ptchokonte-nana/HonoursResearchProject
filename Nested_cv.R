# Split data into training and test sets ---------------------------------------
set.seed(1000)
stratified <- stratified(model_data, c('study_site','TB_status','sex','hiv_status'), 0.7, keep.rownames=TRUE)
sample_data <- initial_split(model_data, prop=0.7, strata = TB_status)
sample_data$in_id <- as.integer(stratified$rn)
train_data <- training(sample_data) 
test_data <- testing(sample_data)
# Nested cross validation ------------------------------------------------------
set.seed(500)
folds <- nested_cv(train_data,
                   outside = vfold_cv(repeats = 1, strata = TB_status), 
                   inside = vfold_cv(repeats = 5, strata = TB_status))
# function to fit and predict model --------------------------------------------
log_reg <- function(object, penalty = 1, mixture = 1) {
  y_col <- ncol(object$data)
  
  recipe <- 
    recipe(TB_status ~., data = train_data) %>% 
    update_role(study_site, sex, enrolment_age, new_role = "ID") %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_downsample(TB_status)
  mod <- 
    logistic_reg(penalty=penalty, mixture=mixture) %>%
    set_engine("glmnet") %>%
    set_mode("classification")
  
  wf <- workflow() %>% add_recipe(recipe) %>% add_model(mod)
  model_fit <- wf %>% fit(data = analysis(object))
  
  holdout_pred <- 
    predict(model_fit, assessment(object)) %>% 
    bind_cols(predict(model_fit, assessment(object), type = "prob")) %>%
    bind_cols(assessment(object) %>% select(TB_status))
  roc_auc(holdout_pred, truth=TB_status, estimate=.pred_Negative)$.estimate

}
# parameterize the function over the tuning parameter --------------------------
wrapper <- function(penalty, mixture, object) log_reg(object, penalty, mixture)
# tune model parameters --------------------------------------------------------
tune_over <- function(object) {
  tree_grid <- grid_regular(penalty(), mixture(), levels = 5)
  tree_grid %>%
    mutate(accuracy = map2_dbl(tree_grid$penalty, tree_grid$mixture, wrapper, object=object))
}
# Summary of tuning results ----------------------------------------------------
tuning<- function(object) {
  map_df(object$splits, tune_over) %>%
    group_by(penalty, mixture) %>%
    summarize(roc_auc = mean(accuracy, na.rm = TRUE),
              n = length(Accuracy),
              .groups = "drop")
}

plan(multisession, workers=8)
#tuning_results <- future_map(folds$inner_resamples, tuning) 

best_tuning <- function(dat) dat[which.max(dat$mean_accuracy),]
tuning_vals <- tuning_results %>% map_df(best_tuning) %>% select(penalty, mixture, mean_accuracy)
# Summary of training results --------------------------------------------------
training <- pmap(list(tuning_vals$penalty, tuning_vals$mixture, folds$splits), wrapper)
training_results <- tuning_vals %>% select(penalty, mixture) %>% mutate(roc_auc=training_results) %>% unnest(cols = c(roc_auc))
training_results %>% summarise(roc_auc = mean(.$roc_auc, na.rm = TRUE)) %>% mutate(data="train", model="logistic regression")





