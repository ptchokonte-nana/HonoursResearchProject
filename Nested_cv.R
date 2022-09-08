set.seed(1000)
stratified <- stratified(model_data, c('study_site','TB_status','sex','hiv_status'), 0.7, keep.rownames=TRUE)
sample_data <- initial_split(model_data, prop=0.7, strata = TB_status)
sample_data$in_id <- as.integer(stratified$rn)
train_data <- training(sample_data) 
test_data <- testing(sample_data)

train_pred_data <- train_data[,4:27]
set.seed(500)
nested_folds <- nested_cv(train_pred_data,
                   outside = vfold_cv(repeats = 1, strata = TB_status), 
                   inside = vfold_cv(repeats = 5, strata = TB_status))

log_reg <- function(object, penalty = 1) {
  y_col <- ncol(object$data)
  mod <- 
    logistic_reg(mode="classification", penalty=penalty) %>%
    set_engine("glmnet") %>%
    fit(TB_status ~., data = analysis(object))
  
  holdout_pred <- 
    predict(mod, assessment(object)) %>% 
    bind_cols(predict(mod, assessment(object), type = "prob")) %>%
    bind_cols(assessment(object) %>% select(TB_status))
  roc_auc(holdout_pred, truth=TB_status, estimate=.pred_Negative)$.estimate
}

wrapper <- function(penalty, object) log_reg(object, penalty)
#For the nested resampling, a model needs to be fit for each tuning parameter
#and each bootstrap split. To do this, create a wrapper
tune_over_penalty <- function(object) {
  tree_grid <- grid_regular(penalty(), levels = 5)
  tree_grid %>%
    mutate(Accuracy = map_dbl(tree_grid$penalty, wrapper, object=object))
}
#Since this will be called across the set of outer cross-validation splits,
#another wrapper is required
summarize_tune_results <- function(object) {
  # Return row-bound tibble that has the 50 vfold_cv results
  map_df(object$splits, tune_over_penalty) %>%
    # For each value of the tuning parameter, compute the 
    # average accuracy which is the inner vfold_cv estimate. 
    group_by(penalty) %>%
    summarize(mean_accuracy = mean(Accuracy, na.rm = TRUE),
              n = length(Accuracy),
              .groups = "drop")
}
plan(multisession)
tuning_results <- future_map(nested_folds$inner_resamples, summarize_tune_results) 





