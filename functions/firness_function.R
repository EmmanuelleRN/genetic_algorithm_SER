# Auxiliary functions to be passed to the GA function -----------------------

# Fitness score 
fitness_score <- function(vars, features, target, sampling_prob = NA, metrics = 'ROC'){
  
  if(!is.na(sampling_prob)){
    sample_index <- funModeling::get_sample(features, percentage_tr_rows = sampling_prob)
    features <- features[sample_index,]
    target <- target[sample_index,]
  }
  
  # keep variables that were selected by the GA algorithm
  names <- colnames(features[vars == 1])
  features_used <- features[, names]
  
  # training and test set using 80/20 split
  index <- sample(seq(1, nrow(features_used)), replace = FALSE, size = round(nrow(features_used) * 0.8))
  training <- features_used[index,] %>%
    bind_cols(target[index,])
  test <- features_used[-index,] %>%
    bind_cols(target[-index,])
  
  
  # get the roc value from the created model
  roc_value <- get_performance_metrics(training, test, metrics)
  
  # the fitness value will be the highest ROC value
  roc_value / sum(vars)
}

get_performance_metrics <- function(training, test, metrics) {
  # Cross-Validation
  fitControl <- caret::trainControl(method = "cv", 
                                    number = 10, 
                                    summaryFunction = twoClassSummary,
                                    classProbs = TRUE)
  
  mtry <-  sqrt(ncol(data_model))
  tunegrid <-  expand.grid(.mtry=round(mtry))
  
  rf_model <-  caret::train(label ~ .,
                                data = training,
                                method = "rf",
                                tuneGrid = tunegrid,
                                trControl = fitControl,
                                metric = metrics)
  
  predictions <- predict(rf_model, test)
  
  sum(predictions == test$label) / length(predictions)
}

custom_fitness <- function(vars, data_x, data_y, p_sampling){
  ix = get_sample(data_x, percentage_tr_rows = p_sampling)
  data_x = data_x[ix,]
  data_y=data_y[ix,]
  # keep only vars from current solution
  names <- colnames(data_x)
  names <- names[vars == 1]
  # get the columns of the current solution
  df = data_x[, names]
  
  if(length(unique(names)) == 1)
    next
  
  # get the roc value from the created model
  roc_value = get_roc_metric(df, data_y, names)
  
  # get the total number of vars for the current selection
  q_vars = sum(vars)
  
  # time for your magic
  fitness_value = roc_value/q_vars
  
  return(fitness_value)
}

get_roc_metric <- function(data_tr_sample, target, best_vars) {
  # data_tr_sample=data_sol
  # target = target_var_s
  # best_vars=names_2
  
  fitControl <- trainControl(method = "cv", 
                             number = 3, 
                             summaryFunction = twoClassSummary,
                             classProbs = TRUE)
  
  data_model <- data_tr_sample %>%
    bind_cols(target) 
  
  levels(data_model$label) <- c('B', 'M')
  
  mtry = sqrt(ncol(data_model))
  tunegrid = expand.grid(.mtry=round(mtry))
  
  fit_model_1 = train(label ~ .,
                      data = data_model,
                      method = "rf", 
                      trControl = fitControl,
                      metric = "ROC"#,
                      #tuneGrid=tunegrid
  )
  
  metric = fit_model_1$results["ROC"][1,1]
  
  return(metric)
}


get_accuracy_metric <- function(data_tr_sample, target, best_vars) {
  data_model <- data_tr_sample %>%
    select(all_of(best_vars)) %>%
    bind_cols(target) 
  
  fitControl <- trainControl(method = "cv", 
                             number = 3, 
                             summaryFunction = twoClassSummary)
  
  mtry = sqrt(ncol(data_model))
  tunegrid = expand.grid(mtry=round(mtry))
  
  fit_model_1 = train(label ~ .,
                      data = data_model,
                      method = "rf",
                      tuneGrid = tunegrid)
  
  
  
  metric=fit_model_1$results["Accuracy"][1,1]
  return(metric)
}  
