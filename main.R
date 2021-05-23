library(GA)
library(tictoc)
library(caret)
library(randomForest)
library(funModeling)
library(dplyr)
library(janitor)

source('functions/fitness_function.R')

cancer_df <- readr::read_csv(file = 'data/cancer.csv') %>%
  janitor::clean_names() %>%
  dplyr::mutate(label = as.factor(ifelse(label == 0, 'B', 'M')))

cancer_features <- cancer_df %>%
  dplyr::select(-label)

cancer_label <- cancer_df %>%
  dplyr::select(label)

# data_x: input data frame
# data_y: target variable (factor)
# GA parameters
param_nBits = ncol(cancer_features)
col_names = colnames(cancer_features)
# Executing the GA 
tic()
ga_feature_selection <-  ga(
  fitness = function(vars){
    fitness_score(vars = vars, 
                  features =  cancer_features, 
                  target = cancer_label, 
                  sampling_prob = 0.7) # custom fitness function
  },
  type = "binary", # optimization data type
  crossover = gabin_uCrossover,  # cross-over method
  elitism = 3, # best N individuals to pass to next generation
  pmutation = 0.3, # mutation rate prob
  popSize = 200, # the number of individuals/solutions
  nBits = param_nBits, # total number of variables
  names = col_names, # variable name
  run = 5, # max iterations without improvement (stopping criteria)
  maxiter = 50, # total runs or generations
  monitor = TRUE, # plot the result at each iteration
  keepBest = TRUE, # keep the best solution at the end
  parallel = TRUE, # allow parallel processing
  seed = 8888 # for reproducibility purposes
)
toc()
# Checking the results
summary(ga_feature_selection)

# Following line will return the variable names of the final and best solution
best_vars_ga <- col_names[ga_feature_selection@solution[1,] == 1]

# Checking the variables of the best solution...
best_vars_ga

# Checking the performance
partition <-  get_train_test_partition(cancer_features, cancer_label)
get_performance_metrics(training = partition$training[c(best_vars_ga, 'label')], 
                        test = partition$test[c(best_vars_ga, 'label')],
                        metrics = 'ROC')

get_performance_metrics(training = partition$training,
                        test = partition$test,
                        metrics = 'ROC')
