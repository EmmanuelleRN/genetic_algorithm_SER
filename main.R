library(GA)
library(magrittr)
library(tictoc)
library(caret)
library(randomForest)
library(funModeling)
library(tidyverse)

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
ga_GA_1 = ga(fitness = function(vars) fitness_score(vars = vars, 
                                                    features =  cancer_features, 
                                                    target = cancer_label, 
                                                    sampling_prob = 0.7), # custom fitness function
             type = "binary", # optimization data type
             crossover = gabin_uCrossover,  # cross-over method
             elitism = 3, # best N indiv. to pass to next iteration
             pmutation = 0.03, # mutation rate prob
             popSize = 50, # the number of indivduals/solutions
             nBits = param_nBits, # total number of variables
             names = col_names, # variable name
             run=5, # max iter without improvement (stopping criteria)
             maxiter = 50, # total runs or generations
             monitor = TRUE, # plot the result at each iteration
             keepBest = TRUE, # keep the best solution at the end
             parallel = TRUE, # allow parallel procesing
             seed = 8888 # for reproducibility purposes
)
toc()
# Checking the results
summary(ga_GA_1)

# Following line will return the variable names of the final and best solution
best_vars_ga <- col_names[ga_GA_1@solution[1,]==1]

# Checking the variables of the best solution...
best_vars_ga

# Checking the accuracy
get_performance_metrics(cancer_features, target = cancer_label, best_vars_ga)
