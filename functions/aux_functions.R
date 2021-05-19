#defining various steps required for the genetic algorithm

library(glm)

initial_population <- function(size, n_features){
  population <- vector('list', size)
  
  random_seed <- round(runif(1), 1)
  
  for(individual in seq(size)){
    chromossome <- sample(c(TRUE, FALSE), 
                          n_features, 
                          replace = TRUE, 
                          prob = c(1 - random_seed, random_seed))
    population[[individual]] <- chromossome
  }
  
  population
}

fitness_score <- function(population, training, test){
  scores <- vector('double', length(population))
  
  for(chromossome in seq(length(population))){
    if(sum(population[[chromossome]]) == 0)
      next
    
    glm_fit <- caret::train(
      form = label ~ .,
      data = training[,c(population[[chromossome]], TRUE)],
      #trControl = trainControl(method = "cv", number = 5),
      method = "glm",
      family = "binomial"
    )
    
    predictions <- predict(glm_fit, test[, c(population[[chromossome]], FALSE)])
    accuracy <- sum(predictions == test$label) / length(predictions)
    
    scores[chromossome] <- accuracy
  }
  
  return(list(score = scores, pop = population))
}
    
selection <- function(population_after_fit, n_parents){
  population_next_gen <- vector('list', n_parents)
  
  for(i in seq(n_parents)){
    population_next_gen[i] <- population_after_fit[i]
  }
  
  population_next_gen
}

crossover <-function(pop_after_sel):
      population_nextgen=pop_after_sel
    for i in range(len(pop_after_sel)):
      child=pop_after_sel[i]
    child[3:7]=pop_after_sel[(i+1)%len(pop_after_sel)][3:7]
    population_nextgen.append(child)
    return population_nextgen
    
    def mutation(pop_after_cross,mutation_rate):
      population_nextgen = []
    for i in range(0,len(pop_after_cross)):
      chromosome = pop_after_cross[i]
    for j in range(len(chromosome)):
      if random.random() < mutation_rate:
      chromosome[j]= not chromosome[j]
    population_nextgen.append(chromosome)
    #print(population_nextgen)
    return population_nextgen
    
    def generations(size,n_feat,n_parents,mutation_rate,n_gen,X_train,
                    X_test, y_train, y_test):
      best_chromo= []
    best_score= []
    population_nextgen=initilization_of_population(size,n_feat)
    for i in range(n_gen):
      scores, pop_after_fit = fitness_score(population_nextgen)
    print(scores[:2])
    pop_after_sel = selection(pop_after_fit,n_parents)
    pop_after_cross = crossover(pop_after_sel)
    population_nextgen = mutation(pop_after_cross,mutation_rate)
    best_chromo.append(pop_after_fit[0])
    best_score.append(scores[0])
    return best_chromo,best_score