# Specify model engine tuning cost_complexity
prune_model <- decision_tree(cost_complexity = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

#set up workflow
prune_workflow <- workflow() %>% 
  add_recipe(Lana_recipe) %>% 
  add_model(prune_model)

#create grid
prune_grid <- grid_regular(cost_complexity(range=c(-3, -1)),
                             levels = 10)
#fit model
prune_tune <- tune_grid(
  prune_workflow,
  resamples = Lana_folds,
  grid = prune_grid
)

#save and load in file
save(boosted_tune, file = "tuned_models/prune.rda")
load("tuned_models/prune.rda")

#autoplot for visualization
autoplot(prune_tune)

#collect metrics
collect_metrics(prune_tune)

#select best model
best_prune <- select_best(prune_tune, metric='rmse')

#model selection
prune_final <- finalize_workflow(prune_workflow, best_prune)
prune_final_fit<-fit(prune_final, Lana_train)

#extract model fit results
prune_final_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)

#calculate performance
augment(prune_final_fit, new_data = Lana_test) %>%
  rmse(truth = energy, estimate = .pred)