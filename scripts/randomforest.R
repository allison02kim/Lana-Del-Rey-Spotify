# Specify model engine tuning mtry, trees, min_n, and stting importance='impurity'
rf_model <- rand_forest(mtry = tune(), 
                       trees = tune(), 
                       min_n = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")

#set up workflow
rf_workflow <- workflow() %>% 
  add_recipe(Lana_recipe) %>% 
  add_model(rf_model)

#create grid
rf_grid <- grid_regular(mtry(range = c(1, 6)), 
                        trees(range = c(200,600)), 
                        min_n(range = c(10,20)), 
                        levels = 5)

#fit model
rf_tune <- tune_grid(
  rf_workflow,
  resamples = Lana_folds,
  grid = rf_grid
)

#save and load in file
save(rf_tune, file = "tuned_models.ridge.rda")
load("tuned_models.ridge.rda")

#autoplot for visualization
autoplot(rf_tune)

#select best model
best_rf <- show_best(rf_tune, n = 1, metric="rmse")

#model selection
rf_final <- finalize_workflow(rf_workflow, best_rf)
rf_final_fit <- fit(rf_final, Lana_train)

#calculate performance
augment(rf_final_fit, new_data = Lana_test) %>%
  rmse(truth = energy, estimate = .pred)