# Specify model engine tuning trees, learn_rate (the learning rate)
boosted_model <- boost_tree(mtry = tune(),
                           trees = tune(),
                           learn_rate = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

#set up workflow
boosted_workflow <- workflow() %>% 
  add_recipe(Lana_recipe) %>% 
  add_model(boosted_model)

#create grid
boosted_grid <- grid_regular(mtry(range = c(1, 5)), 
                             trees(range = c(5, 400)),
                             learn_rate(range = c(-0.1, 0)),
                             levels = 5)
#fit model
boosted_tune <- tune_grid(
  boosted_workflow,
  resamples = Lana_folds,
  grid = boosted_grid
)

#save and load in file
save(boosted_tune, file = "tuned_models/boosted.rda")
load("tuned_models/boosted.rda")

#autoplot for visualization
autoplot(boosted_tune)

#collect metrics
collect_metrics(boosted_tune)

#select best model
best_boosted <- select_best(boosted_tune, metric = "rmse")

#model selection
boosted_final <- finalize_workflow(boosted_workflow, best_boosted)
boosted_final_fit<-fit(boosted_final, Lana_train)

#calculate performance
augment(boosted_final_fit, new_data = Lana_test) %>%
  rmse(truth = energy, estimate = .pred)
