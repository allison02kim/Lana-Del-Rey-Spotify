# Specify model engine tuning neighbors
knn_model <- nearest_neighbor(neighbors = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("kknn")

#set up workflow
knn_workflow <- workflow() %>% 
  add_model(knn_model) %>% 
  add_recipe(Lana_recipe)

#create grid
knn_grid <- grid_regular(neighbors(range = c(1,10)), levels = 10)


#fit model
knn_tune <- tune_grid(
  knn_workflow,
  resamples = Lana_folds,
  grid = knn_grid
)

#save and load in file
save(knn_tune, file = "tuned_models.knn.rda")
load("tuned_models.knn.rda")

#autoplot for visualization
autoplot(knn_tune)

#collect metrics
collect_metrics(knn_tune)

#select best model
show_best(knn_tune, metric = "rmse")
best_knn <- select_by_one_std_err(knn_tune, desc(neighbors), metric = "rmse")

#model selection
knn_final <- finalize_workflow(knn_workflow, best_knn)
knn_final_fit<-fit(knn_final, Lana_train)

#calculate performance
augment(knn_final_fit, new_data = Lana_test) %>%
  rmse(truth = energy, estimate = .pred)