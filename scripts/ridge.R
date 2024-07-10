# Specify model engine tuning penalty and setting mixture to 0 to specify ridge
ridge_model <- linear_reg(mixture = 0, 
                         penalty = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")

#set up workflow
ridge_workflow <- workflow() %>% 
  add_recipe(Lana_recipe) %>% 
  add_model(ridge_model)

#create grid
ridge_grid <- grid_regular(penalty(range = c(0, 1),
                                     trans = identity_trans()),
                             levels = 10)

#fit model
ridge_tune <- tune_grid(
  ridge_workflow,
  resamples = Lana_folds,
  grid = ridge_grid
)

#save and load in file
save(rf_tune, file = "tuned_models.ridge.rda")
load("tuned_models.ridge.rda")

#autoplot for visualization
autoplot(ridge_tune)

# collect metrics
collect_metrics(ridge_tune)

# select best model
best_ridge <- select_best(ridge_tune,
                          metric='rmse',
                          penalty,
                          mixture)
best_ridge

# model selection
ridge_final <- finalize_workflow(ridge_workflow, best_ridge)
ridge_final_fit <-fit(ridge_final, data=Lana_train)

# calculate performance
augment(final_ridge_fit, new_data=Lana_test) %>% 
  rmse(truth=energy, estimate=.pred)
