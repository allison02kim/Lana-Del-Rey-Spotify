# Adjusting the recipe because the tuning parameter must be added in the recipe for polynomial regression
# Tuning the degree
poly_recipe <- Lana_recipe %>% 
  step_poly(loudness, acousticness, instrumentalness, tempo, 
            speechiness, liveness, valence, degree = tune())

poly_model <- linear_reg() %>% 
  set_mode("regression") %>% 
  set_engine("lm")

#specify model engine
poly_model <- linear_reg() %>% 
  set_mode("regression") %>% 
  set_engine("lm")

#set up workflow
poly_workflow <- workflow() %>% 
  add_model(poly_model) %>% 
  add_recipe(poly_recipe)

#create grid
poly_grid <- grid_regular(degree(range = c(1,7)), levels = 7)

#fit model
poly_tune <- tune_grid(
  poly_wf,
  resamples = Lana_folds,
  grid = poly_grid
)

#save and load in file
save(poly_tune, file = "tuned_models.poly.rda")
load("tuned_models.poly.rda")

#autoplot for visualization
autoplot(poly_tune)

#select best degree
best_poly <- select_best(poly_tune)

#model selection
poly_final <- finalize_workflow(poly_workflow, best_poly)
poly_final_fit <- fit(poly_final, data = Lana_train)

#calculate performance
poly_metrics <- metric_set(rmse, rsq)

augment(poly_final_fit, new_data = Lana_test) %>%
  poly_metrics(energy, .pred)