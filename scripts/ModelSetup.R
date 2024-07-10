# Setting the seed for reproducibility
set.seed(0124)

# Splitting the data (70/30 split, stratify on energy)
Lana_split <- initial_split(Lana_data, prop = 0.7, strata = energy)
Lana_train <- training(Lana_split)
Lana_test <- testing(Lana_split)

# Creating recipe
Lana_recipe <- recipe(energy ~loudness + acousticness + instrumentalness + 
                        tempo + speechiness + liveness + valence + 
                        danceability, data = Lana_train) %>% 
  # normalizing
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())
Lana_recipe

#create folds
Lana_folds <- vfold_cv(Lana_train, v = 10, strata = energy)
