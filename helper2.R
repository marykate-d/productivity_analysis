#this is a helper file I created for use with the presentation slides. This creates, tunes, and pulls model metrics for all models tested in this project.
#it also pulls in the first helper file (helpers.R) and recreates several of the graphs used in the presentation. 
library(tidyverse)
library(tidymodels)
library(skimr)
library(recipes)
library(themis)
library(kknn)
library(glmnet)
library(discrim)
library(DALEX)
library(DALEXtra)

employee <- read_csv("file_location.csv", show_col_types = FALSE)

num <- employee %>% group_by(team) %>% summarize(n = n()) %>% arrange(n)


employee <- employee %>% 
  mutate(goal_met = ifelse(targeted_productivity <= actual_productivity, 1, 0))

employee <- employee %>% 
  select(!c("department", "date", "day", "actual_productivity")) %>% 
  mutate(wip = ifelse(is.na(wip), 0, wip)) %>% 
  mutate(quarter = as.factor(quarter),
         team = as.factor(team),
         goal_met = as.factor(goal_met))

set.seed(123)
split <- initial_split(employee, strata = goal_met)

train <- training(split)
test  <- testing(split)

folds <- vfold_cv(train, v = 10, strata = goal_met)

metrics_set <- metric_set(bal_accuracy, roc_auc)

rec <- recipe(goal_met ~ ., data = train) %>% 
  step_upsample(goal_met, over_ratio = 0.5) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_factor_predictors()) 

baked <- rec %>% 
  prep() %>% 
  bake(new_data = NULL)


nb_spec <- naive_Bayes(mode = "classification",
                       engine ="naivebayes") 

nb_wflow <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(nb_spec)

nb_fit <- nb_wflow %>% 
  fit_resamples(folds,
                metrics = metrics_set)

nb_metrics <- collect_metrics(nb_fit) %>% 
  mutate(model = "Naive Bayes Model") %>% 
  select(model, .metric, mean)

knn_spec_tune <- nearest_neighbor(
  mode = "classification",
  engine = "kknn",
  neighbors = tune()
)

knn_wf_tune <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(knn_spec_tune)

neighbor_grid <- tibble(neighbors = seq(3, 20, 1))

set.seed(123)

knn_fit_tune <- knn_wf_tune %>% 
  tune_grid(
    resamples = folds,
    grid = neighbor_grid,
    metrics = metrics_set
  )

results <- collect_metrics(knn_fit_tune)

results %>% 
  ggplot(aes(x = neighbors, y = mean)) +
  geom_line() +
  theme_minimal()

chosen_n <- knn_fit_tune %>% 
  select_best(metric = "bal_accuracy")

final_knn <- finalize_workflow(knn_wf_tune, chosen_n)

knn_fit <- final_knn %>% 
  fit_resamples(folds,
                metrics = metrics_set)

knn_metrics <- collect_metrics(knn_fit) %>% 
  mutate(model = "kNN Model") %>% 
  select(model, .metric, mean)

rf_spec <- rand_forest(mode = "classification") %>% 
  set_engine("randomForest")

rf_wflow <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(rf_spec)

rf_fit <- rf_wflow %>% 
  fit_resamples(folds,
                metrics = metrics_set)

rf_metrics <- collect_metrics(rf_fit) %>% 
  mutate(model = "Random Forest Model") %>% 
  select(model, .metric, mean)

boost_spec <- boost_tree(mode = "classification") %>% 
  set_engine("xgboost")

boost_wflow <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(boost_spec)

boost_fit <- boost_wflow %>% 
  fit_resamples(folds,
                metrics = metrics_set)

boost_metrics <- collect_metrics(boost_fit) %>% 
  mutate(model = "XGBoosted Tree Model") %>% 
  select(model, .metric, mean)

all_metrics <- bind_rows(nb_metrics, knn_metrics, rf_metrics, boost_metrics) %>% 
  arrange(desc(mean)) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>% 
  rename(Model = model,
         `ROC AUC` = roc_auc,
         `Balanced Accuracy` = bal_accuracy)

xgb_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),  
  sample_size = tune(),
  mtry = tune()             
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")  

workflow_obj <- workflow() %>%
  add_model(xgb_spec) %>%
  add_recipe(rec)

xgb_grid <- grid_latin_hypercube(
  trees(),
  tree_depth(),
  learn_rate(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train),
  size = 20
)

set.seed(123)
xgb_tuned <- tune_grid(
  workflow_obj,
  resamples = folds,
  grid = xgb_grid,
  metrics = metrics_set
)

best_params <- select_best(xgb_tuned)

final_wf <- finalize_workflow(workflow_obj, best_params)

b_final_fit <- final_wf %>% 
  fit_resamples(folds,
                metrics = metrics_set)

b_metrics <- collect_metrics(b_final_fit) %>% 
  mutate(model = "XGBoosted Model") %>% 
  select(model, .metric, mean)


rf_spec_tune <- rand_forest(mode = "classification",
                            mtry = tune(),
                            min_n = tune(), 
                            trees = tune()) %>% 
  set_engine("randomForest")

rf_wflow_tune <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(rf_spec_tune)

rf_grid <- grid_regular(
  mtry(range = c(2, 10)),
  min_n(range = c(2, 10)),
  trees(range = c(100, 1000)),
  levels = 4  
)

set.seed(123)

rf_tuned <- tune_grid(
  rf_wflow_tune,
  resamples = folds,
  grid = rf_grid,
  metrics = metrics_set
)

rf_best <- select_best(rf_tuned)

rf_final_workflow <- finalize_workflow(rf_wflow_tune, rf_best)

rf_final_fit <- rf_final_workflow %>% 
  fit_resamples(folds,
                metrics = metrics_set)

rf_metrics <- collect_metrics(rf_final_fit) %>% 
  mutate(model = "Random Forest Model") %>% 
  select(model, .metric, mean)

tuned_metrics <- bind_rows(rf_metrics, b_metrics) %>% 
  pivot_wider(names_from = .metric,
              values_from = mean)

rf_test_fit <- rf_final_workflow %>% 
  fit(test)

bal_accuracy_metric <- rf_test_fit %>% 
  augment(test) %>% 
  bal_accuracy(goal_met, estimate = .pred_class)

roc_auc_metric <- rf_test_fit %>% 
  augment(test) %>% 
  roc_auc(goal_met, .pred_0)

metrics <- bind_rows(bal_accuracy_metric, roc_auc_metric)

#code to load in helper functions below (may need to turn eval on to render?)
source("helpers.R")


explainer_rf <- make_explainer_obj(rf_test_fit)

vip_res <- model_parts(explainer_rf)

top_vars <- vip_res %>%
  filter(variable != "_baseline_", variable != "_full_model_") %>%
  group_by(variable) %>%
  summarise(mean_loss = mean(dropout_loss), .groups = "drop_last") %>%
  slice_max(mean_loss, n = 5) %>%
  pull(variable)

feature_plot <- ggplot_imp(vip_res %>% filter(variable %in% top_vars | variable == "_full_model_")) +
  theme_minimal() +
  labs(title = "Top 5 Most Important Features",
       subtitle = "Tuned Random Forest Model") +
  theme(plot.title = element_text(hjust = .5, size = 15),
        plot.subtitle = element_text(hjust = .5, size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12))

pdp_incentive <- model_profile(explainer_rf, N = 100, variables = "incentive")

incentive_plot <- ggplot_ice(pdp_incentive, incentive) +
  theme_minimal() +
  labs(
    title = "Centered Conditional ICE Plot for Incentive",
    x = "Incentive",
    y = "yhat"
  )


pdp_workers <- model_profile(explainer_rf, N = 100, variables = "no_of_workers")

worker_plot <- ggplot_ice(pdp_workers, no_of_workers) +
  theme_minimal() +
  labs(
    title = "Centered Conditional ICE Plot for Number of Workers",
    x = "Number of Workers",
    y = "yhat"
  ) 

pdp_wip <- model_profile(explainer_rf, N = 100, variables = "wip")

wip_plot <- ggplot_ice(pdp_wip, wip) +
  theme_minimal() +
  labs(
    title = "Centered Conditional ICE Plot for Work in Progress ",
    x = "Work in Progress",
    y = "yhat"
  ) 

p <- pdp_2way(rf_test_fit, no_of_workers, wip)
no_worker_2way <- p +
  labs(
    title = "2-way Partial Dependence Plot for Number of Workers and Work in Progress",
    y = "Work in Progress",
    fill = "Likelihood of Not Meeting Goal", 
    x = "Number of Workers"
  )

pdp_smv <- model_profile(explainer_rf, N = 100, variables = "smv")

smv_plot <- ggplot_ice(pdp_smv, smv) +
  theme_minimal() +
  labs(
    title = "Centered Conditional ICE Plot for Standard Minute Value ",
    x = "Standard Minute Value",
    y = "yhat"
  ) 

pdp_team10 <- model_profile(explainer_rf, N = 100, variables = "team_X10")

team_10_plot <- ggplot_ice(pdp_team10, team_X10) +
  theme_minimal() +
  labs(
    title = "Centered Conditional ICE Plot for Team 10",
    x = "Team 10",
    y = "yhat"
  ) 

