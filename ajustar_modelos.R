
##  ............................................................................
##  Paquetes y funciones                                                    ####

source("_paquetes.R")

##  ............................................................................
##  Cargar                                                                  ####

ctre_f <- path("data", "ctlre", ext = "fst")
ctl <- read_fst(ctre_f) |> as_tibble() |> suppressMessages()
ctl_clean <- ctl |> select(!matches("counter$|timeout$"), -c(twr:city))
# ctl_clean <- ctl |> select(!matches("counter$|timeout$"))

##  ............................................................................
##  Split                                                                   ####

set.seed(2023)
ctl_split <- initial_validation_split(data = ctl_clean, strata = diag)
ctl_train          <- training(ctl_split)
ctl_validacion     <- validation(ctl_split)
ctl_validation_set <- validation_set(ctl_split)

##  ............................................................................
##  Cross-validation                                                        ####

set.seed(2023)
ctl_folds <- vfold_cv(ctl_train, strata = diag, v = 10)


##  Modelado                                                                ####
mset <- metric_set(precision, recall, f_meas, roc_auc)

race_ctrl <- control_race(
 save_pred     = TRUE,
 parallel_over = "everything",
 verbose       = TRUE,
 verbose_elim  = TRUE,
 save_workflow = TRUE)

grid_ctrl <- control_grid(
  save_pred     = TRUE,
  parallel_over = "everything",
  verbose       = TRUE,
  save_workflow = TRUE)



# 24 minutos
rf_spec <- rand_forest(
 mtry  = tune(),
 min_n = tune()) |>
set_engine('ranger', importance = "permutation") |>
set_mode('classification')


# 101.6


# multinom_glmnet |> extract_parameter_set_dials() |>
#   extract_parameter_dials("penalty")

# Ctrl + Alt + E

simple <- recipe(diag ~ ., data = ctl_train) |>
 update_role(user, new_role = "id") |>
 # step_dummy_hash(twr, num_terms = 16L) |>
 # step_rm(all_nominal_predictors()) |>
 # step_zv(all_numeric_predictors())  |>  # 214
 # step_nzv(all_numeric_predictors()) |>   # 199
 # step_corr(all_numeric_predictors(), threshold = 0.75) |> # 43
 # step_corr(all_numeric_predictors(), threshold = 0.6) |> # 43
 # step_select_boruta(all_predictors(), outcome = "diag") |>
 # step_orderNorm(all_numeric_predictors()) |>
 # step_center(all_predictors()) |>
 # step_scale(all_predictors()) |>
 # step_spatialsign(all_numeric_predictors()) |>
 # step_select_boruta(all_predictors(), outcome = "diag") |>
  step_select_infgain(
    all_predictors(),
    outcome = "diag",
    threshold = 0.65) |>
 step_smote(diag, skip = TRUE)

# 81 segundos
multinom_glmnet <- multinom_reg(
  penalty = tune(),
  mixture = tune()) |>
  set_engine('glmnet')

bt_lightgbm <- boost_tree(
  tree_depth = 2,
  learn_rate = 0.001,
  trees = tune(),
  min_n = 20) |>
  set_engine(engine = "lightgbm") |>
  set_mode(mode = "classification")

recetas <- list(simple = simple)
modelos <- list(lgbm = bt_lightgbm)

ctl_set <- workflow_set(
 preproc = recetas,
 models  = modelos)

ctl_set

# 1.73 mi
tic()
tune_res <- ajustar_grid(ctl_set)
toc()

(metricas_training <- tune_res |>
 rank_results(select_best = TRUE, rank_metric = "f_meas") |>
 select(modelo = wflow_id, .metric, mean, rank) |>
 pivot_wider(names_from = .metric, values_from = mean) |>
 select(modelo, f1_score_tr = f_meas, precision_tr = precision,
 recall_tr = recall))

# Selección manual de modelos
best <- metricas_training |> pull(modelo) %>% set_names(.)

lista_mejores <- best |>
 map(~ tune_res |> extract_workflow_set_result(id = .x) |>
 select_best(metric = "f_meas"))

tic()
validation_result_list <- map2(
 .x = best,
 .y = lista_mejores, ~ tune_res %>%
 extract_workflow(id = .x) %>%
 finalize_workflow(.y) %>%
 last_fit(split = ctl_validation_set$splits[[1]], metrics = mset))
toc()

metricas_validation <- validation_result_list %>%
 map_dfr(~ collect_metrics(.x), .id = "modelo") %>%
 select(-.estimator) |>
 pivot_wider(names_from = .metric, values_from = .estimate) |>
 select(modelo, f1_score_val = f_meas, precision_val = precision,
 recall_val = recall, roc_auc)

(comparacion <- metricas_training |>
 left_join(metricas_validation, join_by(modelo)) |>
 mutate(
  overfit_f1     = f1_score_tr > f1_score_val,
  overfit_recall = recall_tr > recall_val,
  overfit_precision = precision_tr > precision_val,
  sof = if_all(starts_with("overfit"), ~ .x == FALSE)) |>
 select(
  modelo,
  f1_score_tr,
  f1_score_val,
  # overfit_f1,
  recall_tr,
  recall_val,
  # overfit_recall,
  precision_tr,
  precision_val,
  sof
  # overfit_precision)
  ))

tic()
test_result_list <- map2(
  .x = best,
  .y = lista_mejores,
  ~ tune_res |>
    extract_workflow(id = .x) |>
    finalize_workflow(.y) |>
    last_fit(split = ctl_split, metrics = mset, add_validation_set = TRUE))
toc()

metricas_test <- test_result_list %>%
 map_dfr(~ collect_metrics(.x), .id = "modelo") %>%
 select(-.estimator) |>
 pivot_wider(names_from = .metric, values_from = .estimate) %>%
 select(modelo, f1_score_test = f_meas, precision_test = precision,
 recall_test = recall, roc_auc) |>
 mutate(modelo = best)

(comparacion_final <- metricas_training |>
  # left_join(metricas_validation, join_by(modelo)) |>
  inner_join(metricas_test, join_by(modelo)) |>
  mutate(
    overfit_f1     = f1_score_tr > f1_score_test,
    overfit_recall = recall_tr > recall_test,
    overfit_precision = precision_tr > precision_test) |>
  relocate(modelo,
           f1_score_tr,
           f1_score_test,
           overfit_f1,
           recall_tr,
           recall_test,
           overfit_recall,
           precision_tr,
           precision_test,
           overfit_precision) |>
  arrange(-f1_score_test))


validation_result_list[[1]] |> extract_fit_engine() |>



best_results <- tune_res %>%
  extract_workflow_set_result("simple_lgbm") %>%
  select_best(metric = "f_meas")

imp_spec <- bt_lightgbm %>%
  finalize_model(best_results) %>%
  set_engine(engine = "lightgbm") |>
  set_mode(mode = "classification")

simple <- recipe(diag ~ ., data = ctl_clean) |>
 update_role(user, new_role = "id") |>
 step_select_infgain(
  all_predictors(),
  outcome = "diag",
  threshold = 0.65) |>
 step_smote(diag, skip = TRUE)


wf <- workflow(simple, imp_spec)

ajuste <- wf |>
  finalize_workflow(lista_mejores) |>
  parsnip::fit(ctl_clean)

lgbm <- ajuste |>
  extract_fit_engine() |>
  lgb.importance(percentage = TRUE)


save(lgbm, file = "lgbm.rda")


tic()
modelo_lgbm <- workflow() %>%
  add_recipe(simple) %>%
  add_model(imp_spec) %>%
  last_fit(ctl_clean)
  extract_fit_parsnip()
toc()

  vip(aesthetics = list(alpha = 0.8, fill = "midnightblue"))











































