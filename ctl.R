#   ____________________________________________________________________________
#   CTL                                                                     ####

# Fecha: 2023-10-17
# Autor: William Chavarría

##  ............................................................................
##  Paquetes y funciones                                                    ####

source("_paquetes.R")

##  ............................................................................
##  Variables                                                               ####

# Etiquetas para variable respuesta
etiquetas <- c(
 "CAPACIDAD",
 "OPTIMIZACION",
 "COBERTURA",
 "DISPONIBILIDAD",
 "NO_DIAG")

# Tabla par renombrar columnas
lookup <- c(
  poll  = "fct_srvy_dt",
  erf   = "l_ul_interference_avg",
  user  = "msisdn_dd",
  erb   = "erab_success_rate",
  bts   = "bts_sh_nm",
  lod   = "cell_load",
  rrc   = "rrc_success_rate",
  drp   = "service_drop_rate",
  prb   = "rate_prb_dl",
  cqi   = "corrected_cqi",
  lte   = "time_lte",
  thp   = "thoughput_dl",
  m64   = "modulation_64qam_ratio",
  m16   = "modulation_16qam_ratio",
  psk   = "modulation_qpsk_ratio",
  time  = "cll_prctg",
  vol   = "volte_erlang",
  dis   = "cell_unavail")

# Remover de métricas
metrics_to_remove <- c(
  "srvy_id",
  "r1",
  "prttn_hr",
  # "status",
  # "cll_prctg",
  "ra_ta_ue_index1",
  "ra_ta_ue_index2",
  "ra_ta_ue_index3",
  "ra_ta_ue_index4",
  "ra_ta_ue_index5",
  "ra_ta_ue_index6",
  "ra_ta_ue_index7",
  "ra_ta_ue_total",
  "thpughput_ul",
  # "volte_erlang",
  "cell_unavail_s1fail"
  )

enteros <- c(
 "msisdn_dd",
 "corrected_cqi",
 "cell_unavail",
 "cell_unavail_s1fail")

porcentajes <-c(
 "rate_prb_dl",
 "cell_load",
 "rrc_success_rate",
 "erab_success_rate",
 "modulation_64qam_ratio",
 "modulation_16qam_ratio",
 "modulation_qpsk_ratio",
 "service_drop_rate",
 "time_lte",
 "cll_prctg",
 "tad")

sitios_ruedas <- c("COW990", "COW994", "COW995", "COW999")

##  ............................................................................
##  Ingesta                                                                 ####

# Rutas ------------------------------------------------------------------------

diag_f <- path("data", "diags", ext = "csv")
metr_f <- path("data", "metri", ext = "fst")
sifs_f <- path("data", "sitfs", ext = "fst")

# Diagnósticos -----------------------------------------------------------------

# Definir columnas de interés y su tipo correspondiente
cols_diag <- cols_only(
 fct_srvy_dt = "c",
 msisdn_dd   = "c",
 srvy_id     = "i",
 time_lte    = "d",
 class_desc  = "c",
 diag        = "c")

# Leer el archivo
diag_00 <- read_csv(diag_f, col_types = cols_diag)

# Métricas por hora ------------------------------------------------------------

# 16.61 seg
hourly_metrics_raw <- read_fst(path = metr_f) |> as_tibble()

# Sitios Field Service ---------------------------------------------------------

con <- conectigo::conectar_msql()

sitiosfs_raw <- tbl(con, in_schema("fieldservice", "site_collate")) |>
  select(twr = cilocation, lat = latitude, lon = longitude,
         dpto = department, municipalidad = municipality) %>%
  collect() |>
  distinct(twr, .keep_all = TRUE)

sitiosfs <- sitiosfs_raw |>
  mutate(across(dpto:municipalidad, \(x) estandarizar_columnas(x))) |>
  rename(city = "municipalidad")

##  ............................................................................
##  Prep                                                                    ####

# Diagnóstico ------------------------------------------------------------------

# # Preprocesamiento y filtrado inicial
diagnosticos <- diag_00 |>
 mutate(
  across(diag, toupper)) |>
 filter(diag %in% etiquetas) |>
 split(~ diag) |>
 map_at("NO_DIAG", \(df) df |>
 filter(class_desc == "PROMOTOR") |>
 drop_na() |>
 distinct(msisdn_dd, .keep_all = TRUE)) |>
 # Combinar listas en un solo data frame
 list_rbind() |>
 # Transformaciones finales
 mutate(
  across(diag,
   \(x) case_match(x, "NO_DIAG" ~ "PROMOTOR", .default = diag))) |>
 select(-class_desc) |>
 distinct(msisdn_dd, .keep_all = TRUE)


# Unir y transformar -----------------------------------------------------------

# 18.2 seg
ctl_00 <- diagnosticos |>
 inner_join(
  hourly_metrics_raw, join_by(fct_srvy_dt, msisdn_dd, srvy_id))

# Dataset exploratorio ---------------------------------------------------------

# 327 seg (5 min)
tic()
ctl_01 <- ctl_00 |>
 mutate(
  tad   = ra_ta_ue_index6 + ra_ta_ue_index7,
  fecha = str_sub(prttn_hr, 1, 8) |> ymd(),
  hora  = str_sub(prttn_hr, 9, 10),
  date  = str_c(fecha, hora, sep = " ") |> ymd_h(),
  twr   = str_sub(bts_sh_nm, start = 2, end = 7),
  across(all_of(enteros), as.integer),
  across(thoughput_dl, \(x) x / 1e3),
  across(all_of(porcentajes), \(col) col * 100)) |>
 left_join(sitiosfs, join_by(twr)) |>
 select(-all_of(metrics_to_remove)) |>
 rename(any_of(lookup)) |>
 relocate(
  user, date, poll, fecha, hora, dpto, city,twr, bts, time, lat, lon, prb,
  thp, rrc,erb, drp, tad, lod, erf, cqi, m64, m16, psk, vol, dis, lte, diag)
toc()

tic()
ctl_02 <- ctl_01 |>
 filter(
  !twr %in% sitios_ruedas,
  !if_all(prb:dis, ~ .x == 0),
  !(dis == 0 & diag == "DISPONIBILIDAD")) |>
 arrange(user, fecha, hora, desc(time)) |>
 group_by(user, fecha, hora) |>
 slice_head(n = 5) |>
 ungroup()
toc()
# 11,044,464 × 24


# Este considera el top10 para tener más muestras y no perder varianza
tic()
ctl_03 <- ctl_01 |>
  filter(
    !twr %in% sitios_ruedas,
    !if_all(prb:dis, ~ .x == 0),
    !(dis == 0 & diag == "DISPONIBILIDAD")) |>
  arrange(user, fecha, hora, desc(time)) |>
  group_by(user, fecha, hora) |>
  slice_head(n = 10) |>
  ungroup()
toc()


# Guardar y cargar datasets preprocesados
cted_f <- path("data", "ctlexp", ext = "fst")
top10f <- path("data", "top10", ext = "fst")
write_fst(ctl_02, path = cted_f, compress = 0)
write_fst(ctl_03, path = top10f, compress = 0)
ctl_02 <- read_fst(cted_f) |> as_tibble()
ctl_03 <- read_fst(top10f) |> as_tibble()


# Definir reglas mutuamente excluyentes
capacidad <- expr(prb >= 85 & thp <= 2.7 & tad <= 15)
cobertura <- expr((prb >= 85 & thp <= 2.7) | tad >= 15)
disponibi <- expr(if_all(prb:vol, ~ .x == 0) | dis > 0)
optimizac <- expr(
 erb < 90 |
 rrc < 90 |
 cqi < 7   |
 erf > -95 |
 drp > 1.5 |
 (prb == 0 & thp >  0 & lod == 0) |
 (thp == 0 & prb >  0 & vol == 0) |
 (prb == 0 & thp == 0 & vol == 0) |
 psk > m64
)
promotor  <- expr(
 prb <  85  &
 thp >  2.7 &
 tad <  15  &
 lte >  95  &
 erf <= -95 &
 cqi >  7   &
 dis == 0)

# 11.56 seg
tic()
ctl_04 <- ctl_03 |>
 drop_na() |>
 filter(!(dis == 0 & diag == "DISPONIBILIDAD")) |>
 mutate(
  diag2 = case_when(
   eval(optimizac) ~ "OPTIMIZACION",
   eval(capacidad) ~ "CAPACIDAD",
   eval(promotor)  ~ "PROMOTOR",
   eval(cobertura) ~ "COBERTURA",
   eval(disponibi) ~ "DISPONIBILIDAD",
  .default = diag
  ),
  across(diag:diag2, as.factor)
 )
toc()


tic()
ctl_05 <- ctl_04 |>
 group_by(user) |>
 filter(diag == diag2) |>
 ungroup()
toc()


##  Resumir                                                                 ####


tic()
# 408 seg (6.8 min)
ctl <- ctl_05 |>
  group_by(user) |>
  summarize_metrics() |>
  drop_na() |>
  mutate(across(where(is.character), as.factor))
toc()


# Guardar dataset resumido
write_fst(ctl, path = ctre_f, compress = 0)

# Empezar aquí
ctre_f <- path("data", "ctlre", ext = "fst")
ctl <- read_fst(ctre_f) |> as_tibble() |> suppressMessages()

##  ............................................................................
##  Split                                                                   ####

ctl_split <- initial_validation_split(data = ctl, strata = diag)

ctl_train          <- training(ctl_split)
ctl_validacion     <- validation(ctl_split)
ctl_validation_set <- validation_set(ctl_split)

##  ............................................................................
##  Cross-validation                                                        ####

set.seed(2023)
ctl_folds <- vfold_cv(ctl_train, strata = diag, v = 10)

##  ............................................................................
##  EDA                                                                     ####

# Revisar information gain
mi <- information_gain(
  formula      = diag ~ .,
  data         = ctl_train[-1],
  type         = "infogain",
  discIntegers = FALSE) |>
  as_tibble() |>
  arrange(-importance)

mi |>
 filter(importance > 0.5) |>
 mutate(feature = fct_reorder(attributes, importance, .desc = FALSE)) |>
 ggplot(aes(x = importance, y = feature)) +
 geom_point() +
 labs(title = "MI por usuario") +
 drako +
 theme(
  axis.text.y = element_text(size = 20),
  axis.text.x = element_text(size = 10))


tmwr_cols <- colorRampPalette(c("#91CBD765", "#CA225E"))
principales_features <- ctl_train |>
  select(diag, where(is.numeric), -user) |>
  recipe(diag ~ ., data = _) |>
  step_select_infgain(
   all_numeric_predictors(), outcome = "diag",
   top_p = 16) |>
  step_rm(diag) |> ver()


principales_features |>
cor() |>
corrplot(col = tmwr_cols(200), tl.col = "black", method = "ellipse")




# PCA --------------------------------------------------------------------------

ctl_num <- ctl_train |> select(diag, where(is.numeric), -user)
ctl_rec <- recipe(diag ~ ., data = ctl_num) |>
  step_zv(all_numeric_predictors()) |>
  step_orderNorm(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors())

ctl_trained <- prep(ctl_rec)



plot_validation_results <- function(recipe, dat = ctl_validacion) {
    recipe %>%
      # Estimate any additional steps
      prep() %>%
      # Process the data (the validation set by default)
      bake(new_data = dat) %>%
      # Create the scatterplot matrix
      ggplot(aes(x = .panel_x, y = .panel_y, color = diag, fill = diag)) +
      geom_point(size = 1) +
      geom_autodensity() +
      facet_matrix(vars(-diag), layer.diag = 2) +
      scale_color_brewer(palette = "Dark2") +
      scale_fill_brewer(palette = "Dark2") +
      drako
  }

ctl_trained %>%
  step_pca(all_numeric_predictors(), num_comp = 4) %>%
  plot_validation_results() +
  ggtitle("Principal Component Analysis")

library(learntidymodels)
ctl_trained %>%
  step_pca(all_numeric_predictors(), num_comp = 4) %>%
  prep() %>%
  plot_top_loadings(component_number <= 4, n = 5) +
  scale_fill_brewer(palette = "Paired") +
  ggtitle("Principal Component Analysis")


ctl_trained %>%
  step_pls(all_numeric_predictors(), outcome = "diag", num_comp = 4) %>%
  plot_validation_results() +
  ggtitle("Partial Least Squares")

##  ............................................................................
##  Métricas y racing control                                               ####
mset <- metric_set(precision, recall, f_meas, roc_auc)

# definir race control para búsqueda de hiperparámetros
race_ctrl <- control_race(
  save_pred     = TRUE,
  parallel_over = "everything",
  verbose       = TRUE,
  verbose_elim  = TRUE,
  save_workflow = FALSE)

##  ............................................................................
##  Model Spec                                                              ####

glmnet_spec <- multinom_reg(
  penalty = tune(),
  mixture = tune()) |>
 set_engine('glmnet') |>
 set_mode('classification')


xgboost_spec <- boost_tree(
 tree_depth     = 4,
 trees          = tune(),
 learn_rate     = tune(),
 min_n          = tune(),
 loss_reduction = tune(),
 sample_size    = tune(),
 stop_iter      = tune()) |>
set_engine('xgboost') %>%
set_mode('classification')


keras_spec <- multinom_reg(
  penalty = tune()) |>
  set_engine("keras") |>
  set_mode('classification')


h2o_spec <- multinom_reg(
 penalty = tune(),
 mixture = tune()) |>
set_engine("h2o") |>
set_mode('classification')


spark_spec <- multinom_reg(
 penalty = tune(),
 mixture = tune()) |>
set_engine("spark") |>
set_mode('classification')

# lista de modelos
modelos <- list(
 glmnet  = glmnet_spec,
 xgboost = xgboost_spec,
 keras   = keras_spec,
 h2o     = h2o_spec,
 spark   = spark_spec)

##  ............................................................................
##  Preprocesamiento                                                        ####

infgain_regular <- recipe(diag ~ ., data = ctl_train) |>
  step_rm(matches("timeout$|counter$")) |>
  step_rm(all_nominal_predictors()) |>
  update_role(user, new_role = "id") |>
  step_select_infgain(
    all_predictors(),
    outcome = "diag",
    threshold = 0.8) |>
  step_smote(diag, skip = TRUE)

infgain_norm <- recipe(diag ~ ., data = ctl_train) |>
  step_rm(matches("timeout$|counter$")) |>
  step_rm(all_nominal_predictors()) |>
  update_role(user, new_role = "id") |>
  step_orderNorm(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_select_infgain(
    all_predictors(),
    outcome = "diag",
    threshold = 0.8) |>
  step_smote(diag, skip = TRUE)


recetas <- list(
 infgain_regular      = infgain_regular,
 infgain_norm         = infgain_norm
)


ctl_set <- workflow_set(preproc = recetas, models  = modelos)
ctl_set

# Cambiar a última versión
ver_tr   <- pin_versions(board = tablero_ctl, name = "tune_res")$version[[2]]
tune_res <- pin_read(board = tablero_ctl, name = "tune_res", version = ver)


# Ajustar hiperparámetros ---------------------------------------------------- #

cl <- makePSOCKcluster(10)
registerDoParallel(cl)

tune_res <- ctl_set |>
 workflow_map(
 fn = "tune_race_anova",
 verbose   = TRUE,
 resamples = ctl_folds,
 control   = race_ctrl,
 seed      = 2023,
 metrics   = mset,
 grid      = 20)

stopCluster(cl)
unregister()

# ---------------------------------------------------------------------------- #

pin_write(
  board       = tablero_ctl,
  x           = tune_res,
  name        = "tune_res",
  type        = "qs",
  versioned   = TRUE,
  title       = "Resultados entrenamiento",
  description = "Usando validacion cruzada")


tune_res <- tune_res |> filter(wflow_id %in% c("infgain_regular_glmnet",
                                               "infgain_regular_xgboost",
                                               "infgain_norm_glmnet",
                                               "infgain_norm_xgboost"))

metricas_training <- tune_res |>
 rank_results(select_best = TRUE, rank_metric = "f_meas") |>
 select(modelo = wflow_id, .metric, mean, rank) |>
 pivot_wider(names_from = .metric, values_from = mean) |>
 select(modelo, f1_score_tr = f_meas, precision_tr = precision,
        recall_tr = recall)

metricas_training




tune_res |>
autoplot(rank_metric = "f_meas", metric = "f_meas", select_best = TRUE) +
  geom_text(aes(y = mean - 1/2, label = wflow_id), angle = 90, hjust = 0.5) +
  lims(y = c(0, 1.2)) +
  theme(legend.position = "none") +
  drako

# plot race: https://bit.ly/46KlDui
tune_res |>
  extract_workflow_set_result(id = "infgain_norm_multi_glmnet") |>
  plot_race()

# Revisar los resultados en diferentes folds: https://bit.ly/3Frog8x
tune_res |>
  extract_workflow_set_result(id = "infgain_norm_multi_glmnet") |>
  collect_predictions() |>
  group_by(id) |>
  f_meas(diag, .pred_class)


# Selección manual de modelos
best <- metricas_training %>%
  # slice(1:12) |>
  pull(modelo) %>%
  set_names(.)

lista_mejores <- best %>%
  map(~ tune_res %>% extract_workflow_set_result(id = .x) %>%
        select_best(metric = "f_meas"))



# Probar con validación --------------------------------------------------------

# Cambiar a la versión correspondiente
ver_val  <- pin_versions(board = tablero_ctl, name = "validation")$version[[2]]
tic()
validation_result_list <- pin_read(
 board = tablero_ctl,
 name = "validation",
 version = ver_val)
toc()


tic()
# 93 seg
validation_result_list <- map2(
 .x = best,
 .y = lista_mejores, ~ tune_res %>%
 extract_workflow(id = .x) %>%
 finalize_workflow(.y) %>%
 last_fit(split = ctl_validation_set$splits[[1]], metrics = mset))
toc()

pin_write(
  board       = tablero_ctl,
  x           = validation_result_list,
  type        = "qs",
  name        = "validation",
  versioned   = TRUE)


validation_result_list

metricas_validation <- validation_result_list %>%
  map_dfr(~ collect_metrics(.x), .id = "modelo") %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  select(modelo, f1_score_val = f_meas, precision_val = precision,
         recall_val = recall) |>
  drop_na()

metricas_validation

# Importancia Relativa usando Vip
validation_result_list |>
  pluck("infgain_regular_bt_xgboost") |>
  extract_workflow(id = "infgain_regular_bt_xgboost") |>
  extract_fit_parsnip() |>
  vip(geom = "point", num_features = 20) +
  drako


validation_result_list |>
  keep_at(sof) |>
  pluck(1) |> collect_predictions() |>
  conf_mat(diag, .pred_class) |>
  autoplot() +
  theme_light()

metricas_validation

# comprobar que si estamos utilizando "Macro-averaging"
validation_result_list |>
  keep_at(sof) |>
  pluck(1) |>
  collect_predictions() |>
  f_meas(truth = diag, estimate = .pred_class, estimator = "macro")



comparacion <- metricas_training |>
  left_join(metricas_validation, join_by(modelo)) |>
  mutate(
  overfit_f1     = f1_score_tr > f1_score_val,
  overfit_recall = recall_tr > recall_val,
  overfit_precision = precision_tr > precision_val) |>
  relocate(modelo,
           f1_score_tr,
           f1_score_val,
           overfit_f1,
           recall_tr,
           recall_val,
           overfit_recall,
           precision_tr,
           precision_val,
           overfit_precision)

# agarrar los primeros 5 y mejorar los hiperparámetros y las recetas que si
# funcionan
comparacion

sin_overfit <- comparacion |>
  filter(if_all(starts_with("overfit"), ~ .x == FALSE))

sof <- sin_overfit |> pull(modelo)











