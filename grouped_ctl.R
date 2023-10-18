#   ____________________________________________________________________________
#   CTL                                                                     ####

# Fecha: 2023-10-13
# Autor: William Chavarría

##  ............................................................................
##  Paquetes y funciones                                                    ####

source("_paquetes.R")

##  ............................................................................
##  Variables                                                               ####

etiquetas <- c(
 "CAPACIDAD",
 "OPTIMIZACION",
 "COBERTURA",
 "DISPONIBILIDAD",
 "NO_DIAG")

lookup <- c(
 ciudad         = "cty_nm",
 depto          = "stt_nm",
 region         = "trrtry_tf_nm",
 disp_ttks      = "avlblty_tckt_2hrs",
 disp           = "avlblty",
 disp_prop      = "unvlblty_ttl_hrs_prop",
 thp_lte        = "thp_required_lte")

##  ............................................................................
##  Ingesta                                                                 ####

archivos <- dir_ls(type = "file", path = "data", glob = "*.csv")

c(diag_00, disp_00, metr_00, lluv_00) %<-% (archivos |> map(read_csv))

grouped_metrics <- read_csv("grouped/metricas_agrupadas.csv")

cols_to_remove <- c(
 "tipo_cobertura",   # 97.7% de los datos dicen TBD y el 2% es NA
 "cell_load_3g",     # André indica: "mejor utilizar `cell_load_lte`
 "users_class",      # Ignorar por el momento
 "fct_dt",           # Actualización del registro a nivel de data
 "time_3g",          # Métrica vieja que ya no se le dio seguimiento
 "bs_ln_nm",         # Dejaremos `bs_ln_nm` por contener más información
 "thp_required_3g",  # Demasiados valores faltantes
 "time_lte",         # Demasiados valores faltantes,
 "bs_ln_cd",         # Feature no informativo
 "nps_mdll",         # Genera fuga de datos,
 "bts_sh_nm_1",      # Mejor usar bts_sh_nam
 "cell_load_lte",     # La métrica time_cl y time_lte son mejores
 "trrtry_cmrcl_nm"
)

##  ............................................................................
##  Prep                                                                    ####

diag_01 <- diag_00 |>
 select(
  where(
   \(x) length(unique(x)) != 1 &&     # <1>
    mean(is.na(x)) < 0.5),             # <2>
  -all_of(cols_to_remove)             # <3>
 ) |>
 mutate(
  across(fct_srvy_dt, \(fecha) ymd(fecha)),   # <4>
  across(where(is.character), \(col) estandarizar_columnas(col)), # <5>
  across(msisdn_dd:srvy_id, as.integer)
 ) |>
 relocate(fct_srvy_dt, msisdn_dd, srvy_id)

diag_02 <- diag_01 |>
 arrange(fct_srvy_dt, msisdn_dd, srvy_id) |>
 mutate(
  across(where(is.character), \(x) na_if(x, "TBD")),
  across(where(is.character), \(x) na_if(x, "TO_BE_DETERMINED")))

diag_03 <- diag_02 |>
 filter(diag %in% etiquetas) |>               # <1>
 split(~ diag) |>                             # <2>
 map_at("NO_DIAG", \(df) df |>                # <3>
         filter(class_desc == "PROMOTOR") |>         # <4>
         drop_na() |>                                # <5>
         slice_sample(n = 1800) |>                   # <6>
         distinct(msisdn_dd, .keep_all = TRUE)) |>   # <7>
 list_rbind() |>                              # <8>
 mutate(
  across(diag,
         \(x) case_match(x, "NO_DIAG" ~ "PROMOTOR", .default = diag))) |>  # <9>
 select(-class_desc) |>                                             # <10>
 distinct(msisdn_dd, .keep_all = TRUE) |>
 rename(usuario = msisdn_dd)

metricas_no_informativas <- c("cell_load", "thpughput_ul", "volte_erlang")

metr_g <- grouped_metrics |>
 mutate(across(fct_srvy_dt, ymd)) |>
 relocate(fct_srvy_dt, usuario, srvy_id) |>
 arrange(fct_srvy_dt, usuario)

metr_01 <- metr_00 |>
 mutate(across(fct_srvy_dt, ymd)) |>
 relocate(fct_srvy_dt, msisdn_dd, srvy_id, r1, bts_sh_nm) |>
 arrange(fct_srvy_dt, msisdn_dd) |>
 select(-all_of(metricas_no_informativas))

disp_01 <- disp_00 |>
 mutate(across(everything(), \(x) na_if(x, -1))) |>
 select(-time_thrgpt) |>
 rename(usuario = msisdn_dd)

lluv_01 <- lluv_00 |>
 select(-fct_srvy_15_mnth, -twr) |>
 mutate(across(fct_srvy_dt, ymd)) |>
 rename(usuario = msisdn_dd) |>
 group_by(usuario) |>
 summarise(rain = median(rain),
           fct_srvy_dt = first(fct_srvy_dt),
           srvy_id = first(srvy_id))


ctl_00 <- metr_g |>
 inner_join(disp_01, join_by(usuario, srvy_id))
 # inner_join(lluv_01, join_by(usuario, srvy_id, fct_srvy_dt))

ctl_01 <- diag_03 |>
 inner_join(ctl_00, join_by(usuario, srvy_id, fct_srvy_dt)) |>
 # relocate(diag, .before = fct_srvy_dt) |>
 # relocate(thp_required_lte, .after = cll_prctg) |>
 mutate(
  across(usuario, as.integer),
  fecha = fct_srvy_dt - days(15), .after = fct_srvy_dt,
  across(where(is_character), as_factor)
 ) |>
 relocate(fecha, usuario) |>
 select(-c(srvy_id, fct_srvy_dt)) |>
 relocate(diag, .after = last_col()) |>
 rename(all_of(lookup))

##  ............................................................................
##  Split                                                                   ####

ctl_split <- ctl_01 |>
 initial_validation_split(strata = diag)

ctl_train   <- training(ctl_split)

##  ............................................................................
##  Cross-validation                                                        ####

ctl_folds <-  ctl_train |>
 vfold_cv(v = 5, strata  = "diag")

##  ............................................................................
##  Guardar datos                                                           ####

tablero_ctl <- board_folder(path = "tablero_ctl")

pin_write(
 board       = tablero_ctl,
 x           = ctl_split,
 name        = "ctl_split",
 type        = "rds",
 title       = "Three-way split",
 description = "División aleatoria de tres vías")

pin_write(
 board       = tablero_ctl,
 x           = ctl_folds,
 name        = "ctl_folds",
 type        = "rds",
 title       = "Grouped cross-validation",
 description = "Splited data based on userid variable")

##  ............................................................................
##  Cargar datos                                                            ####

source("_paquetes.R")
tablero_ctl <- board_folder(path = "tablero_ctl")
ctl_split   <- pin_read(board = tablero_ctl, name = "ctl_split")
ctl_folds   <- pin_read(board = tablero_ctl, name = "ctl_folds")
ctl_train   <- training(ctl_split)


##  ............................................................................
##  Métricas y racing control                                               ####
mset <- metric_set(precision, recall, f_meas)

# definir race control para búsqueda de hiperparámetros
race_ctrl <- control_race(
 save_pred     = TRUE,
 parallel_over = "everything",
 verbose       = TRUE,
 verbose_elim  = TRUE,
 save_workflow = TRUE)

##  ............................................................................
##  Model Spec                                                              ####

dt_rpart <- decision_tree(
 tree_depth      = tune(),
 min_n           = tune(),
 cost_complexity = tune()
) |>
 set_engine("rpart") |>
 set_mode("classification")

dt_partykit <- decision_tree(
 tree_depth      = tune(),
 min_n           = tune()) |>
 set_engine("partykit") |>
 set_mode("classification")

bt_lightgbm <- boost_tree(
 mtry  = tune(),
 trees = tune()) |>
 set_engine(engine = "lightgbm") |>
 set_mode(mode = "classification")

rf_partykit <- rand_forest(
 trees = tune(),
 min_n = tune(),
 mtry = tune()) |>
 set_engine(engine = "partykit") %>%
 set_mode(mode = "classification")

rf_ranger <-rand_forest() %>%
 set_engine('ranger') %>%
 set_mode('classification')

multi_keras <- multinom_reg(
 penalty = tune()) %>%
 set_engine('keras')

multi_glmnet <- multinom_reg(
 penalty = tune(),
 mixture = tune()) %>%
 set_engine('glmnet') |>
 set_mode('classification')

bt_xgboost <- boost_tree(
 # tree_depth  = tune(),
 trees       = tune(),
 # learn_rate  = tune(),
 min_n       = tune(),
 # loss_reduction = tune(),
 # mtry        = tune(),
 # sample_size = tune(),
 # stop_iter   = tune()
 ) %>%
 set_engine('xgboost') %>%
 set_mode('classification')


# lista de modelos
modelos <- list(
 dt_rpart    = dt_rpart,
 # mlp_brulee  = mlp_brulee,
 # multi_keras = multi_keras
 multi_glmnet = multi_glmnet,
 bt_xgboost   = bt_xgboost
 # dt_partykit = dt_partykit,
 # bt_lightgbm = bt_lightgbm,
 # rf_partykit = rf_partykit,
 # rf_ranger   = rf_ranger
)

##  ............................................................................
##  Preprocesamiento                                                        ####

rec_05 <- recipe(
 diag ~ .,
 # + thp_dl + thp_lte + prb + erab + mod_64qam + mod_16qam + interf + mod_qpsk + cqi + rrc + drop_rate + lluvia + ta_1 + ta_2 + ta_3 + ta_4 + ta_5 + ta_6 + ta_7,
 data = ctl_train) |>
 # step_rm(all_nominal_predictors()) |>
 step_rm(fecha) |>
 step_impute_median(all_numeric_predictors()) |>
 # step_YeoJohnson(all_numeric_predictors())
 step_normalize(all_numeric_predictors()) |>

 step_impute_knn(ciudad, region, depto) |>
 step_novel(ciudad, region, depto) |>
 step_other(ciudad, region, depto, threshold = 0.01) |>
 step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
 step_smote(diag, skip = TRUE)

rec_05 |> ver() |> glimpse()
rec_05 |> ver() |> inspectdf::inspect_na() |> print(n = Inf)
rec_05 |> ver() |> count(diag)


# step_zv(all_numeric_predictors()) |>
# step_interact(~ mod_64qam:starts_with("mod")) |>
# step_interact(~ mod_64qam:starts_with("thp"))
# step_upsample(diag, skip = TRUE)
# step_smote(diag, skip = TRUE)

recetas <- list(variable = rec_05)
ctl_set <- workflow_set(preproc = recetas, models  = modelos)
ctl_set
ajustar()













