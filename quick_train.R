
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
  fecha          = "fecha_15dias",
  interf         = "l_ul_interference_avg",
  usuario        = "msisdn_dd",
  ciudad         = "cty_nm",
  depto          = "stt_nm",
  region         = "trrtry_tf_nm",
  erab           = "erab_success_rate",
  mod_16qam      = "modulation_16qam_ratio",
  mod_64qam      = "modulation_64qam_ratio",
  mod_qpsk       = "modulation_qpsk_ratio",
  bts            = "bts_sh_nm",
  disp_ttks      = "avlblty_tckt_2hrs",
  disp           = "avlblty",
  disp_prop      = "unvlblty_ttl_hrs_prop",
  rrc            = "rrc_success_rate",
  drop_rate      = "service_drop_rate",
  thp_lte        = "thp_required_lte",
  ta_1           = "ra_ta_ue_index1",
  ta_2           = "ra_ta_ue_index2",
  ta_3           = "ra_ta_ue_index3",
  ta_4           = "ra_ta_ue_index4",
  ta_5           = "ra_ta_ue_index5",
  ta_6           = "ra_ta_ue_index6",
  ta_7           = "ra_ta_ue_index7",
  ta_total       = "ra_ta_ue_total",
  prb            = "rate_prb_dl",
  cqi            = "corrected_cqi",
  thp_dl         = "thoughput_dl",
  lluvia         = "rain",
  tiempo         = "cll_prctg"
)

##  ............................................................................
##  Ingesta                                                                 ####

archivos <- dir_ls(type = "file", path = "data", glob = "*.csv")

c(diag_00, disp_00, metr_00, lluv_00) %<-% (archivos |> map(read_csv))

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
  distinct(msisdn_dd, .keep_all = TRUE)                     # <11>

metricas_no_informativas <- c("cell_load", "thpughput_ul", "volte_erlang")

metr_01 <- metr_00 |>
  mutate(across(fct_srvy_dt, ymd)) |>
  relocate(fct_srvy_dt, msisdn_dd, srvy_id, r1, bts_sh_nm) |>
  arrange(fct_srvy_dt, msisdn_dd) |>
  select(-all_of(metricas_no_informativas))

disp_01 <- disp_00 |>
  mutate(across(everything(), \(x) na_if(x, -1))) |>
  select(-time_thrgpt)

lluv_01 <- lluv_00 |>
  select(-fct_srvy_15_mnth, -twr) |>
  mutate(across(fct_srvy_dt, ymd))

ctl_00 <- metr_01 |>
  inner_join(disp_01, join_by(msisdn_dd, srvy_id)) |>
  inner_join(lluv_01, join_by(msisdn_dd, srvy_id, fct_srvy_dt, bts_sh_nm))

ctl_01 <- diag_03 |>
 inner_join(ctl_00, join_by(msisdn_dd, srvy_id, fct_srvy_dt)) |>
 relocate(diag, .before = fct_srvy_dt) |>
 relocate(thp_required_lte, .after = cll_prctg) |>
 mutate(
  across(msisdn_dd, as.integer),
  fecha_15dias = fct_srvy_dt - days(15), .after = fct_srvy_dt,
  across(where(is_character), as_factor)
 ) |>
 select(-c(r1, fct_srvy_dt, srvy_id)) |>
 filter(!if_all(rate_prb_dl:modulation_qpsk_ratio, ~ .x == 0)) |>
 rename(all_of(lookup))

##  ............................................................................
##  Split                                                                   ####

ctl_split <- ctl_01 |>
  group_initial_validation_split(group = usuario, strata = diag)

ctl_train   <- training(ctl_split)

##  ............................................................................
##  Cross-validation                                                        ####

tic()
ctl_folds <- group_vfold_cv(
  data    = ctl_train,
  group   = "usuario",
  v       = 5,
  balance = "observations",
  strata  = "diag")
toc()

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
 tree_depth  = tune(),
 trees       = tune(),
 learn_rate  = tune(),
 min_n       = tune(),
 loss_reduction = tune(),
 mtry        = tune(),
 sample_size = tune(),
 stop_iter   = tune()) %>%
 set_engine('xgboost') %>%
 set_mode('classification')


# lista de modelos
modelos <- list(dt_rpart = dt_rpart, multi_glmnet = multi_glmnet)

##  ............................................................................
##  Preprocesamiento                                                        ####

rec_05 <- recipe(diag ~ ., data = ctl_train) |>
 step_rm(all_nominal_predictors()) |>
 step_rm(fecha) |>
 step_impute_median(all_numeric_predictors()) |>
 step_normalize(all_numeric_predictors()) |>
 step_upsample(diag, skip = TRUE)


rec_05 |> ver() |> glimpse()
rec_05 |> ver() |> inspectdf::inspect_na() |> print(n = Inf)
rec_05 |> ver() |> count(diag)


recetas <- list(variable = rec_05)
ctl_set <- workflow_set(preproc = recetas, models  = modelos)
ajustar()













