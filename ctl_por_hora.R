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
 "DISPONIBILIDAD")
 # "NO_DIAG")

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
  "cell_unavail_s1fail"
  # "volte_erlang"
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
 filter(diag %in% etiquetas)

# Unir y transformar -----------------------------------------------------------

# 18.2 seg
ctl_00 <- diagnosticos |>
 inner_join(
  hourly_metrics_raw, join_by(fct_srvy_dt, msisdn_dd, srvy_id),
  relationship = "many-to-many")

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
  !if_all(prb:dis, ~ .x == 0)) |>
 arrange(user, fecha, hora, desc(time)) |>
 group_by(user, fecha, hora) |>
 slice_head(n = 5) |>
 ungroup()
toc()
# 11,044,464 × 24

# Guardar dataset preprocesado
cted_f <- path("data", "ctlexp", ext = "fst")
write_fst(ctl_02, path = cted_f, compress = 0)

ctl_02 <- read_fst(cted_f) |> as_tibble()


ctl_02 |> filter(prb < 80 & thp > 2.7 & tad < 15 & lte > 80 & erf < -95 & cqi > 7 & dis == 0)

# AQUI ME QUEDE
# 1. Corregir las etiquetas con base a las nuevas reglas
# 2. Validar que ya no haya OPTIMIZACION

ctl_03 <- ctl_02 |>
  drop_na() |>
  filter(!(dis == 0 & diag == "DISPONIBILIDAD")) |>
  mutate(
    diag2 = case_when(
      (prb > 80 & thp < 2.7 & tad < 15) | lte < 95     ~ "CAPACIDAD",
      prb > 80 & thp < 2.7 & tad > 15  ~ "COBERTURA",
      if_all(prb:vol, ~ .x == 0) & dis > 0 ~ "DISPONIBILIDAD",
      prb < 80 & thp > 2.7 & tad < 15 & lte > 95 & erf < -95 & cqi > 7 & dis == 0 ~ "PROMOTOR",
      cqi < 7 & psk > 33 ~ "OPTIMIZACION",
      .default = diag
    )
  )

ctl_03 |> count(diag2)

ctl_03 |> summarise(n = n_distinct(user), .by = diag2) |> mutate(prop = n / sum(n) * 100)
ctl_03 |> filter(diag2 == "OPTIMIZACION") |> sample_n(size = 55) |> print(n = 55)


##  Resumir                                                                 ####


tic()
# 169.35 (2.82 min)
reglas_de_negocio_df <- ctl_02 |>
  crear_business_rules()
toc()

tic()
# 231 seg (4 min)
ctl <- ctl_02 |>
  group_by(user) |>
  summarize_metrics() |>
  ungroup() |>
  left_join(reglas_de_negocio_df, by = join_by(user))
toc()
# 4,244 × 216

# algunos filtros y transformaciones adicionales (post-resumen)
ctl_m <- ctl |>
 mutate(
   across(where(is.character), as_factor),
   across(where(ends_with("rule")), as.integer)

 )


# Guardar dataset resumido
ctre_f <- path("data", "ctlre", ext = "fst")
# write_fst(ctl_m, path = ctre_f, compress = 0)

# Empezar aquí
ctl <- read_fst(ctre_f) |> as_tibble()

##  ............................................................................
##  Split                                                                   ####

ctl_split <- initial_validation_split(data = ctl, strata = diag)

ctl_train <- training(ctl_split)

##  ............................................................................
##  Cross-validation                                                        ####

ctl_folds <- vfold_cv(ctl_train, strata = diag, v = 3)

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
 filter(importance != 0) |>
 mutate(feature = fct_reorder(attributes, importance, .desc = FALSE)) |>
 ggplot(aes(x = importance, y = feature)) +
 geom_point() +
 labs(title = "MI por usuario") +
 drako +
 theme(
  axis.text.y = element_text(size = 10),
  axis.text.x = element_text(size = 10))


ctl |> count(diag)
  ggplot()



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

rf_ranger <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

# lista de modelos
modelos <- list(rf_ranger = rf_ranger)

##  ............................................................................
##  Preprocesamiento                                                        ####

rec_basica <- recipe(diag ~ ., data = ctl_train) |>
 update_role(user, new_role = "id") |>
 step_novel(twr) |>
 step_other(city, threshold = 2) |>
 step_impute_median(all_numeric_predictors()) |>
 step_zv(all_numeric_predictors()) |>
 step_upsample(diag, skip = TRUE)
 # ver()





ajustar <- function() {

  cl <- makePSOCKcluster(10)
  registerDoParallel(cl)

  tune_res <- ctl_set |>
    workflow_map(
      fn        = "tune_race_anova",
      verbose   = TRUE,
      resamples = ctl_folds,
      control   = race_ctrl,
      seed      = 2023,
      metrics   = mset,
      grid      = 20)

  stopCluster(cl)
  unregister()

  tune_res |>
    rank_results(select_best = TRUE, rank_metric = "f_meas") |>
    select(modelo = wflow_id, .metric, mean, rank) |>
    pivot_wider(names_from = .metric, values_from = mean)

}


recetas <- list(variable = rec_basica)
ctl_set <- workflow_set(preproc = recetas, models  = modelos)
ajustar()


  step_rm(all_nominal_predictors()) |>
  step_rm(fecha) |>
  step_impute_median(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_upsample(diag, skip = TRUE)



















