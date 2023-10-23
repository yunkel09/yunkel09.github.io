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
  thp   = "thoughput_dl",
  dis   = "cell_unavail",
  dif   = "cell_unavail_s1fail")

# Remover de métricas
metrics_to_remove <- c(
  "srvy_id",
  # "r1",
  "prttn_hr",
  "status",
  # "cll_prctg",
  "ra_ta_ue_index1",
  "ra_ta_ue_index2",
  "ra_ta_ue_index3",
  "ra_ta_ue_index4",
  "ra_ta_ue_index5",
  "ra_ta_ue_total",
  "modulation_64qam_ratio",
  "modulation_16qam_ratio",
  "modulation_qpsk_ratio",
  "thpughput_ul",
  "volte_erlang")

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
 "service_drop_rate",
 "tad")

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
 class_desc  = "c",
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
  select(twr = cilocation, status, lat = latitude, lon = longitude,
         dpto = department, municipalidad = municipality) %>%
  collect() |>
  distinct(twr, .keep_all = TRUE)

sitiosfs <- sitiosfs_raw |>
  mutate(across(dpto:municipalidad, \(x) estandarizar_columnas(x))) |>
  rename(city = "municipalidad")


# 5,412
# sitiosfs <- read_fst(sifs_f) |> as_tibble()

##  ............................................................................
##  Prep                                                                    ####

# Diagnóstico ------------------------------------------------------------------

# # Preprocesamiento y filtrado inicial
diagnosticos <- diag_00 |>
 mutate(
  across(diag, toupper)) |>
 filter(diag %in% etiquetas) |>

 # Dividir por 'diag' y aplicar transformaciones al grupo "NO_DIAG"
 split(~ diag) |>
 map_at("NO_DIAG", \(df) df |>
 filter(class_desc == "PROMOTOR") |>
 drop_na() |>
 # slice_sample(n = 1800) |>
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
  inner_join(hourly_metrics_raw, join_by(fct_srvy_dt, msisdn_dd, srvy_id))
# 32,164,414 × 31

ctl_00 |>
  arrange(msisdn_dd, r1, cll_prctg)



# Dataset exploratorio ---------------------------------------------------------

sitios_ruedas <- c("COW990", "COW994", "COW995", "COW999")

# 529 seg (8.33 min)
tic()
ctl_01 <- ctl_00 |>
 filter(

  # Eliminar casos con contadores apagados
  !if_all(
   c(rate_prb_dl,
     cell_load,
     service_drop_rate,
     l_ul_interference_avg,
     thoughput_dl), ~ .x == 0),

  # Los cqis menores a 5 no son de casos normales
  corrected_cqi > 5

 ) |>
 # Crear columna especial
 rowwise() |>
 mutate(tad = ra_ta_ue_index6 + ra_ta_ue_index7, .keep = "unused") |>
 ungroup() |>

 # Crear y transformar columnas
 mutate(

  # fechas
  met = str_sub(prttn_hr, 1, 8) |> ymd(),
  hor = str_sub(prttn_hr, 9, 10),
  date = str_c(met, hor, sep = " ") |> ymd_h(),

  # nuevas
  twr  = str_sub(bts_sh_nm, start = 2, end = 7),

  # conversiones
  across(fct_srvy_dt, ymd),                      # convertir a fecha
  across(all_of(enteros), as.integer),           # convertir a enteros
  across(thoughput_dl, \(x) x / 1e3),            # convertir a Mbps
  across(all_of(porcentajes), \(col) col * 100), # convertir a porcentaje
  across(diag, as_factor)                        # convertir a factor

 ) |>

 # Agregar datos de geografía
 left_join(sitiosfs, join_by(twr)) |>

 # Remover columnas innecesarias y luego renombrar
 select(-all_of(metrics_to_remove)) |>
 rename(any_of(lookup)) |>

 # Reordenar dataset
 relocate(
  user,
  date,
  poll,
  met,
  hor,
  dpto,
  city,
  twr,
  bts,
  lat,
  lon,
  prb,
  thp,
  rrc,
  erb,
  drp,
  tad,
  lod,
  erf,
  cqi,
  dis,
  dif,
  diag) |>
 filter(!twr %in% sitios_ruedas)
toc()
# 31,648,712 × 23

# Guardar dataset preprocesado
cted_f <- path("data", "ctlexp", ext = "fst")
write_fst(ctl_01, path = cted_f, compress = 0)


# Crear dos datasets
tic()
# 719 seg (11 min)
c(top3, top5, top10) %<-% (c(3, 5, 10) |>
 map(~ ctl_01 |>
  arrange(user, met, hor, desc(cll_prctg)) |>
  group_by(user, met, hor) |>
  slice_head(n = .x) |>
  mutate(top = row_number()) |>
  ungroup()))
toc()

top5_f <- path("data", "top5", ext = "fst")
# top10_f <- path("data", "top10", ext = "fst")
# write_fst(top5, path = top5_f, compress = 0)
# write_fst(top10, path = top10_f, compress = 0)

top5 <- read_fst(top5_f) |> as_tibble()

# 45571414 promotor
# 38508957 capacidad
# 46055729 disponibilidad
# 53089094 optimización
# 57522589 cobertura

numeros <- c(45571414, 38508957, 46055729, 53089094, 57522589)


muestra <- top5 |>
  filter(user %in% numeros) |>
  select(user, bts, date, met, hor, cll_prctg, prb:dis,
         diag) |>
  arrange(user, bts, met, hor) |>
  mutate(
   across(cll_prctg, \(x) x * 100),
   # across(diag, as.character),
   diag2 = case_when(
     prb > 85 & thp < 2.5 & tad < 15 ~ "CAPACIDAD",
     prb > 85 & thp < 2.5 & tad > 15 ~ "COBERTURA",
     .default = diag
   )
  )

top5 |>
  pivot_longer(prb:dfi, names_to = "metric", values_to = "value")

top5 |>
  filter(diag == "PROMOTOR", prb > 85 & thp < 2.5 & tad < 15)






muestra |>
  # filter(diag == "OPTIMIZACION") |>
  arrange(-cll_prctg) |>
  mutate(
    across(diag, as.character),
    diag2 = case_when(
    prb > 85 & thp < 2.5 & tad < 15 ~ "CAPACIDAD",
    prb > 85 & thp < 2.5 & tad > 15 ~ "COBERTURA",
    .default = diag
   )
  )


diag_capacity <- muestra |>
  filter(user == 53089094)

diag_capacity |>
  arrange(-cll_prctg)

diag_capacity |>
  pivot_longer(prb:dif, names_to = "metric", values_to = "value") |>
  ggplot(aes(x = value)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ metric, scales = "free")

diag_capacity |>
  arrange(-cll_prctg) |>
  ggplot(aes(x = date, y = prb)) +
  geom_line(aes(color = bts))


# ¿Cómo saber con cual de esta celda tomaron la decisión?
diag_capacity |>
  mutate(
    eff = if_else(is.infinite(prb / thp), 0, (prb / thp)),
    prb_alert = as.integer(prb > 80),
    thp_alert = as.integer(thp < 2.5),
    cap_alert = as.integer(eff > 32)) |>
  relocate(diag, .after = last_col())
  filter(cap_alert == 1)



# ¿Cuántas de cada métrica?
muestra |>
  group_by(user) |>
  summarise(across(c(twr, bts, met, hor), n_distinct))



top5 |> count(diag)

userx <- top5 |>
  filter(user == 30001118, met <= first(met) + days(15))

userx |>
  count(twr, sort = TRUE)

userx |>
  ggplot(aes(y = fct_rev(fct_infreq(bts)))) +
  geom_bar()

userx |>
  arrange(-cll_prctg) |>
  count(cll_prctg)
  ggplot(aes(x = cll_prctg)) +
  geom_histogram(bins = 30)

userx |>
  filter(twr == "GUA181")
  ggplot(aes(x = date, y = prb)) +
  geom_point(aes(color = twr)) +
  geom_line(aes(color = twr))
  # scale_x_datetime(breaks = "hour")



# Cargar dataset preprocesado --------------------------------------------------

# 6.34 seg
ctl_01 <- read_fst(cted_f) |> as_tibble()


##  Resumir                                                                 ####

# algunos filtros y transformaciones adicionales (pre-resumen)
ctl_02 <- ctl_01 |>
 filter(
  !if_all(prb:lod, ~ .x == 0),
  !if_all(prb:thp, ~ .x == 0))

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



















