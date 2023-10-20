#   ____________________________________________________________________________
#   CTL                                                                     ####

# Fecha: 2023-10-17
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
 # date   = "fecha_metrica",
 interf  = "l_ul_interference_avg",
 user    = "msisdn_dd",
 erab    = "erab_success_rate",
 # m16qam = "modulation_16qam_ratio",
 # m64qam = "modulation_64qam_ratio",
 # mqpsk  = "modulation_qpsk_ratio",
 bts       = "bts_sh_nm",
 load      = "cell_load",
 # disp_ttks = "avlblty_tckt_2hrs",
 # disp      = "avlblty",
 # disp_prop = "unvlblty_ttl_hrs_prop",
 rrc       = "rrc_success_rate",
 dropr     = "service_drop_rate",
 # thp_lte   = "thp_required_lte",
 # ta1       = "ra_ta_ue_index1",
 # ta2       = "ra_ta_ue_index2",
 # ta3       = "ra_ta_ue_index3",
 # ta4       = "ra_ta_ue_index4",
 # ta5       = "ra_ta_ue_index5",
 ta6       = "ra_ta_ue_index6",
 ta7       = "ra_ta_ue_index7",
 # ta_to     = "ra_ta_ue_total",
 prb       = "rate_prb_dl",
 cqi       = "corrected_cqi",
 thp       = "thoughput_dl"
 # thp_ul    = "thpughput_ul"
 # lluvia  = "rain",
 # time      = "cll_prctg"
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
 "cell_load_lte",    # La métrica time_cl y time_lte son mejores
 "trrtry_cmrcl_nm",  # No determina la ubicación de la celda
 "stt_nm",           # No determina la ubicación de la celda
 "trrtry_tf_nm",     # No determina la ubicación de la celda
 "cty_nm",           # No determina la ubicación de la celda
 "thp_required_lte"  # Métrica sumarizada
)

# Métricas por hora ------------------------------------------------------------

parquet_file <- dir_ls(type = "file", path = "data", glob = "*.parquet")

tic()
# 11,785,380 × 28 (3.36 seg)
hourly_metrics_raw <- read_parquet(file = parquet_file)
toc()

# Sitios Field Service ---------------------------------------------------------

con <- conectar_msql()

sitiosfs <- tbl(con, in_schema("fieldservice", "site_collate")) |>
 select(
  twr = cilocation,
  lat = latitude,
  lon = longitude,
  dpto = department,
  ciudad = municipality) |>
 collect() |>
 distinct(twr, .keep_all = TRUE)

dbDisconnect(con)

##  ............................................................................
##  Prep                                                                    ####

# Diagnóstico ------------------------------------------------------------------

# # Preprocesamiento y filtrado inicial
diagnosticos <- diag_00 |>
 select(fct_srvy_dt, msisdn_dd, srvy_id, class_desc, diag) |>
 mutate(
  across(fct_srvy_dt, \(fecha) ymd(fecha)),
  across(msisdn_dd:srvy_id, as.integer),
  across(diag, toupper)) |>
 filter(diag %in% etiquetas) |>

 # Dividir por 'diag' y aplicar transformaciones al grupo "NO_DIAG"
 split(~ diag) |>
 map_at("NO_DIAG", \(df) df |>
 filter(class_desc == "PROMOTOR") |>
 drop_na() |>
 slice_sample(n = 1800) |>
 distinct(msisdn_dd, .keep_all = TRUE)) |>

 # Combinar listas en un solo data frame
 list_rbind() |>

 # Transformaciones finales
 mutate(
  across(diag,
  \(x) case_match(x, "NO_DIAG" ~ "PROMOTOR", .default = diag))) |>
  select(-class_desc) |>
 distinct(msisdn_dd, .keep_all = TRUE)

# Métricas por hora ------------------------------------------------------------

metrics_to_remove <- c(
 "r1",
 "prttn_hr",
 "hora",
 "cll_prctg",
 "ra_ta_ue_index1",
 "ra_ta_ue_index2",
 "ra_ta_ue_index3",
 "ra_ta_ue_index4",
 "ra_ta_ue_index5",
 "fecha_metrica",
 "ra_ta_ue_total",
 "modulation_64qam_ratio",
 "modulation_16qam_ratio",
 "modulation_qpsk_ratio",
 "thpughput_ul",
 "volte_erlang")

# 29.64 seg
tic()
# 11,589,585 × 30
hourly_metrics <- hourly_metrics_raw |>
 mutate(
  fecha_metrica = str_sub(prttn_hr, 1, 8),
  hora = str_sub(prttn_hr, 9, 10)) |>
 mutate(
  fecha_metrica = ymd(fecha_metrica),
  date = str_c(fecha_metrica, hora, sep = " ") |> ymd_h(),
  across(fct_srvy_dt, ymd),
  across(c(msisdn_dd, hora), as.integer)
 ) |>
 filter(corrected_cqi > 5) |>
 relocate(hora, date, .after = srvy_id) |>
 select(-all_of(metrics_to_remove))
toc()

# Unir métricas detalle con diagnóstico ----------------------------------------

# 39.27 seg
tic()
# Unir diagnósticos con métricas de red
ctl <- diagnosticos |>
 inner_join(hourly_metrics, join_by(fct_srvy_dt, msisdn_dd, srvy_id)) |>

# Extraer el siteid y agregar información de ubicación
 mutate(
  twr = str_sub(bts_sh_nm, start = 2, end = 7)) |>
 left_join(sitiosfs, join_by(twr)) |>

# Renombrar y remover columnas de enlace
 rename(all_of(lookup)) |>
 select(-c(fct_srvy_dt, srvy_id)) |>

# Remover filas que tengan valor cero en varias columnas consecutivas
filter(
 !if_all(
  c(prb, load, dropr, interf, thp), ~ .x == 0)) |>

# Transformaciones finales de formato y localización de columnas
 mutate(
  across(where(is_character), \(col) estandarizar_columnas(col)),
  across(diag, as_factor)) |>
 relocate(user, where(is.POSIXct), where(is_character),
  where(is.numeric), where(is.factor)) |>
 rowwise() |>
 mutate(ta = ta6 + ta7, .keep = "unused", .after = cqi) |>
 ungroup() |>
 arrange(user, desc(date))
toc()
# 8,383,655 × 18

# Escribir en un fst para acceso rápido futuro
# write_fst(ctl, "./preprocessed_data/ctl_final.fst", compress = 0)

# 1.98 seg
ctl <- read_fst("./preprocessed_data/ctl_final.fst") |> as_tibble()



# craft -------------------------------------------------------------------

muestra <- c(30001118, 30003353)

anomalias <- tribble(
  ~user, ~date, ~prb, ~thp,
  30001118, "2023-07-03 23:00:00", 0.90, 1900,
  30001118, "2023-07-04 23:00:00", 0.91, 1300,
  30001118, "2023-07-05 23:00:00", 0.92, 500,
  30001118, "2023-07-06 22:00:00", 0.93, 800,
  30001118, "2023-07-07 10:00:00", 0.94, 1241,
  30001118, "2023-07-08 11:00:00", 0.95, 241,
  30001118, "2023-07-09 12:00:00", 0.95, 341,
  30001118, "2023-07-10 13:00:00", 0.95, 141,
  30003353, "2023-07-03 23:00:00", 0.20, 2400,
  30003353, "2023-07-04 23:00:00", 0.30, 2300,
  30003353, "2023-07-05 23:00:00", 0.40, 2200,
  30003353, "2023-07-06 22:00:00", 0.50, 2100,
  30003353, "2023-07-07 22:00:00", 0.60, 2000
) |> mutate(date = ymd_hms(date))

prueba <- ctl |>
  filter(user %in% muestra) |>
  group_by(user) |>
  slice(1:10) |>
  ungroup() |>
  select(user, date, prb, thp) |>
  bind_rows(anomalias) |>
  arrange(user, date)

prueba |>

  group_by(user) |>
  mutate(
    prb = prb * 100,
    thp = thp/1e3,
    prb_ratio = (prb / thp),
    prb_out_range = if_else(condition = prb_ratio > 32, 1, 0)) |>
  summarise(
    sum_prb_ratio  = sum(prb_out_range),
    capacity_issue = ifelse(sum_prb_ratio > 5, 1, 0))


# 1. Pasar todas las unidades a porcentaje y no a proporción: multiplicar x 100
# 2. Pasar el thp a mbps
# 3. Crear los nuevos features con base a las reglas de ctl




##  ............................................................................
##  Resumir                                                                 ####

# Crear dos datasets

tic()
# 24 seg
ctl_per_cell <- ctl |>
  group_by(user, bts) |>
  summarize_metrics() |>
  ungroup()
toc()
# 16,252 × 80

tic()
# 7.51 seg
ctl_per_user <- ctl |>
  group_by(user) |>
  summarize_metrics() |>
  ungroup()
toc()
# 4,225 × 79

##  ............................................................................
##  Split                                                                   ####




##  ............................................................................
##  EDA                                                                     ####

mi_per_cell <- information_gain(
  formula      = diag ~ .,
  data         = ctl_per_cell[-1],
  type         = "infogain",
  discIntegers = FALSE) |>
  as_tibble() |>
  arrange(-importance) |>
  rename(per_cell = importance)

mi_per_user <- information_gain(
  formula      = diag ~ .,
  data         = ctl_per_user[-1],
  type         = "infogain",
  discIntegers = FALSE) |>
  as_tibble() |>
  arrange(-importance) |>
  rename(per_user = importance)


mi_per_cell |>
  mutate(feature = fct_reorder(attributes, importance, .desc = FALSE)) |>
  ggplot(aes(x = importance, y = feature)) +
  geom_point() +
  labs(title = "MI por detalle de celda") +
  drako +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10))


mi_per_user |>
  mutate(feature = fct_reorder(attributes, importance, .desc = FALSE)) |>
  ggplot(aes(x = importance, y = feature)) +
  geom_point() +
  labs(title = "MI por usuario") +
  drako +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10))


mi_total <- mi_per_cell |>
  left_join(mi_per_user, join_by(attributes))




cubist_spec <-
  cubist_rules(committees = tune(), neighbors = tune()) %>%
  set_engine("Cubist")

























