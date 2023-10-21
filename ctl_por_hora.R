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
  "r1",
  "prttn_hr",
  "status",
  "cll_prctg",
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
 diag        = "c")

# Leer el archivo
diag_00 <- read_csv(diag_f, col_types = cols_diag)

# Métricas por hora ------------------------------------------------------------

# 16.61 seg
hourly_metrics_raw <- read_fst(path = metr_f) |> as_tibble()

# Sitios Field Service ---------------------------------------------------------

# 5,412
sitiosfs <- read_fst(sifs_f) |> as_tibble()

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

# Unir y transformar -----------------------------------------------------------

# 18.2 seg
ctl_00 <- diagnosticos |>
  inner_join(hourly_metrics_raw, join_by(fct_srvy_dt, msisdn_dd, srvy_id))
# 32,164,414 × 31


# Dataset exploratorio ---------------------------------------------------------

# 253 seg (4.23 min)
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
 rename(all_of(lookup)) |>

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
  diag)
toc()
# 31,648,712 × 23

# Guardar dataset preprocesado
# write_fst(ctl_01, path = cted_f, compress = 0)


# Cargar dataset preprocesado --------------------------------------------------

# 6.34 seg
cted_f <- path("data", "ctlexp", ext = "fst")
ctl_01 <- read_fst(cted_f) |> as_tibble()

##  ............................................................................
##  PRE-EDA                                                                 ####

# is_whole <- function(x) all(floor(x) == x)
#
#
# ctl_01 |>
#   select(where(is.numeric)) |>
#   summarise(across(everything(), is_whole))

ctl_01 |>
 summarise(across(c(met:hor, bts), n_distinct), .by = user)


# craft -------------------------------------------------------------------



# Datasets de prueba ------------------------------------------------------


# thp y prb
muestra <- c(30001118, 30003353)

anomalias <- tribble(
  ~user,    ~date,                 ~prb, ~thp,
  30001118, "2023-07-03 23:00:00", 90,   1.900,
  30001118, "2023-07-03 15:00:00", 91,   1.300,
  30001118, "2023-07-05 23:00:00", 92,   0.500,
  30001118, "2023-07-05 22:00:00", 70,   0.800,
  30001118, "2023-07-06 10:00:00", 94,   1.241,
  30001118, "2023-07-06 11:00:00", 95,   2.41,
  30001118, "2023-07-07 12:00:00", 95,   3.41,
  30001118, "2023-07-08 12:00:00", 95,   1.41,
  30001118, "2023-07-08 13:00:00", 95,   1.41,
  30001118, "2023-07-09 14:00:00", 95,   1.41,
  30001118, "2023-07-09 15:00:00", 95,   1.41,
  30001118, "2023-07-10 16:00:00", 95,   0.41,
  30001118, "2023-07-11 17:00:00", 95,   1.41
  # 30003353, "2023-07-16 23:00:00", 20,   52.400,
  # 30003353, "2023-07-16 11:00:00", 30,   83.00,
  # 30003353, "2023-07-05 23:00:00", 40,   2.200,
  # 30003353, "2023-07-06 22:00:00", 50,   2.100,
  # 30003353, "2023-07-07 22:00:00", 60,   2.000
) |> mutate(
  date = ymd_hms(date))

prueba <- ctl_01 |>
  # filter(user %in% muestra) |>
  filter(user == 30001118) |>
  group_by(user) |>
  slice(1:10) |>
  ungroup() |>
  select(user, date, prb, thp) |>
  bind_rows(anomalias) |>
  mutate(fecha = as.Date(date), .after = date) |>
  select(-date) |>
  arrange(user, fecha)

prueba

# erab y rrc
eb <- ctl_01 |>
  filter(user == 30001118) |>
  slice(1:18) |>
  select(user, met, hor, rrc, erb) |>
  arrange(user, met) |>
  mutate(
    met = as.Date("2023-04-22"),
    rrc = c(rep(75, 12), rep(90, 6)), erb = c(rep(75, 11), rep(90, 7)))


# Funciones básicas ------------------------------------------------------------

# Definir listado de funciones básicas
fun_basicas <- list(
  mean   = ~mean(.x, na.rm = TRUE),
  median = ~median(.x, na.rm = TRUE),
  sd     = ~sd(.x, na.rm = TRUE),
  mim    = ~min(.x, na.rm = TRUE),
  max    = ~max(.x, na.rm = TRUE))

# Resúmenes --------------------------------------------------------------------


# Operación con PRB y THP
capacity_flags <- prueba |>
  mutate(
    capacity_efficienty = (prb / thp),
    capacity_alert = if_else(condition = capacity_efficienty > 32, 1, 0)) |>
  group_by(user, fecha) |>
    summarise(
      sum_prb_ratio  = sum(capacity_alert),
      capacity_issue = ifelse(sum_prb_ratio >= 2, 1, 0)) |>
  group_by(user) |>
  summarise(cap_issues = ifelse(sum(capacity_issue) >= 5, 1, 0))


# Operación con RRC y ERAB
success_rate <- eb |>
  mutate(
    rrc_alert = as.integer(rrc < 90),
    erb_alert = as.integer(erb < 90)
  ) |>
  group_by(user, met) |>
  summarise(
    sum_rrc_alert  = sum(rrc_alert),
    sum_erb_alert  = sum(erb_alert)) |>
  group_by(user) |>
  summarise(
    rrc_issues = if_else(sum_rrc_alert >= 12, 1, 0),
    erb_issues = if_else(sum_erb_alert >= 12, 1, 0)
  )

# Dataset Final ----------------------------------------------------------------

  usuario <- ctl_01 |>
  filter(user == 30001118) |>
  slice(1:10)

 usuario |>
    group_by(user) |>

    summarise(

      across(prb:erb, fun_basicas),
      prb_timeout = mean(prb > 80),
      prb_counter = sum(prb > 80),

      # across(thp, fun_basicas),
      thp_timeout = mean(thp < 2.5),
      thp_counter = sum(thp < 2.5)


  ) |>
  left_join(capacity_flags, join_by(user)) |>
  left_join(success_rate, join_by(user)) %>% glimpse()


# Aquí me quedé

 # Continuar con el resto de métricas y la lógica de los días
 # Escribir a André para que validen bien los muchachos por correo





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

























