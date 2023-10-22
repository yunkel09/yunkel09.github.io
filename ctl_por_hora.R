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

crear_secuencia_fechas <- function(dias, horas) {
  # Fecha de inicio
  fecha_inicio <- as.Date("2023-04-01")

  # Crear secuencia de fechas
  secuencia_fechas <- seq(from = fecha_inicio, by = "days", length.out = dias)

  sort(rep(secuencia_fechas, horas))

}



# Definir listado de funciones básicas
fun_basicas <- list(
  mean   = ~mean(.x, na.rm = TRUE),
  median = ~median(.x, na.rm = TRUE),
  sd     = ~sd(.x, na.rm = TRUE),
  mim    = ~min(.x, na.rm = TRUE),
  max    = ~max(.x, na.rm = TRUE),
  iqr    = ~IQR(.x, na.rm = TRUE),
  p1     = ~quantile(.x, probs = 0.01, na.rm = TRUE),
  p5     = ~quantile(.x, probs = 0.05, na.rm = TRUE),
  p10    = ~quantile(.x, probs = 0.10, na.rm = TRUE),
  p25    = ~quantile(.x, probs = 0.25, na.rm = TRUE),
  p75    = ~quantile(.x, probs = 0.75, na.rm = TRUE),
  p90    = ~quantile(.x, probs = 0.90, na.rm = TRUE),
  p95    = ~quantile(.x, probs = 0.95, na.rm = TRUE),
  p99    = ~quantile(.x, probs = 0.99, na.rm = TRUE),
  sk     = ~skewness(.x, na.rm = TRUE),
  kt     = ~kurtosis(.x, na.rm = TRUE)
  )


# m <- hourly_metrics_raw |>
#  filter(msisdn_dd %in% c(33483697, 30001118))
#
#
#
# m1 <- m |>
#  select(msisdn_dd, bts_sh_nm, starts_with("ra_"), -ra_ta_ue_total) |>
#  rowwise() |>
#  mutate(
#    tad = sum(c_across(ra_ta_ue_index6:ra_ta_ue_index7)),
#    tad_t = sum(c_across(ra_ta_ue_index1:ra_ta_ue_index7))) |>
#  ungroup()
#
#
# m1 |>
#   arrange(bts_sh_nm)

# Datasets de prueba ------------------------------------------------------

# baseline
test <- ctl_01 |>
  filter(user == 30001118) |>
  slice(1:126) |>
  mutate(met = crear_secuencia_fechas(dias = 7, horas = 18))


# PRB  y THP -------------------------------------------------------------------

# issue when: pbr > 80 & thp < 2.5
# horas por día: 2 | días por mes: 5
prbthp <- test |>
 select(user, met, prb, thp) |>
 group_by(user, met) |>
 mutate(
  prb = replace(prb, list = 1:15, values = 85),
  thp = replace(thp, list = 1:15, values = 2.3)) |>
 ungroup()

# Operación con PRB y THP
# horas: 2 | días: 5
capacity_flags <- prbthp |>
  mutate(
    prb_alert = as.integer(prb > 80),
    thp_alert = as.integer(thp < 2.5),
    cap_effic = (prb / thp),
    cap_alert = if_else(condition = cap_effic > 32, 1, 0)) |>
  group_by(user, met) |>
  summarise(
    prb_flag      = if_else(sum(prb_alert) >= 2, 1, 0),
    thp_flag      = if_else(sum(thp_alert) >= 2, 1, 0),
    cap_flag      = if_else(sum(cap_alert) >= 2, 1, 0)) |>
  group_by(user) |>
  summarise(
    thp_rule = if_else(sum(thp_flag) >= 5, 1, 0),
    prb_rule = if_else(sum(prb_flag) >= 5, 1, 0),
    cap_rule = if_else(sum(cap_flag) >= 5, 1, 0)
  )

# ERAB y RRC -------------------------------------------------------------------


# issue when: rrc < 90 and or erab < 90
# horas: 12 | días: 3
rrc_erab <- test |>
  select(user, met, rrc, erb) |>
  group_by(user, met) |>
  mutate(
    rrc = replace(rrc, list = 1:15, values = 85),
    erb = replace(rrc, list = 1:15, values = 35)
  ) |>
  ungroup()

rrc_erab |> summarise(rrc_out = sum(rrc < 90), erb_out = sum(erb < 90), .
                      by = met)

# Operación con RRC y ERAB
# horas 12 | días: 3
success_rate <- rrc_erab |>
  mutate(
    # Fuera del umbral
    rrc_alert = as.integer(rrc < 90),  #
    erb_alert = as.integer(erb < 90)   #
  ) |>
  group_by(user, met) |>
  summarise(
    # sum_rrc_alert  = sum(rrc_alert),
    # sum_erb_alert  = sum(erb_alert),

    # Indicador de horas
    rrc_flag = if_else(sum(rrc_alert) >= 12, 1, 0),
    erb_flag = if_else(sum(erb_alert) >= 12, 1, 0)) |>
  group_by(user) |>
  summarise(
    # Indicador de días
    rrc_rule = if_else(sum(rrc_flag) >= 3, 1, 0),
    erb_rule = if_else(sum(erb_flag) >= 3, 1, 0)
  )

# Drop Rate --------------------------------------------------------------------

# issue when: drp > 1.5
# horas: 3 | días: 3
drop_df <- test |>
  select(user, met, drp) |>
  group_by(user, met) |>
  mutate(
    drp = replace(
      drp,
      list = sample.int(n = 18, size = 7, replace = F),
      values = 1.8)
  ) |>
  ungroup()

drop_df |> summarise(drp_out = sum(drp > 1.5), .by = met)


# Operación con drop_rate
# horas: 3 | días: 3
drp_rule <- drop_df |>
  mutate(
    drp_alert = as.integer(drp > 1.5)
  ) |>
  group_by(user, met) |>
  summarise(
    sum_drp_alert  = sum(drp_alert), # por hora
    drp_flag  = if_else(sum_drp_alert >= 3, 1, 0)
  ) |>
  # ungroup() |>
  group_by(user) |>
  summarise(
    drp_rule = if_else(sum(drp_flag) >= 3, 1, 0))

# TAD---------------------------------------------------------------------------

# issue when: ta > 15
# horas: 4 | días: 7
tad_df <- test |>
  select(user, met, tad) |>
  group_by(user, met) |>
  mutate(
    tad = replace(
      tad,
      list = sample.int(n = 18, size = 7, replace = F),
      values = 20)
  ) |>
  ungroup()

tad_df |> summarise(tad_out = sum(tad > 15), .by = met)


# Operación con tad
# horas: 4 | días: 7
tad_rule <- tad_df |>
  mutate(
    tad_alert = as.integer(tad > 15)
  ) |>
  group_by(user, met) |>
  summarise(
    sum_tad_alert  = sum(tad_alert), # por hora
    tad_flag  = if_else(sum_tad_alert >= 4, 1, 0)
  ) |>
  # ungroup() |>
  group_by(user) |>
  summarise(
    tad_rule = if_else(sum(tad_flag) >= 7, 1, 0))




# Interferencia ----------------------------------------------------------------

# issue when: erf > -95
# horas: 3 | días: 3
erf_df <- test |>
  select(user, met, erf) |>
  group_by(user, met) |>
  mutate(
    erf = replace(
      erf,
      list = sample.int(n = 18, size = 7, replace = F),
      values = -83)
  ) |>
  ungroup()

erf_df |> summarise(erf_out = sum(erf > -95), .by = met)


# Operación con Interferencia
# horas: 5 | días: 3
erf_rule <- erf_df |>
  mutate(
    erf_alert = as.integer(erf > -95)
  ) |>
  group_by(user, met) |>
  summarise(
    sum_erf_alert  = sum(erf_alert), # por hora
    erf_flag  = if_else(sum_erf_alert >= 5, 1, 0)
  ) |>
  # ungroup() |>
  group_by(user) |>
  summarise(
    erf_rule = if_else(sum(erf_flag) >= 3, 1, 0))


# CQI --------------------------------------------------------------------------

# issue when: cqi < 7
# horas: 6 | días: 4
cqi_df <- test |>
  select(user, met, cqi) |>
  arrange(met) |>
  group_by(user, met) |>
  mutate(
    cqi = replace(
      cqi,
      list = sample.int(n = 18, size = 7, replace = F),
      values = 5)
  ) |>
  ungroup()

# validar cantidad de horas por día
cqi_df |> summarise(cqi_out = sum(cqi < 7), .by = met)


# Operación con cqi
# horas: 6 | días: 4
cqi_rule <- cqi_df |>
  mutate(
    cqi_alert = as.integer(cqi < 7)
  ) |>
  group_by(user, met) |>
  summarise(
    sum_cqi_alert  = sum(cqi_alert), # por hora
    cqi_flag  = if_else(sum_cqi_alert >= 6, 1, 0)
  ) |>
  # ungroup() |>
  group_by(user) |>
  summarise(
    cqi_rule = if_else(sum(cqi_flag) >= 4, 1, 0))


# Disponibilidad ---------------------------------------------------------------

# issue when: dis > 200
# horas: 1 | días: 1
dis_df <- test |>
  select(user, met, dis, dif) |>
  group_by(user, met) |>
  mutate(
    dis = replace(
      dis,
      list = sample.int(n = 18, size = 7, replace = F),
      values = 201),
    dif = replace(
      dif,
      list = sample.int(n = 18, size = 7, replace = F),
      values = 201)
  ) |>
  ungroup()

dis_df |> summarise(dis_out = sum(dis > 200), dif_out = sum(dif > 200), .by = met)


# Operación con Interferencia
# horas: 5 | días: 3
dis_rule <- dis_df |>
  mutate(
    dis_alert = as.integer(dis > 200),
    dif_alert = as.integer(dif > 200)
  ) |>
  group_by(user, met) |>
  summarise(
    sum_dis_alert  = sum(dis_alert), # por hora
    sum_dif_alert  = sum(dif_alert), # por hora
    dis_flag  = if_else(sum_dis_alert >= 1, 1, 0),
    dif_flag  = if_else(sum_dif_alert >= 1, 1, 0)
  ) |>
  # ungroup() |>
  group_by(user) |>
  summarise(
    dis_rule = if_else(sum(dis_flag) >= 1, 1, 0),
    dif_rule = if_else(sum(dif_flag) >= 1, 1, 0)
  )




# Dataset Final ----------------------------------------------------------------


data_prueba <- test |>
  select(user, met, prb:dif)

crear_business_rules <- function(df) {
 df |>
  mutate(
    eff = (prb / thp), # relación entre prb y thp
    prb_alert = as.integer(prb > 80),
    thp_alert = as.integer(thp < 2.5),
    cap_alert = as.integer(eff > 32),
    rrc_alert = as.integer(rrc < 90),
    erb_alert = as.integer(erb < 90),
    drp_alert = as.integer(drp > 1.5),
    tad_alert = as.integer(tad > 15),
    erf_alert = as.integer(erf > -95),
    cqi_alert = as.integer(cqi < 7),
    dis_alert = as.integer(dis > 200),
    dif_alert = as.integer(dif > 200)
  ) |>
  group_by(user, met) |>
  summarise(
    # Indicador de umbrales de horas al día como mínimo
    prb_flag = if_else(sum(prb_alert) >= 2, 1, 0),
    thp_flag = if_else(sum(thp_alert) >= 2, 1, 0),
    cap_flag = if_else(sum(cap_alert) >= 2,  1, 0),
    rrc_flag = if_else(sum(rrc_alert) >= 12, 1, 0),
    erb_flag = if_else(sum(erb_alert) >= 12, 1, 0),
    drp_flag = if_else(sum(drp_alert) >= 3,  1, 0),
    tad_flag = if_else(sum(tad_alert) >= 4, 1, 0),
    erf_flag = if_else(sum(erf_alert) >= 5, 1, 0),
    cqi_flag = if_else(sum(cqi_alert) >= 6, 1, 0),
    dis_flag = if_else(sum(dis_alert) >= 1, 1, 0),
    dif_flag = if_else(sum(dif_alert) >= 1, 1, 0)
  ) |>
  group_by(user) |>
  summarise(
    # Indicador de días al mes con el problema
    prb_rule = if_else(sum(prb_flag) >= 5, 1, 0),
    thp_rule = if_else(sum(thp_flag) >= 5, 1, 0),
    cap_rule = if_else(sum(cap_flag) >= 5, 1, 0),
    rrc_rule = if_else(sum(rrc_flag) >= 3, 1, 0),
    erb_rule = if_else(sum(erb_flag) >= 3, 1, 0),
    drp_rule = if_else(sum(drp_flag) >= 3, 1, 0),
    tad_rule = if_else(sum(tad_flag) >= 7, 1, 0),
    erf_rule = if_else(sum(erf_flag) >= 3, 1, 0),
    cqi_rule = if_else(sum(cqi_flag) >= 4, 1, 0),
    dis_rule = if_else(sum(dis_flag) >= 1, 1, 0),
    dif_rule = if_else(sum(dif_flag) >= 1, 1, 0)
  )
}


reglas_de_negocio_df <- data_prueba |>
  crear_business_rules()

data_prueba |>
  group_by(user) |>
  summarise(across(where(is.numeric), fun_basicas)) %>% glimpse()


 data_prueba |>
    group_by(user) |>
    summarise(

      across(where(is.numeric), fun_basicas),

      prb_timeout = mean(prb > 80),
      prb_counter = sum(prb  > 80),

      thp_timeout = mean(thp < 2.5),
      thp_counter = sum(thp  < 2.5),

      rrc_timeout = mean(rrc < 90),
      rrc_counter = sum(rrc  < 90),

      erb_timeout = mean(erb < 90),
      erb_counter = sum(erb  < 90),

      drp_timeout = mean(drp > 1.5),
      drp_counter = sum(drp  > 1.5),

      tad_timeout = mean(tad > 15),
      tad_counter = sum(tad  > 15),

      lod_timeout = mean(lod > 77),
      lod_counter = sum(lod  > 77),

      erf_timeout = mean(erf > -95),
      erf_counter = sum(erf  > -95),

      cqi_timeout = mean(cqi < 7),
      cqi_counter = sum(cqi  < 7),

      dis_timeout = mean(dis > 200),
      dif_counter = sum(dis  > 200)

  ) %>% glimpse()
  left_join(reglas_de_negocio_df, by = join_by(user)) %>% glimpse()


# 246 columnas
test |>
  group_by(user) |>
  summarize_metrics() |>
  left_join(reglas_de_negocio_df, by = join_by(user)) %>% glimpse()


##  Resumir                                                                 ####

tic()
# 241 seg (4 min)
ctl_per_user <- ctl_01 |>
  group_by(user) |>
  summarize_metrics() |>
  ungroup() |>
  left_join(reglas_de_negocio_df, by = join_by(user))
toc()
# 4,244 × 216


##  ............................................................................
##  Split                                                                   ####




##  ............................................................................
##  EDA                                                                     ####


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
  mutate(feature = fct_reorder(attributes, per_user, .desc = FALSE)) |>
  ggplot(aes(x = per_user, y = feature)) +
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

























