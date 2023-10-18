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
        "cell_load_lte",    # La métrica time_cl y time_lte son mejores
        "trrtry_cmrcl_nm",  # No determina la ubicación de la celda
        "stt_nm",           # No determina la ubicación de la celda
        "trrtry_tf_nm",     # No determina la ubicación de la celda
        "cty_nm",           # No determina la ubicación de la celda
        "thp_required_lte"  # Métrica sumarizada
)


# Métricas por hora ------------------------------------------------------------

parquet_file <- dir_ls(type = "file", path = "data", glob = "*.parquet")

# 659,966 x 28
kpis_00 <- read_parquet(file = parquet_file)


# Sitios Field Service ---------------------------------------------------------

con <- conectar_msql()

sitiosfs <- tbl(con, in_schema("fieldservice", "site_collate")) |>
  select(twr = cilocation, status, lat = latitude, lon = longitude,
         dpto = department, municipalidad = municipality) %>%
  collect() |>
  distinct(twr, .keep_all = TRUE)

dbDisconnect(con)

##  ............................................................................
##  Prep                                                                    ####


# Diagnóstico ------------------------------------------------------------------

# 72,644 × 6
diag_01 <- diag_00 |>
 select(
  where(
   \(x) length(unique(x)) != 1 &&
   mean(is.na(x)) < 0.5),
   -all_of(cols_to_remove)
 ) |>
 mutate(
  across(fct_srvy_dt, \(fecha) ymd(fecha)),
  across(where(is.character), \(col) estandarizar_columnas(col)),
  across(msisdn_dd:srvy_id, as.integer)
 ) |>
 relocate(fct_srvy_dt, msisdn_dd, srvy_id)

# Segunda transformación
diag_02 <- diag_01 |>
 arrange(fct_srvy_dt, msisdn_dd, srvy_id) |>
 mutate(
  across(where(is.character), \(x) na_if(x, "TBD")),
  across(where(is.character), \(x) na_if(x, "TO_BE_DETERMINED")))

# Tercera transformación: 6,587 x 8
diag_03 <- diag_02 |>
 filter(diag %in% etiquetas) |>
 split(~ diag) |>
 map_at("NO_DIAG", \(df) df |>
 filter(class_desc == "PROMOTOR") |>
 drop_na() |>
 slice_sample(n = 1800) |>
 distinct(msisdn_dd, .keep_all = TRUE)) |>
 list_rbind() |>
 mutate(
  across(diag,
  \(x) case_match(x, "NO_DIAG" ~ "PROMOTOR", .default = diag))) |>
  select(-class_desc) |>
 distinct(msisdn_dd, .keep_all = TRUE)


# Métricas resumidas -----------------------------------------------------------


excluir_metricas <- c("cell_load", "thpughput_ul", "volte_erlang")

# 273,434 x 24
metr_01 <- metr_00 |>
 mutate(across(fct_srvy_dt, ymd)) |>
 relocate(fct_srvy_dt, msisdn_dd, srvy_id, r1, bts_sh_nm) |>
 arrange(fct_srvy_dt, msisdn_dd) |>
 select(-all_of(excluir_metricas))

metr_01 |>
 filter(msisdn_dd == 31324863) |>
 select(fct_srvy_dt, msisdn_dd, bts_sh_nm, cll_prctg, rate_prb_dl)


# Métricas detalle -------------------------------------------------------------

# 659,966 x 30
kpis_01 <- kpis_00 |>
 separate_wider_position(prttn_hr, c(fecha_metrica = 8, hora = 2)) |>
 mutate(
  fecha_metrica = ymd(fecha_metrica),
  timestamp = str_c(fecha_metrica, hora, sep = " ") |> ymd_h(),
  across(fct_srvy_dt, ymd),
  across(msisdn_dd, as.integer)
 ) |>
 relocate(hora, timestamp, .after = srvy_id)

# 483,933 × 35
kpis_02 <- diag_03 |>
 inner_join(kpis_01, join_by(fct_srvy_dt, msisdn_dd, srvy_id))

kpis_03 <- kpis_02 |>
 select(fct_srvy_dt:cty_nm, bts_sh_nm) |>
 mutate(twr = str_sub(bts_sh_nm, start = 2, end = 7)) |>
 left_join(sitiosfs, join_by(twr))

## Mapa

theme_set(theme_light())
library(ggthemes)

mapa <- diag_03 |>
 inner_join(lluv_01, join_by(fct_srvy_dt, msisdn_dd, srvy_id))

rangos <- map_data("world") |>  as_tibble() |>
 filter(region == "Guatemala")  |>
 reframe(rango_lat = range(lat), rango_lon = range(long))


just_gt <- kpis_03 |>
 filter(stt_nm == "GUATEMALA") |>
 slice_sample(n = 5000) |>
 drop_na()

just_gt |>
ggplot(aes(lon, lat)) +
 geom_point() +
 borders(regions = "Guatemala") +
 theme_map()


just_gt |>
 filter(str_detect(twr, "^GUA|^GTA", negate = TRUE))


## Mapa

theme_set(theme_light())
library(ggthemes)

mapa <- diag_03 |>
 inner_join(lluv_01, join_by(fct_srvy_dt, msisdn_dd, srvy_id))

rangos <- map_data("world") |>  as_tibble() |>
 filter(region == "Guatemala")  |>
 reframe(rango_lat = range(lat), rango_lon = range(long))


just_gt <- mapa |>
 filter(stt_nm == "GUATEMALA") |>
 slice_sample(n = 5000) |>
 drop_na()


ggplot(aes(lon, lat)) +
 geom_point() +
 borders(regions = "Guatemala") +
 # scale_size_area() +
 # coord_quickmap() +
 theme_map()

map_data("world") |>
 as_tibble() |>
 count(region) |> print(n = Inf)muestra <- c(
        30299503,
        31462506,
        32063433,
        55261666,
        31530772,
        45644345,
        53596846,
        46915988,
        31139092,
        38369266)

# 30299503 (promotor)
# 31462506 (promotor)

# 32063433 (optimización)
# 55261666 (optimización)

# 31530772 (disponibilidad)
# 45644345 (disponibilidad)

# 53596846 (cobertura)
# 46915988 (cobertura)

# 31139092 (capacidad)
# 38369266 (capacidad)


diag_x <- diag_03 |>
 filter(msisdn_dd %in% muestra) |>
 select(fct_srvy_dt, msisdn_dd, srvy_id, diag)
#
usuario_x <- kpis_01 |>
 filter(msisdn_dd %in% muestra) |>
 select(fct_srvy_dt, timestamp, msisdn_dd, srvy_id, bts_sh_nm,
        fecha_metrica, hora, cll_prctg, rate_prb_dl) |>
 mutate(across(msisdn_dd, as.integer),
        across(fct_srvy_dt, ymd)) |>
 arrange(timestamp)


review <- diag_x |>
 left_join(usuario_x, join_by(msisdn_dd, srvy_id, fct_srvy_dt)) |>
 relocate(diag, .after = last_col())


review |> inspectdf::inspect_na()
review

review |>
 group_by(msisdn_dd) |>
 summarise(across(bts_sh_nm:hora, n_distinct),
           diag = first(diag)) |>
 relocate(diag, .after = msisdn_dd) |>
 arrange(diag)

review |>
 group_by(msisdn_dd, fecha_metrica) |>
 summarise(across(c(bts_sh_nm, hora), n_distinct),
                  diag = first(diag)) |>
 relocate(diag, .after = msisdn_dd) |>
 arrange(diag) |>
 ungroup()


## Disponibilidad

disp_01 <- disp_00 |>
 mutate(across(everything(), \(x) na_if(x, -1))) |>
 select(-time_thrgpt)




ctl_00 <- metr_01 |>
 inner_join(disp_01, join_by(msisdn_dd, srvy_id)) |>
 inner_join(lluv_01, join_by(msisdn_dd, srvy_id, fct_srvy_dt, bts_sh_nm))




tbl(con, in_schema("fieldservice", "directory_siteattribute"))

## Lluvia

lluv_01 <- lluv_00 |>
 select(-fct_srvy_15_mnth) |>
 mutate(across(fct_srvy_dt, ymd)) |>
 left_join(sitiosfs, join_by(twr))




