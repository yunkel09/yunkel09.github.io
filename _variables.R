#   ____________________________________________________________________________
#   Variables Globales                                                      ####

# Fecha: 2023-11-01
# Autor: William Chavarría


centinelas <- c("TBD", "TO_BE_DETERMINED")

# Etiquetas para variable respuesta
etiquetas <- c(
        "CAPACIDAD",
        "OPTIMIZACION",
        "COBERTURA",
        "DISPONIBILIDAD",
        "NO_DIAG")

# Tabla para renombrar columnas con nombres más cortos
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
        "ra_ta_ue_index1",
        "ra_ta_ue_index2",
        "ra_ta_ue_index3",
        "ra_ta_ue_index4",
        "ra_ta_ue_index5",
        "ra_ta_ue_index6",
        "ra_ta_ue_index7",
        "ra_ta_ue_total",
        "thpughput_ul",
        "cell_unavail_s1fail")

# Atributos de tipo entero
enteros <- c(
        "msisdn_dd",
        "corrected_cqi",
        "cell_unavail",
        "cell_unavail_s1fail")

# Atributos de tipo ratio que deben ser pasado a porcentaje
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

# Tipos de columnas en tabla maestra 'diagnósticos'
cols_diag <- cols_only(
 fct_srvy_dt = "c",
 msisdn_dd   = "c",
 srvy_id     = "i",
 time_lte    = "d",
 class_desc  = "c",
 diag        = "c")

# Rutas
diag_f <- path("data", "diags", ext = "csv")
# metr_f <- path("data", "metri", ext = "fst")
metr_f <- path("data", "metricas_tiny", ext = "fst")
sifs_f <- path("data", "sitfs", ext = "fst")



paleta <-c(
 "#D56702FF", "#AD8875FF", "#DB1C6AFF", "#4092E1FF", "#02AF44FF", "#D7B5A6",
 "#FCCD1BFF", "#BAB0AC",   "#1B8EC4FF", "#E64B35FF")

tableau <- c(
 "#4E79A7", "#A0CBE8", "#F28E2B", "#FFBE7D", "#59A14F", "#8CD17D", "#B6992D",
 "#F1CE63", "#499894", "#86BCB6", "#E15759", "#FF9D9A", "#79706E", "#BAB0AC",
 "#D37295", "#FABFD2", "#B07AA1", "#D4A6C8", "#9D7660", "#D7B5A6")

allcolors <- c(paleta, tableau)

# ---------------------------------------------------------------------------- #
