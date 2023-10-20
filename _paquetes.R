

import::from(zeallot,       `%<-%`)
import::from(fs,            dir_ls)
import::from(parallel,      detectCores, makePSOCKcluster, stopCluster)
import::from(doParallel,    registerDoParallel)
import::from(conectigo,     conectar_msql, cargar_fuentes)
import::from(DBI,           dbDisconnect)
import::from(dbplyr,        in_schema)
import::from(FSelectorRcpp, information_gain)
import::from(cowplot,       .except = "stamp")

pacman::p_load(
 fst,
 tictoc,
 themis,
 janitor,
 pins,
 bonsai,
 baguette,
 finetune,
 arrow,
 colino,
 textrecipes,
 spatialsample,
 tidymodels,
 tidyverse)


options(pillar.sigfig    = 5,
        tibble.print_min = 50,
        scipen = 999,
        digits = 7,
        tidymodels.dark = TRUE,
        readr.show_col_types = FALSE,
        dplyr.summarise.inform = FALSE)


cargar_fuentes()

yunkel <- theme_cowplot(font_family = "yano") +
  theme(
    plot.margin          = unit(c(10, 2, 2, 2), "mm"),
    axis.title           = element_text(size = 30),
    axis.text            = element_text(size = 20),
    plot.title           = element_text(size = 50),
    plot.subtitle        = element_text(size = 18),
    legend.position      = "bottom",
    legend.text          = element_text(size = 20),
    legend.justification = "center",
    legend.title         = element_blank())

# tema con grid horizontal y vertical
drako <- theme_bw(base_family = "yano", base_size = 14) +
  theme(
    plot.margin = unit(c(10, 2, 2, 2), "mm"),
    axis.title    = element_text(size = 30),
    axis.text     = element_text(size = 20),
    plot.title    = element_text(size = 50),
    plot.subtitle = element_text(size = 18),
    legend.title  = element_blank())

theme_set(yunkel)



# preparar y desplegar receta
ver <- . %>% prep() %>% juice()

# detener el backend
unregister <- function() {
 env <- foreach:::.foreachGlobals
 rm(list=ls(name=env), pos=env)
}

estandarizar_columnas <- function(columna) {
 columna |>
  toupper() |>
  str_replace_all("[\\s-]", "_")
}


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


# Función para sumarizar métricas
summarize_metrics <- function(df) {
 df |>
  summarise(

  twr = first(twr),
  dpto = first(dpto),
  ciudad = first(ciudad),

  prb_mean = mean(prb),
  prb_median = median(prb),
  prb_min = min(prb),
  prb_max = max(prb),
  prb_sd = sd(prb),
  prb_out_of_range_time = mean(prb > 0.85) * 100,
  prb_out_of_range_count = sum(prb > 0.85),
  prb_out_of_range_bin = as.integer(mean(prb > 0.85) * 100 >= 3),

  load_mean = mean(rrc),
  load_median = median(load),
  load_min = min(load),
  load_max = max(load),
  load_sd = sd(load),
  load_out_of_range_time = mean(load > 0.77) * 100,
  load_out_of_range_count = sum(load > 0.77),
  load_out_of_range_bin = as.integer(mean(load > 0.77) * 100 >= 3),

  rrc_mean = mean(rrc),
  rrc_median = median(rrc),
  rrc_min = min(rrc),
  rrc_max = max(rrc),
  rrc_sd = sd(rrc),
  rrc_out_of_range_time = mean(rrc < 0.9) * 100,
  rrc_out_of_range_count = sum(rrc < 0.9),
  rrc_out_of_range_bin = as.integer(mean(rrc < 0.9) * 100 >= 3),

  erab_mean = mean(erab),
  erab_median = median(erab),
  erab_min = min(erab),
  erab_max = max(erab),
  erab_sd = sd(erab),
  erab_out_of_range_time = mean(erab < 0.9) * 100,
  erab_out_of_range_count = sum(erab < 0.9),
  erab_out_of_range_bin = as.integer(mean(erab < 0.9) * 100 >= 3),

  dropr_mean = mean(dropr),
  dropr_median = median(dropr),
  dropr_min = min(dropr),
  dropr_max = max(dropr),
  dropr_sd = sd(dropr),
  dropr_out_of_range_time = mean(dropr > 0.015) * 100,
  dropr_out_of_range_count = sum(dropr > 0.015),
  dropr_out_of_range_bin = as.integer(mean(dropr > 0.015) * 100 >= 3),

  interf_mean = mean(interf),
  interf_median = median(interf),
  interf_min = min(interf),
  interf_max = max(interf),
  interf_sd = sd(interf),
  interf_out_of_range_time = mean(interf > -95) * 100,
  interf_out_of_range_count = sum(interf > -95),
  interf_out_of_range_bin = as.integer(mean(interf > -95) * 100 >= 3),

  thp_dl_mean = mean(thp_dl),
  thp_dl_median = median(thp_dl),
  thp_dl_min = min(thp_dl),
  thp_dl_max = max(thp_dl),
  thp_dl_sd = sd(thp_dl),
  thp_dl_out_of_range_time = mean(thp_dl < 2.7) * 100,
  thp_dl_out_of_range_count = sum(thp_dl < 2.7),
  thp_dl_out_of_range_bin = as.integer(mean(thp_dl < 2.7) * 100 >= 3),

  cqi_mean = mean(cqi),
  cqi_median = median(cqi),
  cqi_min = min(cqi),
  cqi_max = max(cqi),
  cqi_sd = sd(cqi),
  cqi_out_of_range_time = mean(cqi < 7) * 100,
  cqi_out_of_range_count = sum(cqi < 7),
  cqi_out_of_range_bin = as.integer(mean(cqi < 7) * 100 >= 3),

  ta_mean = mean(ta),
  ta_median = median(ta),
  ta_min = min(ta),
  ta_max = max(ta),
  ta_sd = sd(ta),
  ta_out_of_range_time = mean(ta > 0.15) * 100,
  ta_out_of_range_count = sum(ta > 0.15),
  ta_out_of_range_bin = as.integer(mean(ta > 0.15) * 100 >= 3),

  lat = first(lat),
  lon = first(lon),

  diag = first(diag)

  )
}



















