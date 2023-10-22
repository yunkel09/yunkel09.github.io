

import::from(zeallot,       `%<-%`)
import::from(fs,            dir_ls, path)
import::from(parallel,      detectCores, makePSOCKcluster, stopCluster)
import::from(doParallel,    registerDoParallel)
import::from(conectigo,     cargar_fuentes)
# import::from(DBI,           dbDisconnect)
import::from(moments,        skewness, kurtosis)
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
        tibble.print_min = 30,
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
  iconv(from = "UTF-8", to = "latin1") |>
  iconv(from = "latin1", to = "ASCII//TRANSLIT") |>
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
  city = first(city),

  across(prb:dif, fun_basicas),

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
  dif_counter = sum(dis  > 200),

  lat = first(lat),
  lon = first(lon),

  # coordenadas polares
  r = sqrt(lat^2 + lon^2),  # radio
  theta = atan2(lon, lat),  # ángulo

  diag = first(diag)

  )
}



















