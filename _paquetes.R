

import::from(zeallot,       `%<-%`)
import::from(fs,            dir_ls, path)
import::from(parallel,      detectCores, makePSOCKcluster, stopCluster)
import::from(doParallel,    registerDoParallel)
import::from(conectigo,     cargar_fuentes)
import::from(corrplot,      corrplot)
import::from(DBI,           dbDisconnect)
import::from(dbplyr,        in_schema)
import::from(rlang,         expr)
import::from(ggforce,       geom_autodensity, facet_matrix)
import::from(bestNormalize, bestNormalize, step_best_normalize, step_orderNorm)
import::from(FSelectorRcpp, information_gain)
import::from(cowplot,       .except = "stamp")

pacman::p_load(
 # file readers
 arrow,
 fst,

 paletteer,
 tictoc,
 themis,
 janitor,
 pins,
 bonsai,
 baguette,
 finetune,

 gt,
 gtExtras,

 colino,
 textrecipes,
 # spatialsample,
 embed,
 butcher,
 vip,
 agua,
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

theme_set(drako)


tablero_ctl <- board_folder(path = "tablero_ctl")

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
  p99    = ~quantile(.x, probs = 0.99, na.rm = TRUE)
)


crear_business_rules <- function(df) {
  df |>
    mutate(
      # relación entre prb y thp
      eff = if_else(is.infinite(prb / thp), 0, (prb / thp)),
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

# Función para sumarizar métricas
summarize_metrics <- function(df) {
 df |>
  summarise(

  twr = first(twr),
  dpto = first(dpto),
  city = first(city),

  across(prb:lte, fun_basicas),

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

  psk_timeout = mean(psk > m64),
  psk_counter = sum(psk > m64),

  dis_timeout = mean(dis > 0),
  dif_counter = sum(dis  > 0),

  lte_timeout = mean(lte <= 95),
  lte_counter = sum(lte  <= 95),

  lat = first(lat),
  lon = first(lon),

  # coordenadas polares
  r = sqrt(lat^2 + lon^2),  # radio
  theta = atan2(lon, lat),  # ángulo

  diag = first(diag2)

  )
}


paleta <-c(
  "#D56702FF",
  "#AD8875FF",
  "#DB1C6AFF",
  "#4092E1FF",
  "#02AF44FF",
  "#D7B5A6",
  "#FCCD1BFF",
  "#BAB0AC",
  "#1B8EC4FF",
  "#E64B35FF")

tableau <- c("#4E79A7", "#A0CBE8", "#F28E2B", "#FFBE7D", "#59A14F",
             "#8CD17D", "#B6992D", "#F1CE63", "#499894", "#86BCB6", "#E15759",
             "#FF9D9A", "#79706E", "#BAB0AC", "#D37295", "#FABFD2", "#B07AA1",
             "#D4A6C8", "#9D7660", "#D7B5A6")

allcolors <- c(paleta, tableau)



mean_ci <- function(df, metrica, conf.level = 0.95) {
  x  <- df[[metrica]][!is.na(df[[metrica]])]
  m  <- mean(x)
  ci <- t.test(x)$conf.int
  data.frame(media = m, lower = ci[1], upper = ci[2])
}


# resumen estadístico versátil
resumir <- function(.df) {
 my_skim <- skim_with(
  base = NULL,
  numeric = sfl(media   = ~ mean(., na.rm = TRUE),
                mediana = ~ median(., na.rm = TRUE),
                maximo  = ~ max(., na.rm = TRUE),
                minimo  = ~ min(., na.rm = TRUE),
                sd = ~ sd(., na.rm = TRUE)
                # varianza = ~ var(., na.rm = TRUE),
                # iqr = ~ IQR(., na.rm = TRUE),
                # skewness = ~ skewness(., na.rm = TRUE),
                # kurtosis = ~ kurtosis(., na.rm = TRUE)
  ), append = F)
 my_skim(.df) |>
  rename_with(~ str_replace_all(.x, "numeric\\.", "")) |>
  as_tibble() |>
  select(-skim_type) |>
  rename(variable = skim_variable)
}









