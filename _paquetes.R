

import::from(zeallot,       `%<-%`)
import::from(fs,            dir_ls)
import::from(parallel,      detectCores, makePSOCKcluster, stopCluster)
import::from(doParallel,    registerDoParallel)
import::from(conectigo,     conectar_msql)
import::from(DBI,           dbDisconnect)
import::from(dbplyr,        in_schema)

pacman::p_load(
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
 tidymodels,
 tidyverse)


options(pillar.sigfig    = 5,
        tibble.print_min = 10,
        scipen = 999,
        digits = 7,
        tidymodels.dark = TRUE,
        readr.show_col_types = FALSE,
        dplyr.summarise.inform = FALSE)

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
