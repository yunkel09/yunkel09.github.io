set.seed(1014)

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  fig.retina = 2,
  fig.width = 6,
  fig.asp = 2/3,
  fig.show = "hold"
)

options(pillar.sigfig    = 5,
        tibble.print_min = 10,
        scipen = 999,
        digits = 7,
        tidymodels.dark = TRUE,
        readr.show_col_types = FALSE,
        dplyr.summarise.inform = FALSE)

# options(
#   dplyr.print_min = 6,
#   dplyr.print_max = 6,
#   pillar.max_footer_lines = 2,
#   pillar.min_chars = 15,
#   stringr.view_n = 6,
#   cli.num_colors = 0,
#   cli.hyperlink = FALSE,
#   pillar.bold = TRUE,
#   width = 77, # 80 - 3 for #> comment
#   tidymodels.dark = TRUE,
#   readr.show_col_types = FALSE,
#   dplyr.summarise.inform = FALSE
# )

import::from(zeallot,       `%<-%`)
import::from(fs,            dir_ls)
import::from(fst,           read_fst, write_fst)
import::from(cowplot,       .except = "stamp")
import::from(conectigo,     cargar_fuentes)
import::from(skimr,         skim, skim_with, sfl)
import::from(patchwork,     plot_layout, plot_annotation)
import::from(ggstatsplot,   ggbetweenstats)
import::from(viridis,       scale_fill_viridis)
import::from(gtools,        combinations)
import::from(colorblindr,   scale_fill_OkabeIto)
import::from(bestNormalize, bestNormalize, step_best_normalize)
import::from(doParallel,    registerDoParallel)
import::from(nnet,          multinom)
import::from(effects,       predictorEffects)
import::from(parallel,      detectCores, makePSOCKcluster, stopCluster)
pacman::p_load(
 knitr,
 paletteer,
 tictoc,
 themis,
 scales,
 janitor,
 interactions,
 gt,
 gtExtras,
 glue,
 inspectdf,
 corrr,
 DataExplorer,
 ggpackets,
 pins,
 bonsai,
 finetune,
 tidymodels,
 tidyverse)


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
    plot.subtitle = element_text(size = 18))

theme_set(yunkel)
