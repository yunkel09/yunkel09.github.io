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

options(
  dplyr.print_min = 6,
  dplyr.print_max = 6,
  pillar.max_footer_lines = 2,
  pillar.min_chars = 15,
  stringr.view_n = 6,
  # Temporarily deactivate cli output for quarto
  cli.num_colors = 0,
  cli.hyperlink = FALSE,
  pillar.bold = TRUE,
  width = 77, # 80 - 3 for #> comment
  tidymodels.dark = TRUE,
  readr.show_col_types = FALSE,
  dplyr.summarise.inform = FALSE
)

import::from(zeallot,       `%<-%`)
import::from(fs,            dir_ls)
import::from(fst,           read_fst, write_fst)
import::from(cowplot,       .except = "stamp")
import::from(conectigo,     cargar_fuentes)
import::from(DataExplorer,  plot_intro, plot_missing)
import::from(skimr,         skim, skim_with, sfl)
import::from(moments,       skewness, kurtosis)
pacman::p_load(knitr, paletteer, scales, janitor, gt, gtExtras, tidyverse)


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

