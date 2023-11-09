#   ____________________________________________________________________________
#   Paquetes                                                                ####

# Fecha: 2023-11-01
# Autor: William Chavarría


import::from(zeallot,       `%<-%`)
import::from(fs,            dir_ls, path)
import::from(conectigo,     cargar_fuentes)
import::from(corrplot,      corrplot)
import::from(ggforce,       geom_autodensity, facet_matrix, geom_autopoint)
import::from(nnet,          multinom)
import::from(patchwork,     plot_layout, plot_annotation)
import::from(finetune,      plot_race)
import::from(effects,       predictorEffects)
import::from(bestNormalize, step_orderNorm, bestNormalize)
import::from(FSelectorRcpp, information_gain)
import::from(viridis,       scale_fill_viridis)
import::from(scales,        label_percent)
import::from(ggpackets,     ggpacket)
import::from(rlang,         expr)
import::from(skimr,         skim, skim_with, sfl)
import::from(lightgbm,      lgb.importance, lgb.plot.importance)
import::from(cowplot,       .except = "stamp")
import::from(ggforce,       geom_autodensity, facet_matrix)
import::from(ggridges,      geom_density_ridges_gradient, theme_ridges)

pacman::p_load(
 fs,
 fst,
 DataExplorer,
 glue,
 inspectdf,
 corrr,
 paletteer,
 themis,
 janitor,
 pins,
 gt,
 gtExtras,
 finetune,
 bonsai,
 colino,
 # embed,
 # vip,
 tidymodels,
 tidyverse)

cargar_fuentes()

# Guardar objetos
tablero_ctl <- board_folder(path = "tablero_ctl")

# ---------------------------------------------------------------------------- #









