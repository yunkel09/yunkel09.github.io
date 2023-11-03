
#   ____________________________________________________________________________
#   Configuraciones                                                         ####

# Fecha: 2023-11-01
# Autor: William Chavarría

options(
 pillar.sigfig    = 5,
 tibble.print_min = 30,
 scipen = 999,
 digits = 7,
 tidymodels.dark = TRUE,
 readr.show_col_types = FALSE,
 dplyr.summarise.inform = FALSE)

# Fuentes William
cargar_fuentes()

# Tema limpio
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

# Tema con grid horizontal y vertical
drako <- theme_bw(base_family = "yano", base_size = 14) +
 theme(
 plot.margin = unit(c(10, 2, 2, 2), "mm"),
 axis.title    = element_text(size = 30),
 axis.text     = element_text(size = 20),
 plot.title    = element_text(size = 50),
 plot.subtitle = element_text(size = 18),
 legend.title  = element_blank())

# Establer tema por defecto
theme_set(drako)

# ---------------------------------------------------------------------------- #