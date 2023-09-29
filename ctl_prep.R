#   ________________________________________________________________________
#   Proyecto                                                            ####

# Título: CTL
# Objetivo:Análisis exploratorio de set de datos preliminares
# Fecha: 2023-09-05
# Autor: William Chavarría

# Cargar librerías
import::from(caret,       nearZeroVar)
import::from(zeallot,     `%<-%`)
import::from(car,         vif)
import::from(corrplot,    corrplot)
import::from(inspectdf,   inspect_na)
import::from(cowplot,     .except = "stamp")
import::from(fs,          dir_ls)
import::from(correlation, correlation, cor_test)
import::from(conectigo,   mostrar_en_excel)
pacman::p_load(janitor,   fst, paletteer, scales, tidyverse)

# Opciones
options(
 pillar.sigfig    = 5,
 tibble.print_min = 15,
 scipen = 999,
 digits = 7,
 tidymodels.dark = TRUE,
 readr.show_col_types = FALSE,
 dplyr.summarise.inform = FALSE)

## .........................................................................
## Funciones                                                            ####

centinelas <- c("TBD", "TO_BE_DETERMINED")

# Revisar presencia de valores centinelas
revisar_centinelas <- function(df, sentinel) {
 df |>
  select(where(is.character)) |>
  summarise(
   across(
    everything(), \(x) sum(x %in% sentinel, na.rm = TRUE))
  ) |>
  pivot_longer(
   cols = everything(),
   names_to = "feature",
   values_to = "tbds"
  ) |>
  arrange(desc(tbds))
}

# barras de conteo para variables binarias o politómicas
barra <- function(df, x) {
	dfx <- df |>
		tabyl({{x}}) |>
		adorn_pct_formatting() |>
		mutate({{x}} := fct_reorder({{x}}, n, .desc = F))

	dfx |>
		ggplot(aes(y = {{x}}, x = n)) +
		geom_col(aes(fill = {{x}}), width = 0.8) +
		geom_text(aes(label = str_c(label_comma()(n), " ", "(", percent, ")")),
												hjust = 1.1,
												size = 8,
												color = "white",
												fontface = "bold") +
		scale_x_continuous(
			name = NULL,
			expand = c(0, 0),
			labels = label_number(scale_cut = cut_short_scale())) +
		scale_y_discrete(name = NULL, expand = c(0, 0.5)) +
		scale_fill_paletteer_d(`"ggsci::nrc_npg"`) +
		theme_minimal_vgrid(font_family = "yano") +
		theme(plot.margin   = unit(c(10, 2, 2, 2), "mm"),
								axis.text = element_text(size = 30),
								plot.title = element_text(size = 50, face = "bold"),
								legend.position = "bottom",
								legend.text = element_text(size = 30),
								legend.justification = "center",
								legend.title = element_blank())
}

estandarizar_columnas <- function(columna) {
 columna |>
  toupper() |>
  str_replace_all("[\\s-]", "_")
}

## .........................................................................
## Diagnóstico - Tabla maestra                                          ####


# Lectura de archivos
c(diag_00, metr_00, metr_a) %<-%
 (dir_ls(type = "file", glob = "*.csv") |>
 map(read_csv))


# Columnas a eliminar manualmente (validado previamente)
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
 "bts_sh_nm_1"       # Mejor usar bts_sh_nam
 )

# Filtrado y limpieza inicial
## 1. Quitar columnas que tengan una constante
## 2. Quitar columnas que tengan más del 50% de valores perdidos
## 3. Eliminar columnas innecesarias validadas por CTL Team
diag_01 <- diag_00 |>
 select(
  where(\(x) length(unique(x)) != 1 && mean(is.na(x)) < 0.5),
  -all_of(cols_to_remove)
 ) |>
 mutate(
 	across(fct_srvy_dt, \(fecha) ymd(fecha)),
 	across(where(is.character), \(col) estandarizar_columnas(col)),
 	across(msisdn_dd:srvy_id, as.integer)
 ) |>
 relocate(fct_srvy_dt, msisdn_dd, srvy_id)


# Faltantes implícitos
## 1. Identificar la cantidad de valores centinelas hay en las columnas
##
diag_01 |> revisar_centinelas(sentinel = centinelas)

# Revisar duplicados
diag_01 |>
 get_dupes(msisdn_dd) |>
 count(msisdn_dd, sort = TRUE)


# Codificar explícitamente los valores centinelas
diag_02 <- diag_01 |>
 arrange(fct_srvy_dt, msisdn_dd, srvy_id) |>
 mutate(
  across(where(is.character), \(x) na_if(x, "TBD")),
  across(where(is.character), \(x) na_if(x, "TO_BE_DETERMINED"))
 )

# Validar que los centinelas se codificaron explícitamente
revisar_centinelas(diag_02, sentinel = centinelas)

# Colapsar variable respuesta
diag_02 |>
 tabyl(diag) |>
 arrange(desc(n)) |>
 adorn_pct_formatting()
 # mostrar_en_excel()

# Análisis para colapsar ciertas etiquetas en categorías más amplias
diag_02 |>
 filter(diag == "NO_DIAG") |>
 barra(x = class_desc) +
	labs(title = "Class Description en etiqueta no-diag")


etiquetas <- c(
	"CAPACIDAD",
	"OPTIMIZACION",
	"COBERTURA",
	"DISPONIBILIDAD",
	# "TRANSMISION",
	"NO_DIAG")


set.seed(2023)
# Colapso de etiquetas
## Aun se debe garantizar que los PROMOTORES tengan métricas saludables
## Quitamos valores NA en todos los promotores
## Seleccionamos las últimas 1821 observaciones
## Eliminar `class_desc` porque genera fuga de datos (data leakage)
diag_03 <- diag_02 |>
 filter(diag %in% etiquetas) |>
 split(~ diag) |>
 map_at("NO_DIAG", \(df) df |>
  filter(class_desc == "PROMOTOR") |>
  drop_na() |>
  slice_sample(n = 1800) |>
  # remover duplicados solo para promotores
  distinct(msisdn_dd, .keep_all = TRUE)
 ) |>
 list_rbind() |>
 mutate(
 across(diag,
  \(x) case_match(x, "NO_DIAG" ~ "PROMOTOR", .default = diag))) |>
 select(-c(class_desc, cell_load_lte)) |>
 # Permitir que se repita el usuario si la encuesta es diferente
 distinct(msisdn_dd, srvy_id, .keep_all = TRUE)


# Comprobar repetidos en PROMOTORES
diag_03 |> filter(diag == "PROMOTOR") |> get_dupes(msisdn_dd)

# Comprobar repetidos en DETRACTORES
diag_03 |> filter(diag != "PROMOTOR") |> get_dupes(msisdn_dd)

# Cada registro es una encuesta diferente aunque sea el mismo usuario
diag_03 |> filter(diag != "PROMOTOR") |> get_dupes(msisdn_dd, srvy_id)


diag_03 |>
 barra(x = diag) +
 labs(title = "Etiquetas")

# Conteo
diag_03 |> count(diag, sort = TRUE)

# Evaluar multicolinealidad (alta correlación entre features numéricos)
diag_03 |>
 select(where(\(x) is.numeric(x)), -c(msisdn_dd:srvy_id)) |>
 drop_na() |>
 correlation() |>
 as_tibble() |>
 filter(abs(r) >= 0.75)

# Valores faltantes
inspect_na(diag_03)


## .........................................................................
## Métricas                                                             ####

# 273,434 x 27
metr_00

# Valores faltantes
inspect_na(metr_00) |> print(n = Inf)

# Filtrado y limpieza inicial
## Agregar formato de fecha y reordenar columnas
metr_01 <- metr_00 |>
 mutate(
  across(fct_srvy_dt, ymd)
 ) |>
 relocate(fct_srvy_dt, msisdn_dd, srvy_id, r1, bts_sh_nm) |>
 arrange(fct_srvy_dt, msisdn_dd) |>
 select(-c(cell_load, service_drop_rate))


metr_a |> inspectdf::inspect_na()

# Unir únicamente los usuarios que tienen métricas asociadas.  Solo se
# toma en cuenta la encuesta que aparece en diag aunque en métricas
# aparezca más de una.
ctl_00 <- metr_01 |>
 inner_join(metr_a |> select(-time_thrgpt),
            join_by(msisdn_dd, srvy_id))

# Valores faltantes
inspect_na(ctl_00) |> print(n = Inf)


# 273,434 × 30
ctl_00

# Unir con tabla diagnóstico
ctl_01 <- diag_03 |>
 inner_join(ctl_00, join_by(msisdn_dd, srvy_id, fct_srvy_dt)) |>
 relocate(diag, .after = last_col()) |>
 select(-r1) |>
 mutate(across(c(msisdn_dd, srvy_id), as.integer))

# 26,559 × 35
ctl_01

# Valores faltantes
inspect_na(ctl_01) |> print(n = Inf)

write_fst(x = ctl_01, path = "ctl.fst", compress = 0)












