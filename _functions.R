

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
  geom_col(aes(fill = {{x}}), width = 0.9) +
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
  theme(
  plot.margin   = unit(c(10, 2, 2, 2), "mm"),
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