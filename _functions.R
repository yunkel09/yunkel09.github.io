

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

# generar gráfico de densidad
estimar_densidad <- function(df, d, color, titulo) {

  brk <- hist(df[[d]], plot = FALSE)$breaks
  e <- summary(df[[d]], digits = 4)[c(1,3,4,6)] %>%
    set_names(make_clean_names(names(.)))
  com <- e  |> map_chr(comma, accuracy = 0.1)
  a <- glue('Media: {com[3]} | Mediana: {com[2]} |')
  b <- glue('min: {com[1]}')
  ley <- str_c(a, b)

  df %>%
    ggplot(aes(x = .data[[d]], y = after_stat(density))) +
    geom_histogram(fill   = color, colour = "black", linewidth = .2,
                   breaks = brk) +
    scale_x_continuous(name = d, breaks = brk) +
    geom_density(linewidth = 1) +
    geom_vline(xintercept = e[["median"]],linetype = "dashed",
               color = "red",  alpha = 0.5) +
    geom_vline(xintercept = e[["mean"]],  linetype = "dashed",
               color = "blue", alpha = 0.5) +
    labs(title = titulo, subtitle = ley) +
    yunkel + theme(plot.title    = element_text(size = 46),
                   plot.subtitle = element_text(size = 30),
                   axis.text     = element_text(size = 16, angle = 90))
}


# wrapper para anova de una vía
bx <- function(.df, .x, .y) {
  ggbetweenstats(
    x = {{ .x }},
    y = {{ .y }},
    data = .df,
    bf.message = FALSE,
    p.adjust.method = "bonferroni",
    var.equal = FALSE,
    outlier.tagging = TRUE,

    pairwise.display = "s",
    title = "ANOVA 1 vía",

    ggsignif.args = list(textsize = 5,
                         tip_length = 0.01)) +
    theme(
      plot.title = element_text(size = 46),
      plot.subtitle = element_text(size = 24),
      axis.text = element_text(size = 20))
}


# configuración de tree-maps
config_v <- ggpacket() +
  scale_fill_viridis(
    discrete = F,
    option = "C",
    name = "TIME-CL",
    guide = guide_colorbar(
      direction = "horizontal",
      label.position = "bottom",
      title.position = "top",
      ticks = FALSE,
      barwidth = grid::unit(2.8, "in"),
      barheight = grid::unit(0.2, "in"))
  ) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = grid::unit(1, "pt"),
    legend.position = "top",
    legend.justification = "left",
    legend.title.align = 0.5,
    legend.title = element_text(size = 20))

# graficar heatmap
gr_mo <- function(df, .x, .y, l = F, r = 0.4) {

  if (l == T) {

    df |>
      ggplot(aes(x = .data[[.x]], y = .data[[.y]])) +
      geom_tile(aes(fill = time_cl), color = "white", linewidth = 0.25) +
      config_v() +
      coord_fixed(ratio = r) +
      theme(legend.position = "top")

  } else {

    df |>
      ggplot(aes(x = .data[[.x]], y = .data[[.y]])) +
      geom_tile(aes(fill = time_cl), color = "white", linewidth = 0.25) +
      config_v() +
      coord_fixed(ratio = r) +
      theme(legend.position = "none")

  }
}


graficar_mosaicos <- function(df, f1, f2, leyenda = F, ra = 0.4) {
  aux <- df |>
    group_by(.data[[f1]], .data[[f2]]) |>
    summarise(eventos = n(), time_cl = mean(time_cl), .groups = "drop")

  l1 <- nlevels(aux[, 1, drop = T])
  l2 <- nlevels(aux[, 2, drop = T])

  if (l1 > l2) {
    aux |> gr_mo(.x = f2, .y = f1, l = leyenda, r = ra)
  } else {
    aux |> gr_mo(.x = f1, .y = f2, l = leyenda, r = ra)
  }
}















