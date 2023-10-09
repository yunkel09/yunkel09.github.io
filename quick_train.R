

source("_common.R")
source("_functions.R")
source("_load.R")

theme_set(theme_light())

tablero_ctl <- board_folder(path = "tablero_ctl")

ctl_split <- pin_read(board = tablero_ctl, name = "ctl_split")
ctl_folds <- pin_read(board = tablero_ctl, name = "ctl_folds")
ctl_train <- training(ctl_split)

ctl_train |>
 count(diag, sort = TRUE)

ctl_pivoted <- ctl_train |>
 pivot_longer(cols = thp_required_lte:rain,
              names_to = "metric", values_to = "value")

ctl_pivoted |>
 ggplot(aes(value, fill = diag)) +
 geom_density(alpha = .5) +
 facet_wrap(~ metric, scales = "free")


ctl_pivoted |>
 ggplot(aes(diag, value)) +
 geom_boxplot() +
 facet_wrap(~ metric, scales = "free")

# no hay suficiente variación entre las categorías.


sl <- sleepstudy %>%
 relocate(Subject, Days, Reaction) |>
 clean_names() |>
 as_tibble()

new_subject <- tibble(
        days = 0:9,
        subject = "one"
)

nuevo_usuario <- tibble(
        rbs = 0:5,
        usuario = 40009655
)


library(gee)
library(multilevelmod)

gee_spec <- linear_reg() %>%
 set_engine("gee", corstr = "exchangeable")

gee_fit <- gee_spec %>%
        fit(reaction ~ days + id_var(subject), data = sl)


gee_fit

# ver el efecto del prb en cada una de las top 5 celdas
usuarios <- ctl_train |>
 select(msisdn_dd, bts_sh_nm, rate_prb_dl) |>
 arrange(msisdn_dd) |>
 summarise(n = n_distinct(bts_sh_nm), .by = msisdn_dd) |>
 filter(n == 5) |>
 slice_head(n = 36) |>
 pull(msisdn_dd)


ctl_multilevel <- ctl_train |>
 select(msisdn_dd, bts_sh_nm, cll_prctg, rate_prb_dl,
        ra_ta_ue_index6, ra_ta_ue_index7, ra_ta_ue_total, diag) |>
 filter(msisdn_dd %in% usuarios) |>
 rename(id = msisdn_dd, prb = rate_prb_dl) |>
 rowwise() |>
 mutate(suma_index = ra_ta_ue_index6 + ra_ta_ue_index7) |>
 ungroup()
 # group_by(id) |>
 # mutate(rbs = row_number()) |>
 # relocate(rbs, .after = id) |>
 # select(-bts_sh_nm) |>
 # ungroup()

# Sumar los porcentajes de tiempo (cll_prctg) que la métrica estuvo fuera
# de rango

# con timing-advance, solo usar 6 y 7 y lo que esté arriba del 20%
ctl_multilevel |>
 filter(id == 30139770) |>
 mutate(ponderado = cll_prctg * suma_index)

ctl_multilevel |>
 ggplot(aes(x = rbs, y = prb)) +
 geom_point(aes(color = diag)) +
 geom_line(aes(color = diag)) +
 geom_hline(yintercept = 0.85,
            linetype = "dashed",
            color = "red") +
 facet_wrap(~ id) +
 drako
# no necesariamente los prb mayor a 0.85 son de capacidad



ctl_fit <- gee_spec %>%
 fit(prb ~ rbs + id_var(id), data = ctl_multilevel)


predict(gee_fit, new_subject |> select(days)) |>
 bind_cols(new_subject)

predict(ctl_fit, nuevo_usuario |> select(rbs)) |>
        bind_cols(nuevo_usuario)



library(colino)


data(cells, package = "modeldata")



rec <- recipe(class ~ ., data = cells[, -1]) %>%
 step_select_infgain(all_predictors(),
                outcome = "class",
                threshold = 0.9,
                id = "infgain")



rec |> ver()









