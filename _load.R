

diag_00 <- read_fst("data/diag_00.fst") |> as_tibble() |> suppressMessages()
metr_00 <- read_fst("data/metr_00.fst") |> as_tibble()
disp_00 <- read_fst("data/disp_00.fst") |> as_tibble()
lluv_00 <- read_fst("data/lluv_00.fst") |> as_tibble()

cargar_fuentes()

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
