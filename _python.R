

pacman::p_load(fs, reticulate)

ruta_python <- path("C:",
                    "Users",
                    "wchavarria",
                    "anaconda3",
                    "envs",
                    "data_products",
                    "python",
                    ext = "exe")

Sys.setenv(RETICULATE_PYTHON = ruta_python)
use_python(ruta_python, required = T)

