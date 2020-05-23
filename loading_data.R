  # 1. Cargamos los paquetes necesarios ####
  library(readODS)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(ggplot2)
  library(gridExtra)
  library(ggpubr)
  library(readr)

  # 2. Cargamos los datos desde el archivo de LibreOffice Calc ####
  path <- "data/xy_coord_div.ods"

  # Creamos una lista con los datos de todos los tipos de dientes
  xy_coord <- path %>%
    ods_sheets() %>%
    #set_names() %>%
    map(read_ods, skip = 1, path = path)

  names(xy_coord) <- c("UI", "UC", "UP", "UM", "LI", "LC", "LP", "LM")

  # Creamos una función para que se reordenen los datos de cada diente dentro de la lista
  # Le llamamos stack_df()

  stack_df <- function(x) {
    gather(x, XY, val) %>%
      mutate(coord = substr(XY, 1, 1),
             tooth_num = parse_number(XY)) %>%
      group_by(XY) %>%
      mutate(sample = 1:n()) %>%
      ungroup() %>%
      select(-XY) %>%
      spread(coord, val) %>%
      filter(!is.na(X) & !is.na(Y))
  }

  # Aplicamos la función stack_df() a cada elemento de la lista.
  xy_coord_stack <- lapply(xy_coord, stack_df)
  save(xy_coord_stack, file = "data/xy_coord_stack.RData")

  # Representamos gráficamente las regresiones
  # Generamos primero una función para representar las regresiones
  polpr <- function(x) {
    ggplot(x, aes(x = X, y = Y)) +
      geom_point(alpha = .10, size = 1, shape = 19) +
      stat_smooth(method = 'lm', formula = y ~ poly(x, 3)) +
      xlim(0, 100) + ylim(0, 100) +
      theme_minimal()
  }

polpr_gg <- lapply(xy_coord_stack, polpr)
do.call(ggarrange, polpr_gg)
