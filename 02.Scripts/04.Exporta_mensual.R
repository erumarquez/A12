rm(list = ls())

# 01. Carga de librerías y bases ------------------------------------------


library(tidyverse)
library(googledrive)
library(googlesheets4)

cuadros <- readRDS("03.Output/02.Export/cuadros_export.rds")

cuadros <- cuadros # %>% # paso fecha a caracter porque sino me pone números raros en el googlesheet
#  map(~mutate(., periodo = as.character(periodo)))

# 02.Subo resultados a googlesheet ---------------------------------------------

## Autorización googlesheets ----
googledrive::drive_auth("mdointerno@gmail.com")
gs4_auth(token = drive_token())


## Leo googlesheets ----

resultados_a12_ggsheet <- as_sheets_id("https://docs.google.com/spreadsheets/d/18d9AL7J93jWdChlS2TwdGiUgBtfl-GsZxwbQfsgQcoo/edit") %>%
  as.character() # Leo la googlesheet resultados_a12

gs4_browse(resultados_a12_ggsheet) # la abro en el explorador

range_write(resultados_a12_ggsheet, # escribo sheet slide_4
            sheet = "slide_4",
            data = cuadros[["Cuadro 1"]],
            reformat = FALSE,
            range = "A1")

range_write(resultados_a12_ggsheet, # escribo sheet slide_5
            sheet = "slide_5",
            data = cuadros[["Cuadro 2"]],
            reformat = FALSE)

range_write(resultados_a12_ggsheet, # escribo sheet slide_6
            sheet = "slide_6",
            data = cuadros[["Cuadro 3"]],
            reformat = FALSE)

range_write(resultados_a12_ggsheet, # escribo sheet slide_7
            sheet = "slide_7",
            data = cuadros[["Cuadro 4"]],
            reformat = FALSE)

range_write(resultados_a12_ggsheet, # escribo sheet slide_8
            sheet = "slide_8",
            data = cuadros[["Cuadro 5"]],
            reformat = FALSE)

range_write(resultados_a12_ggsheet, # escribo sheet slide_9_1
            sheet = "slide_9_1",
            data = cuadros[["Cuadro 6"]],
            reformat = FALSE)

range_write(resultados_a12_ggsheet, # escribo sheet slide_9_2
            sheet = "slide_9_2",
            data = cuadros[["Cuadro 7"]],
            reformat = FALSE)

range_write(resultados_a12_ggsheet, # escribo sheet slide_10
            sheet = "slide_10",
            data = cuadros[["Cuadro 8"]],
            reformat = FALSE)

range_write(resultados_a12_ggsheet, # escribo sheet slide_10_aux
            sheet = "slide_10_aux",
            data = cuadros[["Cuadro 8.1"]],
            reformat = FALSE)

range_write(resultados_a12_ggsheet, # escribo sheet slide_11_1
            sheet = "slide_11_1",
            data = cuadros[["Cuadro 9.1"]],
            reformat = FALSE)

range_write(resultados_a12_ggsheet, # escribo sheet slide_11_2
            sheet = "slide_11_2",
            data = cuadros[["Cuadro 9.2"]],
            reformat = FALSE)

range_write(resultados_a12_ggsheet, # escribo sheet slide_12_1
            sheet = "slide_12_1",
            data = cuadros[["Cuadro 10.1"]],
            reformat = FALSE)

range_write(resultados_a12_ggsheet, # escribo sheet slide_12_2
            sheet = "slide_12_2",
            data = cuadros[["Cuadro 10.2"]],
            reformat = FALSE)

range_write(resultados_a12_ggsheet, # escribo sheet slide_12_3
            sheet = "slide_12_3",
            data = cuadros[["Cuadro 10.3"]],
            reformat = FALSE)
