rm(list = ls())


# 02. Carga de librerías y bases -------------------------------------------

library(tidyverse)
library(readxl)
library(clock)
library(writexl)
library(googledrive)
library(googlesheets4)



cuits <- read_csv("01.Bases/01.Raw/A12_CUIT_provincia_rubro_20210805.csv") |>
  rename("año" = anio) |>
  mutate(periodo = date_build(año, mes)) |> 
  select(-año, -mes)


base <- cuits |>
  filter(periodo >= "2020-01-01")

# cantidad de cuits desde enero 2020

cuits_desde_ene20 <- base |> 
  distinct(cuit)

cant_cuits_desde_ene20 <- cuits_desde_ene20 |> 
  nrow() |> 
  as_tibble()

# cuits presentes en la base antes de agostso y despues de agosto

cuits_antes_ago21 <-  cuits |> 
  filter(periodo < "2021-08-01") |> 
  distinct(cuit) |> 
  mutate(antes_ago21 = "si")

cuits_ago21 <-  base |> 
  filter(periodo == "2021-08-01") |> 
  distinct(cuit) |>
  mutate(ago21 = "si")

compa <- full_join(cuits_antes_ago21, cuits_ago21) 

# ausentes antes de agosto 21
ausentes_antes_ago21 <- compa |> 
  filter(is.na(antes_ago21))

cant_ausentes_antes_ago21 <-  ausentes_antes_ago21 |> 
  nrow() |> 
  as_tibble()

asd <- cuits |> 
  inner_join(ausentes_antes_ago21)



export <- list("cuits presentes desde ene20"             = cuits_desde_ene20,
               "cantidad de cuits desde ene20" = cant_cuits_desde_ene20,
               "cuits que solo están en ago21"           = ausentes_antes_ago21,
               "cantidad cuits que solo en ago21"  = cant_ausentes_antes_ago21,
               "listado nuevos cuits"                    = asd)

write_xlsx(export, "pedido_cuitsa12.xlsx")



cuits |> 
  distinct(cuit) |> 
  nrow()
