rm(list = ls())

# 01. Seleccionar el mes desde el que se quiere acumular para los gráficos  -----------------------



mes <- "2021-01-01"



# 02. Carga de librerías y bases -------------------------------------------

library(tidyverse)
library(readxl)
library(clock)
library(writexl)
library(googledrive)
library(googlesheets4)

base <- readRDS("01.Bases/02.Clean/base_final_a12.rds")

cuits <- read_csv("01.Bases/01.Raw/A12_CUIT_provincia_rubro_20210805.csv") |>
  rename("año" = anio) |>
  mutate(periodo = date_build(año, mes)) |> 
  select(-año, -mes)

comercios <- read_xlsx("01.Bases/01.Raw/A12_comercios.xlsx")

# 03. Procesamiento -------------------------------------------------------

## 03.01 Participación monto ----

cuadro_1 <- base  |> 
  group_by(periodo, provincia) |> 
  summarise(monto = sum(monto)) |> 
  ungroup() |> 
  group_by(periodo) |> 
  mutate(part_en_pais = monto / sum(monto)) |> 
  ungroup()

cuadro_1 <- cuadro_1 |> 
  filter(periodo >= "2020-01-01")

## 03.02 Participación operaciones ----

cuadro_2 <- base |>
  group_by(periodo, provincia) |> 
  summarise(operaciones = sum(operaciones)) |> 
  ungroup() |> 
  group_by(periodo) |> 
  mutate(part_en_pais = operaciones / sum(operaciones)) |> 
  ungroup()

cuadro_2 <- cuadro_2 |> 
  filter(periodo >= "2020-01-01")

## 03.03 Ventas por rubro y cuotas ----

principales_7_rubros_monto <- base |> 
  filter(periodo >= !!mes) |> # cambiar esto en caso de que se quiera el acumulado de otro año
  group_by(provincia, rubroa12) |> 
  summarise(monto = sum(monto)) |> 
  ungroup() |> 
  group_by(provincia) |> 
  arrange(desc(monto)) |>
  filter(row_number() <= 7) |> 
  ungroup() |> 
  arrange(provincia) |>
  select(-monto) |> 
  mutate(principal = TRUE)

cuadro_3_pre <- base |>
  filter(periodo >= !!mes) |> # cambiar esto en caso de que se quiera el acumulado de otro año
  left_join(principales_7_rubros_monto, by = c("provincia", "rubroa12")) |> 
  mutate(rubro_auxi = if_else(is.na(principal), "Otros", rubroa12))
  
cuadro_3_1 <- cuadro_3_pre |> 
  group_by(provincia, rubro_auxi) |> 
  summarise(monto = sum(monto)) |> 
  ungroup() |>
  group_by(provincia) |> 
  mutate(part = monto / sum(monto)) |> 
  ungroup()

cuadro_3_2 <- cuadro_3_pre |> 
  filter(principal) |> 
  group_by(provincia, rubro_auxi, cuotas) |>
  summarise(monto = sum(monto)) |>
  ungroup() |> 
  group_by(provincia, rubro_auxi) |> 
  mutate(part = monto /  sum(monto)) |> 
  ungroup()

## 03.04 CUIT por provincia ----

cuadro_4 <- cuits |> # cantidad de cuits por provincia
  distinct(periodo, provincia, cuit) |> 
  count(periodo, provincia, name = "cantidad de CUITs") |> 
  filter(periodo >= "2020-01-01") |> 
  arrange(periodo, desc(`cantidad de CUITs`), provincia)

# se puede llegar a contar los principales cuits por operaciones y por monto por periodo y provincia


## 03.05 Comercios por provincia ----

cuadro_5_1 <- comercios |> 
  count(cuit, name = "cantidad de CUITs", sort = TRUE) |> 
  mutate(participacion = `cantidad de CUITs` / sum(`cantidad de CUITs`))

cuadro_5_2 <- comercios |>
  group_by(cuit) |>
  filter( n() > 1 ) # la base tiene muchos duplicados en cuit por direcciones distintas
cuadro_5_2 <- cuadro_5_2 |> head(1000)

cuadro_5_3 <- comercios |> 
  distinct(cuit, provincia, localidad) |> 
  count(provincia, localidad, name = "cantidad de CUITs", sort = TRUE) |> 
  arrange(provincia, localidad)

cuadro_5_4 <- comercios |> 
  distinct(cuit, provincia, localidad, direccion) |> 
  arrange(provincia, localidad, cuit)
cuadro_5_4 <- cuadro_5_4 |> head(1000)


# 04. Exportación -------------------------------------------------------------

## 04.01 Autorización googlesheets ----

googledrive::drive_auth("mdointerno@gmail.com")
gs4_auth(token = drive_token())

## 04.02 Leo googlesheets ----

resultados_a12_ggsheet <- as_sheets_id("https://docs.google.com/spreadsheets/d/1jh10T0S8-kwXnYVsXEDILyHTafOCZiA_i822LQVCUVk/edit#gid=0") |>
  as.character() # Leo la googlesheet

gs4_browse(resultados_a12_ggsheet) # la abro en el explorador

## 04.03 Exporto ----

range_write(resultados_a12_ggsheet, # escribo sheet slide_3
            sheet = "slide_3",
            data = cuadro_1,
            reformat = FALSE,
            range = "B1")

range_write(resultados_a12_ggsheet, # escribo sheet slide_4
            sheet = "slide_4",
            data = cuadro_2,
            reformat = FALSE,
            range = "B1")

range_write(resultados_a12_ggsheet, # escribo sheet slide_5_1
            sheet = "slide_5_1",
            data = cuadro_3_1,
            reformat = FALSE)

range_write(resultados_a12_ggsheet, # escribo sheet slide_5_2
            sheet = "slide_5_2",
            data = cuadro_3_2,
            reformat = FALSE)

range_write(resultados_a12_ggsheet, # escribo sheet cant_cuits
            sheet = "cant_cuits",
            data = cuadro_4,
            reformat = FALSE)

range_write(resultados_a12_ggsheet, # escribo sheet duplicados_base
            sheet = "duplicados_base",
            data = cuadro_5_2,
            reformat = FALSE)

range_write(resultados_a12_ggsheet, # escribo sheet cant_cuits_local
            sheet = "cant_cuits_local",
            data = cuadro_5_3,
            reformat = FALSE)

range_write(resultados_a12_ggsheet, # escribo sheet direcciones
            sheet = "direcciones",
            data = cuadro_5_4,
            reformat = FALSE)
