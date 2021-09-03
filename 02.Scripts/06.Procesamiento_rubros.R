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

ponderaciones_mdolibre <- readRDS("01.Bases/02.Clean/pond_plataformas.rds")


# 03. Corrección de base cuits --------------------------------------------

# El rubro e-commerce debe ser repartido, hay solo 2 cuits que tienen ese concepto. mdo libre y otro q no está la razón social en la base de nombres
# Lo que voy a hacer es repartir dentro del cuit mercado libre ese gasto de ecommerce. el otro cuit q tiene un gasto marginal en ecommerce lo dejo
# También voy a juntar"Pequeños electrodomésticos", "Línea Blanca", "Televisores" en "Electrodomésticos"

commerce_cuit <- cuits |> # me quedo solo con los datos de e commerce que tienen cuit en la base de cuits con nombres (el q tiene nombre es el de cap fed, el otro q es poco no tiene nombre es de prov bs as )
  filter(rubroa12 == "First Data E-Commerce") |> 
  semi_join(
    comercios |> distinct(cuit, razon_social),
    by = "cuit"
  ) |> 
  distinct(cuit)


ponderaciones_mdolibre <- ponderaciones_mdolibre |> # ponderaciones de gasto de mdo libre
  group_by(periodo, rubroa12) |> 
  summarise(operaciones = sum(operaciones),
            monto       = sum(monto)) |> 
  ungroup() |> 
  group_by(periodo) |> 
  mutate(part_op    = operaciones / sum(operaciones),
         part_monto = monto / sum(monto)) |> 
  ungroup() |> 
  select(-operaciones, -monto)


gastos_ecommerce <- cuits |> # reparto
  filter(cuit     == commerce_cuit$cuit,
         rubroa12 == "First Data E-Commerce") |> 
  group_by(periodo, provincia, rubroa12, cuit) |> 
  summarise(monto       = sum(monto),
            operaciones = sum(operaciones)) |> 
  ungroup()


ecommerce_repartido <- gastos_ecommerce |> 
  left_join(ponderaciones_mdolibre, by = "periodo") |> 
  mutate(monto_nuevo       = part_op * monto,
         operaciones_nuevo = part_op * operaciones) |> 
  select(provincia, rubroa12 = rubroa12.y, cuit, monto = monto_nuevo, operaciones = operaciones_nuevo, periodo)


cuits <- cuits |>
  filter(cuit != commerce_cuit$cuit | rubroa12 != "First Data E-Commerce")
  bind_rows(ecommerce_repartido)




# 04. Junto 3 rubros en "Electrodomésticos" --------------------------------------------------

cuits <- cuits |>
  mutate(rubroa12 = if_else(rubroa12 %in% c("Pequeños electrodomésticos", "Línea Blanca", "Televisores"),
                            "Electrodomésticos",
                            rubroa12))

base <- base |>
  mutate(rubroa12 = if_else(rubroa12 %in% c("Pequeños electrodomésticos", "Línea Blanca", "Televisores"),
                            "Electrodomésticos",
                            rubroa12))


# 05. Procesamiento -------------------------------------------------------

## 05.01 Participación monto ----

cuadro_1 <- base  |> 
  group_by(periodo, rubroa12) |> 
  summarise(monto = sum(monto)) |> 
  ungroup() |> 
  group_by(periodo) |> 
  mutate(part_en_pais = monto / sum(monto)) |> 
  ungroup()

cuadro_1 <- cuadro_1 |> 
  filter(periodo >= "2020-01-01")

## 05.02 Participación operaciones ----

cuadro_2 <- base |>
  group_by(periodo, rubroa12) |> 
  summarise(operaciones = sum(operaciones)) |> 
  ungroup() |> 
  group_by(periodo) |> 
  mutate(part_en_pais = operaciones / sum(operaciones)) |> 
  ungroup()

cuadro_2 <- cuadro_2 |> 
  filter(periodo >= "2020-01-01")

## 05.03 Participación provincias por rubro ----

cuadro_3 <- base |>
  filter(periodo >= !!mes) |> # filtro a partir de este mes
  group_by(provincia, rubroa12) |> 
  summarise(monto = sum(monto)) |> 
  ungroup() |> 
  group_by(rubroa12) |> 
  mutate(participacion = monto / sum(monto)) |> 
  ungroup() |> 
  arrange(desc(participacion))

## 05.04 Participación principales cuits por rubro ----

cuadro_4 <- cuits |>
  filter(periodo >= !!mes) |> # se filtra a partir del mes seleccionado arriba
  group_by(rubroa12, cuit) |> 
  summarise(monto       = sum(monto),
            operaciones = sum(operaciones)) |> 
  ungroup()

cuadro_4_auxi <- cuadro_4 |> 
  group_by(rubroa12) |> 
  top_n(10, wt = monto) |> 
  ungroup()|> 
  arrange(rubroa12, desc(monto)) |> 
  mutate(filtro = TRUE)

cuadro_4 <- cuadro_4 |> 
  left_join(cuadro_4_auxi |> select(rubroa12, cuit, filtro), by = c("rubroa12", "cuit")) |> 
  mutate(cuit_aux = if_else(!is.na(filtro), as.character(cuit), "Resto"))

cuadro_4 <- cuadro_4 |>
  group_by(rubroa12, cuit_aux) |> 
  summarise(monto       = sum(monto),
            operaciones = sum(operaciones)) |> 
  ungroup() |> 
  group_by(rubroa12) |> 
  mutate(part_monto       = monto / sum(monto),
         part_operaciones = operaciones / sum(operaciones)) |> 
  ungroup() |> 
  arrange(rubroa12, desc(monto))


cuadro_4 <- cuadro_4 |> left_join( 
comercios |> 
  distinct(cuit, razon_social) |> 
  group_by(cuit) |> 
  slice(1) |> 
  ungroup() |> 
  mutate(cuit_aux = as.character(cuit)),
by = "cuit_aux"
) |> 
  arrange(rubroa12, desc(monto)) |> 
  rename(!! paste("monto acumulado desde", mes)        := monto,
         !! paste("operaciones acumuladas desde", mes) := operaciones)

## 05.05 Cantidad de cuits en el 2021 por rubro

cuadro_5 <- cuits |> 
  filter(periodo >= !!mes) |> 
  distinct(rubroa12, cuit) |> 
  count(rubroa12) |> 
  arrange(desc(n)) |> 
  rename(!! paste("cantidad de cuits desde", mes) := n)
  


# 06. Exportación -------------------------------------------------------------

## 06.01 Autorización googlesheets ----

googledrive::drive_auth("lucas.e.peralta.mail@gmail.com")
gs4_auth(token = drive_token())

## 06.02 Leo googlesheets ----

resultados_a12_ggsheet <- as_sheets_id("https://docs.google.com/spreadsheets/d/19KJIOsI4CAGeb8wkLC2jYW-3xBMhHghJmbbv-eZ9oYw/edit#gid=0") |>
  as.character() # Leo la googlesheet

#gs4_browse(resultados_a12_ggsheet) # la abro en el explorador

## 06.03 Exporto ----

range_write(resultados_a12_ggsheet, # escribo sheet slide_2_1
            sheet = "slide_2_1",
            data = cuadro_1,
            reformat = FALSE,
            range = "B1")

range_write(resultados_a12_ggsheet, # escribo sheet slide_2_2
            sheet = "slide_2_2",
            data = cuadro_2,
            reformat = FALSE,
            range = "B1")

range_write(resultados_a12_ggsheet, # escribo sheet slide_3
            sheet = "slide_3",
            data = cuadro_3,
            reformat = FALSE,
            range = "A1")

range_write(resultados_a12_ggsheet, # escribo sheet slide_4_1
            sheet = "slide_4_1",
            data = cuadro_4,
            reformat = FALSE,
            range = "B1")

range_write(resultados_a12_ggsheet, # escribo sheet slide_4_2
            sheet = "slide_4_2",
            data = cuadro_5,
            reformat = FALSE,
            range = "B1")
