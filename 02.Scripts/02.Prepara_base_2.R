rm(list = ls())
# 01. Carga de librerías y bases -------------------------------------------

library(tidyverse)
library(readxl)
library(clock)
library(writexl)


base_raw <- read_csv("01.Bases/01.Raw/A12_mensual_20210701/A12_mensual_20210701.csv") %>%
  rename("año" = anio) %>%
  mutate(periodo = date_build(año, mes)) %>% 
  select(-año, -mes)

base <- base_raw  %>% rename(rubro_a_repartir = rubroa12)

ponderaciones <- readRDS("01.Bases/02.Clean/pond_plataformas.rds") %>% select(-operaciones, -monto, -ultimo_periodo_valido)

# 02. Procesamiento -------------------------------------------------------

unique(base$marca_medio_pago) # medios de pagos en la base
unique(base$rubro_a_repartir) # rubros en la base
if(nrow(distinct(ponderaciones, plataforma)) != 1) print("HAY MAS DE UNA PLATAFORMA EN LA BASE")

# hacer un par de medidas descriptivas de cuanto es First Data E-Commerce y Otro en el total de la base por meses

## First Data E-Commerce y Otros ----

first_data_y_otros <- base %>% filter(rubro_a_repartir %in% c("First Data E-Commerce", "Otros")) # me quedo solo con estos 2 rubros para repartir

first_data_y_otros %>% distinct(provincia, rubro_a_repartir, marca_medio_pago) %>% arrange(rubro_a_repartir) # provincias, rubros, y marcas


