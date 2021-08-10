rm(list = ls())
# 01. Carga de liberías y bases -------------------------------------------

library(tidyverse)
library(readxl)
library(clock)


base <- read_csv("01.Bases/01.Raw/A12_mensual_20210701/A12_mensual_20210701.csv") %>% rename("año" = anio) %>%
  mutate(periodo = date_build(año, mes)) %>% rename(rubro_a_repartir = rubroa12)

ponderaciones <- readRDS("01.Bases/02.Clean/pond_plataformas.rds") %>% select(-operaciones, -monto, -ultimo_periodo_valido)

# 02. Procesamiento -------------------------------------------------------


unique(base$marca_medio_pago) # medios de pagos en la base
unique(base$rubro_a_repartir) # rubros en la base
if(nrow(distinct(ponderaciones, plataforma))!=1) print("HAY MAS DE UNA PLATAFORMA EN LA BASE")

# hacer un par de medidas descriptivas de cuanto es First Data E-Commerce y Otro en el total de la base por meses

## First Data E-Commerce y Otros ----

first_data_y_otros <- base %>% filter(rubro_a_repartir %in% c("First Data E-Commerce", "Otros")) # me quedo solo con estos 2 rubros para repartir

first_data_y_otros %>% distinct(provincia, rubro_a_repartir, marca_medio_pago) %>% arrange(rubro_a_repartir) # provincias, rubros, y marcas

# Tomar estos gastos y repartirlos en las categorías de la base ponderaciones.
# 

totales <- first_data_y_otros %>% # gasto total por provincia y periodo
   group_by(periodo, provincia) %>% 
   summarise(monto       = sum(monto),
             operaciones = sum(operaciones)) %>% 
   ungroup() %>% 
   arrange(periodo, provincia)

asd <- totales %>% filter(provincia == "BUENOS AIRES", periodo == "2021-06-01") %>% left_join(ponderaciones %>% filter(cuotas == "Total"), by = "periodo")


asd %>% summarise(sum = sum(participacion_monto_por_cuota),
                  asd = sum(participacion_operaciones_por_cuota))

ww <- asd %>% mutate(monto_nuevo = monto * participacion_monto_por_cuota,
                     operaciones_nuevo = operaciones * participacion_operaciones_por_cuota)


ww %>% summarise(sum(monto_nuevo),
                 sum(operaciones_nuevo))

ww %>% 


  first_data_y_otros

