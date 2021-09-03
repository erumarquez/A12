rm(list = ls())
# 01. Carga de librerías y bases -------------------------------------------

library(tidyverse)
library(readxl)
library(clock)
library(writexl)


base <- readRDS("01.Bases/02.Clean/base_final_a12.rds") %>%
  select(periodo, provincia, rubroa12, marca_medio_pago, cuotas, operaciones, monto)


base <- base %>%
  mutate(rubroa12 = if_else(rubroa12 %in% c("Pequeños electrodomésticos", "Línea Blanca", "Televisores"),
                            "Electrodomésticos",
                            rubroa12))

export <- base |> 
  filter(periodo >= "2021-01-01") |> 
  group_by(rubroa12) |> 
  summarise(operaciones = sum(operaciones),
            monto       = sum(monto)) |> 
  ungroup() |> 
  mutate(part_operaciones = operaciones / sum(operaciones),
         part_monto       = monto / sum(monto))

write_xlsx(export, "part_desde_ene21.xlsx")
