rm(list = ls())


# 01. Seleccionar el último mes con datos completos -----------------------



mes <- "2021-09-01"



# 02. Carga de librerías y bases -------------------------------------------

library(tidyverse)
library(readxl)
library(clock)
library(writexl)


base <- read_csv("01.Bases/01.Raw/A12_mensual_20211007.csv") %>%
  rename("año" = anio) %>%
  mutate(periodo = date_build(año, mes)) %>% 
  select(-año, -mes) %>%
  filter(periodo <= as.Date(!!mes), # filtro periodos menores o igual al señalado arriba
         complete.cases(.),
         cuotas %in% c(3, 6, 12, 18, 24, 30),
         monto > 0)

ponderaciones <- readRDS("01.Bases/02.Clean/pond_plataformas.rds") %>% select(-operaciones, -monto)

# 03. Procesamiento First Data E-Commerce y Otros -------------------------------------------------------

unique(base$marca_medio_pago) # medios de pagos en la base
unique(base$rubroa12) # rubros en la base
if(nrow(distinct(ponderaciones, plataforma)) != 1) print("HAY MAS DE UNA PLATAFORMA EN LA BASE")

# hacer un par de medidas descriptivas de cuanto es First Data E-Commerce y Otro en el total de la base por meses

first_data_y_otros <- base %>% filter(rubroa12 %in% c("First Data E-Commerce", "Otros")) # me quedo solo con estos 2 rubros para repartir

first_data_y_otros %>% distinct(provincia, rubroa12, marca_medio_pago) %>% arrange(rubroa12) # provincias, rubros, y marcas

# calculo los gastos y operaciones por provincia, por periodo y por cuota

first_data_y_otros <- first_data_y_otros %>% 
  group_by(periodo, provincia, cuotas) %>% 
  summarise(monto       = sum(monto),
            operaciones = sum(operaciones)) %>% 
  ungroup() %>% 
  arrange(periodo, provincia, cuotas) 


# comienzo a repartir

first_data_y_otros <- first_data_y_otros %>% left_join(ponderaciones, by = c("periodo", "cuotas")) # adiciono ponderaciones

first_data_y_otros <- first_data_y_otros %>%
  rename(operaciones_a_repartir = operaciones,
         monto_a_repartir       = monto)

chequeo1 <- first_data_y_otros %>% group_by(periodo, provincia, cuotas) %>% summarise(sum(part_monto_en_cuota), sum(part_operaciones_en_cuota))

gastos_repartidos <- first_data_y_otros %>%
  mutate(monto       = monto_a_repartir * part_monto_en_cuota,
         operaciones = round(operaciones_a_repartir * part_operaciones_en_cuota)) %>% 
  mutate(operaciones = if_else(operaciones == 0, 1, operaciones)) %>% 
  group_by(periodo, cuotas, provincia) %>% 
  mutate(chequeo_monto          = sum(monto),
         chequeo_operaciones    = sum(operaciones)) %>% 
  ungroup()


write_xlsx(gastos_repartidos, "03.Output/01.Chequeos/gastos_repartidos.xlsx") # exporto un xlsx con lo repartido


# 04. Bind de dfs y exportación --------------------------------------------

df_1 <- base %>% filter(!rubroa12 %in% c("First Data E-Commerce", "Otros")) # la base entera original sin los rubros de "e-commerce" y "otros"

gastos_repartidos <- gastos_repartidos %>% mutate(marca_medio_pago = "concepto_repartido") # agrego variable que esta presente en la base original

df_2 <- gastos_repartidos %>% select(colnames(df_1))

export <- bind_rows(df_1, df_2)

saveRDS(export, "01.Bases/02.Clean/base_final_a12.rds")
write_xlsx(export, "03.Output/01.Chequeos/base_final_a12.xlsx")

# 05. Chequeo -------------------------------------------------------------


chequeo2 <- export %>% group_by(periodo, provincia, cuotas) %>% 
  summarise(suma_monto       = sum(monto),
            suma_operaciones = sum(operaciones)) %>% 
  ungroup() %>% 
  full_join(

base %>% group_by(periodo, provincia, cuotas) %>% 
  summarise(suma_monto       = sum(monto),
            suma_operaciones = sum(operaciones)) %>% 
  ungroup(),
by = c("periodo", "provincia", "cuotas"), suffix = c("_clean", "_raw")
  ) %>% 
  mutate(chequeo_monto = suma_monto_clean == suma_monto_raw,
         chequeo_operaciones = suma_operaciones_clean == suma_operaciones_raw,
         diferencia_operacioens = suma_operaciones_clean - suma_operaciones_raw)



