rm(list = ls())
# 01. Carga de librerías y bases -------------------------------------------

library(tidyverse)
library(readxl)
library(clock)


base <- readRDS("01.Bases/02.Clean/base_final_a12.rds") %>%
  select(periodo, provincia, rubroa12, marca_medio_pago, cuotas, operaciones, monto)

# Exploración:
base %>% filter(!complete.cases(.))
unique(base$marca_medio_pago)
unique(base$rubroa12)
unique(base$cuotas)
unique(base$periodo)

ipc <- read_excel("01.Bases/01.Raw/ipc.xlsx") %>% mutate(periodo = as.Date(periodo))


# 02. Procesamiento -------------------------------------------------------

## Se colapsan 3 rubros en 1 ----

base <- base %>%
  mutate(rubroa12 = if_else(rubroa12 %in% c("Pequeños electrodomésticos", "Línea Blanca", "Televisores"),
                            "Electrodomésticos",
                            rubroa12))

unique(base$rubroa12)


## Deflactación de monto, base enero 2019 ----

mes_base <- ipc %>% filter(periodo == "2019-01-01") %>% pull(2)

ipc <- ipc %>%
  mutate(ipc_base_ene19    = ipc_indec_ng / mes_base * 100, # creo índice con base enero 2019 = 100
         coef_deflactor    = ipc_indec_ng / mes_base,
         variacion_mensual = (ipc_base_ene19 / lag(ipc_base_ene19, 1, order_by = periodo) - 1))

base <- base %>%
  left_join(ipc %>% select(-ipc_indec_ng, -variacion_mensual), by = "periodo") %>% 
  mutate(monto_constante = monto / coef_deflactor)

## Slides 4, 5 y 6 ----

#### Monto corriente ----
cuadro_1 <- base %>% group_by(periodo) %>% # monto corriente
  summarise(monto_corriente = sum(monto)) %>% 
  ungroup() %>% 
  arrange(periodo) %>% 
  mutate(var_mensual = (monto_corriente / lag(monto_corriente, 1, order_by = periodo) - 1 ))

#### Monto constante ----
cuadro_2 <- base %>% group_by(periodo) %>% 
  summarise(monto_constante = sum(monto_constante)) %>% 
  ungroup() %>% 
  arrange(periodo) %>% 
  mutate(var_mensual = (monto_constante / lag(monto_constante, 1, order_by = periodo) - 1 ))

#### Operaciones ----
cuadro_3 <- base %>% group_by(periodo) %>% 
  summarise(operaciones = sum(operaciones)) %>% 
  ungroup() %>% 
  arrange(periodo) %>% 
  mutate(var_mensual = (operaciones / lag(operaciones, 1, order_by = periodo) - 1 ))

## Slides 7 y 8 ----

### Principales 7 rubros del último mes por monto corriente ----
principales_7_rubros_monto <- base %>% filter(periodo == max(periodo)) %>% group_by(periodo, rubroa12) %>% 
  summarise(monto = sum(monto)) %>% 
  ungroup() %>% 
  arrange(desc(monto)) %>% 
  head(7)


aux_base_1 <- base %>% 
  mutate(agrupar = if_else(rubroa12 %in% (principales_7_rubros_monto$rubroa12), FALSE, TRUE),
         rubro_auxiliar = if_else(agrupar, "Otros", rubroa12))

cuadro_4 <- aux_base_1 %>%
  group_by(periodo, rubro_auxiliar) %>% 
  summarise(monto_constante = sum(monto_constante),
            monto           = sum(monto)) %>% 
  ungroup() %>% 
  group_by(periodo) %>% 
  mutate(part_monto_rubro_en_mes = monto / sum(monto)) %>% 
  ungroup() %>%
  arrange(periodo) %>%
  group_by(rubro_auxiliar) %>% 
  mutate(var_mensual_monto_const = monto_constante / lag(monto_constante, 1, order_by = periodo) - 1,
         var_inter_monto_const   = monto_constante / lag(monto_constante, 12, order_by = periodo) - 1) %>% 
  ungroup()

cuadro_4 <- cuadro_4 %>% arrange(periodo) %>% group_by(rubro_auxiliar) %>% 
  mutate(part_monto_rubro_en_mes_año_ant = lag(part_monto_rubro_en_mes, 12, order_by = periodo)) %>% 
  ungroup()

# exportar cuadro_4 también para que lo vean analistas

write_xlsx(cuadro_4, "export.xlsx")

### Principales 7 rubros del último mes por operaciones ----

principales_7_rubros_operaciones <- base %>% filter(periodo == max(periodo)) %>% group_by(periodo, rubroa12) %>% 
  summarise(operaciones = sum(operaciones)) %>% 
  ungroup() %>% 
  arrange(desc(operaciones)) %>% 
  head(7)


aux_base_2 <- base %>% 
  mutate(agrupar = if_else(rubroa12 %in% (principales_7_rubros_operaciones$rubroa12), FALSE, TRUE),
         rubro_auxiliar = if_else(agrupar, "Otros", rubroa12))

cuadro_5 <- aux_base_2 %>%
  group_by(periodo, rubro_auxiliar) %>% 
  summarise(operaciones = sum(operaciones)) %>% 
  ungroup() %>% 
  group_by(periodo) %>% 
  mutate(part_operaciones_rubro_en_mes = operaciones / sum(operaciones)) %>% 
  ungroup() %>%
  arrange(periodo) %>%
  group_by(rubro_auxiliar) %>% 
  mutate(var_mensual_operaciones = operaciones / lag(operaciones, 1, order_by = periodo) - 1,
         var_inter_operaciones   = operaciones / lag(operaciones, 12, order_by = periodo) - 1)


cuadro_5_resumen <- cuadro_5 %>% filter(periodo %in% c(max(periodo), add_months(max(cuadro_5$periodo), -12)))


write_xlsx(cuadro_5, "export_2.xlsx")

## Slide 9 ----

### Participaciones por cuotas. Total base ----

cuadro_6 <- base %>% group_by(periodo, cuotas) %>% 
  summarise(monto = sum(monto)) %>% 
  ungroup() %>% 
  group_by(periodo) %>% 
  mutate(participacion_monto_cuota_en_mes = monto / sum(monto)) %>% 
  ungroup()

cuadro_6_resumen <- cuadro_6 %>% filter(periodo %in% c(max(periodo), add_months(max(cuadro_6$periodo), -12), add_months(max(cuadro_6$periodo), -1))) %>% 
  arrange(periodo) %>% 
  group_by(cuotas) 


write_xlsx(cuadro_6, "export_3.xlsx")

### Participaciones por cuotas. Para cada uno de los 7 principales rubros ----

aux_base_3 <- base %>% filter(rubroa12 %in% principales_7_rubros_monto$rubroa12)

cuadro_7 <- aux_base_3 %>% group_by(periodo, rubroa12, cuotas) %>% 
  summarise(monto = sum(monto)) %>% 
  ungroup() %>% 
  group_by(periodo, rubroa12) %>% 
  mutate(participacion_monto_cuota_en_rubro_mes = monto / sum(monto)) %>% 
  ungroup()

cuadro_7_resumen <- cuadro_7 %>% filter(periodo == max(periodo))


## Slide 10 ----

cuadro_7_resumen

# falta la parte de ticket promedio
