rm(list = ls())
# 01. Carga de librerías y bases -------------------------------------------

library(tidyverse)
library(readxl)
library(clock)
library(writexl)


base <- readRDS("01.Bases/02.Clean/base_final_a12.rds") %>%
  select(periodo, provincia, rubroa12, marca_medio_pago, cuotas, operaciones, monto)

# Exploración:
base %>% filter(!complete.cases(.))
unique(base$marca_medio_pago)
unique(base$rubroa12)
unique(base$cuotas)
unique(base$periodo)

ipc <- read_excel("01.Bases/01.Raw/ipc.xlsx") %>% mutate(periodo = as.Date(periodo))

poblacion <- readRDS("01.Bases/02.clean/poblacion.rds")


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
cuadro_1 <- base %>%
  group_by(periodo) %>% # monto corriente
  summarise(monto_corriente = sum(monto)) %>% 
  ungroup() %>% 
  arrange(periodo) %>% 
  mutate(var_mensual = (monto_corriente / lag(monto_corriente, 1, order_by = periodo) - 1 ))

#### Monto constante ----
cuadro_2 <- base %>%
  group_by(periodo) %>% 
  summarise(monto_constante = sum(monto_constante)) %>% 
  ungroup() %>% 
  arrange(periodo) %>% 
  mutate(var_mensual = (monto_constante / lag(monto_constante, 1, order_by = periodo) - 1 ))

#### Operaciones ----
cuadro_3 <- base %>%
  group_by(periodo) %>% 
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
  mutate(agrupar        = if_else(rubroa12 %in% (principales_7_rubros_monto$rubroa12), FALSE, TRUE),
         rubro_auxiliar = if_else(agrupar, "Otros", rubroa12))


cuadro_4 <- aux_base_1 %>%
  group_by(periodo, rubro_auxiliar) %>% 
  summarise(monto_constante = sum(monto_constante),
            monto           = sum(monto)) %>% 
  ungroup() %>% 
  group_by(periodo) %>% 
  mutate(part_monto_rubro_en_mes = monto / sum(monto)) %>% 
  ungroup() %>%
  filter(periodo %in% c(max(periodo), add_months(max(base$periodo), -1), add_months(max(base$periodo), -12))) %>%
  arrange(periodo) %>%
  group_by(rubro_auxiliar) %>% 
  mutate(var_mensual_monto_const = monto_constante / lag(monto_constante, 1, order_by = periodo) - 1,
         var_inter_monto_const   = monto_constante / lag(monto_constante, 2, order_by = periodo) - 1) %>% 
  ungroup()

cuadro_4 <- cuadro_4 %>% arrange(periodo) %>% group_by(rubro_auxiliar) %>% 
  mutate(part_monto_rubro_en_mes_año_ant = lag(part_monto_rubro_en_mes, 2, order_by = periodo)) %>% 
  ungroup()

# exportar alguna base completa


### Principales 7 rubros del último mes por operaciones ----

principales_7_rubros_operaciones <- base %>%
  filter(periodo == max(periodo)) %>%
  group_by(periodo, rubroa12) %>% 
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
  filter(periodo %in% c(max(periodo), add_months(max(base$periodo), -1), add_months(max(base$periodo), -12))) %>%
  arrange(periodo) %>%
  group_by(rubro_auxiliar) %>% 
  mutate(var_mensual_operaciones = operaciones / lag(operaciones, 1, order_by = periodo) - 1,
         var_inter_operaciones   = operaciones / lag(operaciones, 2, order_by = periodo) - 1) %>% 
  ungroup()

cuadro_5 <- cuadro_5 %>% arrange(periodo) %>% group_by(rubro_auxiliar) %>% 
  mutate(part_operaciones_rubro_en_mes_año_ant = lag(part_operaciones_rubro_en_mes, 2, order_by = periodo)) %>% 
  ungroup()


# exportar alguna base completa

## Slide 9 ----

### Participaciones por cuotas. Total base ----

cuadro_6 <- base %>%
  group_by(periodo, cuotas) %>% 
  summarise(monto = sum(monto)) %>% 
  ungroup() %>% 
  group_by(periodo) %>% 
  mutate(participacion_monto_cuota_en_mes = monto / sum(monto)) %>% 
  ungroup()

cuadro_6_resumen <- cuadro_6 %>% filter(periodo %in% c(max(periodo), add_months(max(cuadro_6$periodo), -12), add_months(max(cuadro_6$periodo), -1))) %>% 
  arrange(periodo)


#write_xlsx(cuadro_6, "export_3.xlsx")

### Participaciones por cuotas. Para cada uno de los 7 principales rubros ----

aux_base_3 <- base %>%
  filter(rubroa12 %in% principales_7_rubros_monto$rubroa12)

cuadro_7 <- aux_base_3 %>%
  group_by(periodo, rubroa12, cuotas) %>% 
  summarise(monto = sum(monto)) %>% 
  ungroup() %>% 
  group_by(periodo, rubroa12) %>% 
  mutate(participacion_monto_cuota_en_rubro_mes = monto / sum(monto)) %>% 
  ungroup()

cuadro_7_resumen <- cuadro_7 %>% filter(periodo == max(periodo))


## Slide 10 ----

aux_base_4 <- base %>%
  filter(rubroa12 %in% principales_7_rubros_monto$rubroa12)

cuadro_8_1 <- aux_base_4 %>%
  group_by(periodo, rubroa12, cuotas) %>% 
  summarise(monto       = sum(monto),
            operaciones = sum(operaciones)) %>% 
  ungroup() %>%
  mutate(ticket_promedio = monto / operaciones) %>%
  filter(periodo %in% c(max(periodo), add_months(max(aux_base_4$periodo), -1), add_months(max(aux_base_4$periodo), -12)))


cuadro_8_2 <- cuadro_8_1 %>%
  group_by(periodo, rubroa12) %>% 
  summarise(monto       = sum(monto),
            operaciones = sum(operaciones)) %>% 
  ungroup() %>%
  mutate(ticket_promedio = monto / operaciones) %>%
  filter(periodo %in% c(max(periodo), add_months(max(aux_base_4$periodo), -1), add_months(max(aux_base_4$periodo), -12))) %>% 
  mutate(cuotas = "Total")

cuadro_8 <- bind_rows(cuadro_8_1 %>% mutate(cuotas = as.character(cuotas)), cuadro_8_2)

cuadro_8 <- cuadro_8 %>%
  arrange(periodo) %>% 
  group_by(rubroa12, cuotas) %>% 
  mutate(var_mensual = ticket_promedio / lag(ticket_promedio, 1, order_by = periodo) - 1,
         var_inter   = ticket_promedio / lag(ticket_promedio, 2, order_by = periodo) - 1) %>% 
  ungroup()

## Slide 11 ----

cuadro_9_1 <- base %>% 
  group_by(periodo, provincia) %>% 
  summarise(monto = sum(monto)) %>% 
  ungroup() %>% 
  group_by(periodo) %>% 
  mutate(participacion = monto / sum(monto)) %>% 
  ungroup() %>%
  filter(periodo %in% c(max(periodo), add_months(max(base$periodo), -1), add_months(max(base$periodo), -12))) %>% 
  arrange(periodo) %>% 
  group_by(provincia) %>% 
  mutate(var_mensual = monto / lag(monto, 1, order_by = periodo) - 1,
         var_inter   = monto / lag(monto, 2, order_by = periodo) - 1) %>% 
  ungroup()


cuadro_9_2 <- base %>%
  left_join(poblacion %>% select(provincia, region)) %>% 
  group_by(periodo, region) %>% 
  summarise(monto = sum(monto)) %>% 
  ungroup() %>% 
  group_by(periodo) %>% 
  mutate(participacion = monto / sum(monto)) %>% 
  ungroup() %>%
  filter(periodo %in% c(max(periodo), add_months(max(base$periodo), -1), add_months(max(base$periodo), -12))) %>% 
  arrange(periodo) %>% 
  group_by(region) %>% 
  mutate(var_mensual = monto / lag(monto, 1, order_by = periodo) - 1,
         var_inter   = monto / lag(monto, 2, order_by = periodo) - 1) %>% 
  ungroup()

  
  
## Slide 12 ----

pob_tot <- poblacion %>%
  summarise(poblacion_pais = sum(poblacion)) %>% pull(1)

cuadro_10_1 <- base %>%
  group_by(periodo, provincia) %>% 
  summarise(monto = sum(monto)) %>% 
  ungroup() %>% 
  left_join(poblacion, by = "provincia") %>% 
  mutate(monto_per_capita = monto / poblacion)

cuadro_10_2 <- cuadro_10_1 %>% group_by(periodo, region) %>% 
  summarise(poblacion = sum(poblacion),
            monto     = sum(monto)) %>% 
  ungroup() %>% 
  mutate(monto_per_capita = monto / poblacion)

cuadro_10_3 <- base %>%
  group_by(periodo, provincia, rubroa12) %>% 
  summarise(monto = sum(monto)) %>% 
  ungroup() %>%
  filter(periodo == max(periodo)) %>% 
  group_by(periodo, provincia) %>% 
  mutate(participacion = monto / sum(monto)) %>% 
  ungroup() %>% 
  group_by(provincia) %>% 
  filter(monto == max(monto)) %>% 
  ungroup()


# completar slide 12

# 03. Exportación ---------------------------------------------------------


cuadros_export <- list("Cuadro 1"   = cuadro_1,
                       "Cuadro 2"   = cuadro_2,
                       "Cuadro 3"   = cuadro_3,
                       "Cuadro 4"   = cuadro_4,
                       "Cuadro 5"   = cuadro_5,
                       "Cuadro 6"   = cuadro_6_resumen,
                       "Cuadro 7"   = cuadro_7_resumen,
                       "Cuadro 8"   = cuadro_8,
                       "Cuadro 9.1" = cuadro_9_1,
                       "Cuadro 9.2" = cuadro_9_2,
                       "Cuadro 10.1"  = cuadro_10_1,
                       "Cuadro 10.2"  = cuadro_10_2,
                       "Cuadro 10.3"  = cuadro_10_3)


saveRDS(cuadros_export, "03.Output/02.Export/cuadros_export.rds")
