rm(list = ls())
# 01. Carga de librerías y bases -------------------------------------------

library(tidyverse)
library(readxl)
library(clock)
library(writexl)
library(lubridate)


base <- readRDS("01.Bases/02.Clean/base_final_a12.rds") %>%
  select(periodo, provincia, rubroa12, marca_medio_pago, cuotas, operaciones, monto)

# Exploración:
base %>% filter(!complete.cases(.))
unique(base$marca_medio_pago)
unique(base$rubroa12)
unique(base$cuotas)
unique(base$periodo)

source("02.Scripts/Auxiliares/01.Carga_ipc.R") # carga el xlsx de ipc y genera variables para deflactar en determinado período base

poblacion <- readRDS("01.Bases/02.clean/poblacion.rds")


# 02. Procesamiento -------------------------------------------------------

## Se colapsan 3 rubros en 1 ----

base <- base %>%
  mutate(rubroa12 = if_else(rubroa12 %in% c("Pequeños electrodomésticos", "Línea Blanca", "Televisores"),
                            "Electrodomésticos",
                            rubroa12))

unique(base$rubroa12)

## Deflactación de monto, base febrero 2022 ----

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
  mutate(var_mensual  = monto_corriente / lag(monto_corriente, 1, order_by = periodo) - 1,
         var_acum_año = monto_corriente / monto_corriente[replace(row_number() - get_month(periodo), row_number() - get_month(periodo) < 1, NA)] - 1)


#write_xlsx(cuadro_1, "export_1.xlsx")

#### Monto constante ----
cuadro_2 <- base %>%
  group_by(periodo) %>% 
  summarise(monto_constante = sum(monto_constante)) %>% 
  ungroup() %>% 
  arrange(periodo) %>% 
  mutate(var_mensual  = monto_constante / lag(monto_constante, 1, order_by = periodo) - 1,
         var_acum_año = monto_constante / monto_constante[replace(row_number() - get_month(periodo), row_number() - get_month(periodo) < 1, NA)] - 1)

#### Operaciones ----
cuadro_3 <- base %>%
  group_by(periodo) %>% 
  summarise(operaciones = sum(operaciones)) %>% 
  ungroup() %>% 
  arrange(periodo) %>% 
  mutate(var_mensual  = operaciones / lag(operaciones, 1, order_by = periodo) - 1,
         var_acum_año = operaciones / operaciones[replace(row_number() - get_month(periodo), row_number() - get_month(periodo) < 1, NA)] - 1)


## Slides 7 y 8 ----

### Principales 7 rubros del último mes por monto corriente ----

principales_7_rubros_monto <- base %>%
  filter(periodo == max(periodo)) %>%
  group_by(periodo, rubroa12) %>% 
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
  filter(periodo %in% c(max(periodo), add_months(max(base$periodo), -1),
                        add_months(max(base$periodo), - 12),
                        add_months(max(base$periodo), - get_month(max(base$periodo))),
                        add_months(max(base$periodo), - get_month(max(base$periodo)) - 12 )
                        )) %>%
  arrange(periodo) %>%
  group_by(rubro_auxiliar) %>% 
  mutate(var_mensual_monto_const  = monto_constante / lag(monto_constante, 1, order_by = periodo) - 1,
         var_inter_monto_const    = monto_constante / lag(monto_constante, 3, order_by = periodo) - 1,
         var_acum_año_monto_const = monto_constante / lag(monto_constante, 2, order_by = periodo) - 1,
         var_mensual_monto_corr   = monto / lag(monto, 1, order_by = periodo) - 1,
         var_inter_monto_corr     = monto / lag(monto, 3, order_by = periodo) - 1,
         var_acum_año_monto_corr  = monto / lag(monto, 2, order_by = periodo) - 1) %>% 
  ungroup() 

cuadro_4 <- cuadro_4 %>%
  arrange(periodo) %>%
  group_by(rubro_auxiliar) %>% 
  mutate(part_monto_rubro_en_mes_año_ant = lag(part_monto_rubro_en_mes, 3, order_by = periodo)) %>% 
  ungroup()

cuadro_4 <- cuadro_4 %>%
  mutate(auxi_ordena = if_else(rubro_auxiliar == "Otros", TRUE, FALSE)) %>% 
  arrange(periodo, auxi_ordena, desc(monto)) %>% 
  select(-auxi_ordena) |> 
  mutate_at(6:12, .funs = ~(if_else(periodo != max(periodo), NA_real_, .x) ))


# Principales 7 rubros del 2021 

new_base <- as.data.frame(base) %>%
  mutate(year = lubridate::year(base$periodo))


prin_7_rubros_monto_21 <- new_base %>%
  filter(year == 2021) %>%
  group_by(year, rubroa12) %>%
  summarise(monto = sum(monto)) %>%
  ungroup() %>%
  arrange(desc(monto)) %>%
  head(7)

# base filtro 7 principales rubros 2021

aux_base_2 <- base %>%
  mutate(agrupar        = if_else(rubroa12 %in% (prin_7_rubros_monto_21$rubroa12), FALSE, TRUE),
         rubro_auxiliar = if_else(agrupar, "Otros", rubroa12))


aux_base_2 <- aux_base_2 %>% 
  mutate(year = lubridate::year(aux_base_2$periodo))

test <- aux_base_2 %>%
  filter(year == 2021) %>%
  group_by(year, rubro_auxiliar) %>%
  summarise(monto_constante = sum(monto_constante),
            monto           = sum(monto)) %>%
  ungroup() %>% 
  group_by(year) %>% 
  mutate(part_monto_real_rubro_2021 = monto_constante / sum(monto_constante),
         part_monto_corr_rubro_2021 = monto / sum(monto)) %>% 
  select(-monto_constante, -monto) %>% 
  ungroup() %>% 
  select(-year) 



# Principales 7 rubros del 2022 

prin_7_rubros_monto_22 <- new_base %>%
  filter(year == 2022) %>%
  group_by(year, rubroa12) %>%
  summarise(monto = sum(monto)) %>%
  ungroup() %>%
  arrange(desc(monto)) %>%
  head(7)

# base filtro 7 principales rubros 2022

aux_base_3 <- base %>%
  mutate(agrupar        = if_else(rubroa12 %in% (prin_7_rubros_monto_22$rubroa12), FALSE, TRUE),
         rubro_auxiliar = if_else(agrupar, "Otros", rubroa12))


aux_base_3 <- aux_base_3 %>% 
  mutate(year = lubridate::year(aux_base_3$periodo))

test_2022 <- aux_base_3 %>%
  filter(year == 2022) %>%
  group_by(year, rubro_auxiliar) %>%
  summarise(monto_constante = sum(monto_constante),
            monto           = sum(monto)) %>%
  ungroup() %>% 
  group_by(year) %>% 
  mutate(part_monto_real_rubro_2022 = monto_constante / sum(monto_constante),
         part_monto_corr_rubro_2022 = monto / sum(monto)) %>% 
  select(-monto_constante, -monto) %>% 
  ungroup() %>% 
  select(-year) 

#aca hago el join para juntar los df con los calculos acumulados


cuadro_4 <- cuadro_4 %>%
  left_join(., test, by = "rubro_auxiliar") 

cuadro_4 <- cuadro_4 %>% 
  left_join(.,test_2022, by = "rubro_auxiliar") %>% 
  mutate_at(6:16, .funs = ~(if_else(periodo != max(periodo), NA_real_, .x) ))
 


# exportar alguna base completa


### Principales 7 rubros del último mes por operaciones ----

principales_7_rubros_operaciones <- base %>%
  filter(periodo == max(periodo)) %>%
  group_by(periodo, rubroa12) %>% 
  summarise(operaciones = sum(operaciones)) %>% 
  ungroup() %>% 
  arrange(desc(operaciones)) %>% 
  head(7)


aux_base_4 <- base %>% 
  mutate(agrupar        = if_else(rubroa12 %in% (principales_7_rubros_operaciones$rubroa12), FALSE, TRUE),
         rubro_auxiliar = if_else(agrupar, "Otros", rubroa12))


cuadro_5 <- aux_base_4 %>%
  group_by(periodo, rubro_auxiliar) %>% 
  summarise(operaciones = sum(operaciones)) %>% 
  ungroup() %>% 
  group_by(periodo) %>% 
  mutate(part_operaciones_rubro_en_mes = operaciones / sum(operaciones)) %>% 
  ungroup() %>%
  filter(periodo %in% c(max(periodo),
                        add_months(max(base$periodo), -1),
                        add_months(max(base$periodo), -12))) %>%
  arrange(periodo) %>%
  group_by(rubro_auxiliar) %>% 
  mutate(var_mensual_operaciones = operaciones / lag(operaciones, 1, order_by = periodo) - 1,
         var_inter_operaciones   = operaciones / lag(operaciones, 2, order_by = periodo) - 1) %>% 
  ungroup()

cuadro_5 <- cuadro_5 %>% arrange(periodo) %>% group_by(rubro_auxiliar) %>% 
  mutate(part_operaciones_rubro_en_mes_año_ant = lag(part_operaciones_rubro_en_mes, 2, order_by = periodo)) %>% 
  ungroup()

cuadro_5 <- cuadro_5 %>%
  mutate(auxi_ordena = if_else(rubro_auxiliar == "Otros", TRUE, FALSE)) %>% 
  arrange(periodo, auxi_ordena, desc(operaciones)) %>% 
  select(-auxi_ordena)



### Principales 7 rubros de 2021 por operaciones ----

prin_7_rubros_opera_21 <- new_base %>%
  filter(year == 2021) %>%
  group_by(year, rubroa12) %>%
  summarise(operaciones = sum(operaciones)) %>%
  ungroup() %>%
  arrange(desc(operaciones)) %>%
  head(7)


# Base filtro 7 principales rubros 2021

aux_base_5 <- base %>%
  mutate(agrupar        = if_else(rubroa12 %in% (prin_7_rubros_opera_21$rubroa12), FALSE, TRUE),
         rubro_auxiliar = if_else(agrupar, "Otros", rubroa12))

aux_base_5 <- aux_base_5 %>% 
  mutate(year = lubridate::year(aux_base_5$periodo))

df.acumulado_1 <- aux_base_5 %>%
  filter(year == 2021) %>%
  group_by(year, rubro_auxiliar) %>%
  summarise(operaciones = sum(operaciones)) %>%
  ungroup() %>% 
  group_by(year) %>% 
  mutate(part_opera_rubro_2021 = operaciones / sum(operaciones)) %>% 
  select(-operaciones) %>% 
  ungroup() %>% 
  select(-year) %>% 
  mutate(auxi_ordena = if_else(rubro_auxiliar == "Otros", TRUE, FALSE)) %>% 
  arrange(auxi_ordena, desc(part_opera_rubro_2021)) %>% 
  select(-auxi_ordena)%>% 
  rename(rubros_2021 = rubro_auxiliar)


### Principales 7 rubros de 2022 por operaciones ----

prin_7_rubros_opera_22 <- new_base %>%
  filter(year == 2022) %>%
  group_by(year, rubroa12) %>%
  summarise(operaciones = sum(operaciones)) %>%
  ungroup() %>%
  arrange(desc(operaciones)) %>%
  head(7)


# Base filtro 7 principales rubros 2022

aux_base_6 <- base %>%
  mutate(agrupar        = if_else(rubroa12 %in% (prin_7_rubros_opera_21$rubroa12), FALSE, TRUE),
         rubro_auxiliar = if_else(agrupar, "Otros", rubroa12))

aux_base_6 <- aux_base_6 %>% 
  mutate(year = lubridate::year(aux_base_6$periodo))

df.acumulado_2 <- aux_base_6 %>%
  filter(year == 2022) %>%
  group_by(year, rubro_auxiliar) %>%
  summarise(operaciones = sum(operaciones)) %>%
  ungroup() %>% 
  group_by(year) %>% 
  mutate(part_opera_rubro_2022 = operaciones / sum(operaciones)) %>% 
  select(-operaciones) %>% 
  ungroup() %>% 
  select(-year) %>% 
  mutate(auxi_ordena = if_else(rubro_auxiliar == "Otros", TRUE, FALSE)) %>% 
  arrange(auxi_ordena, desc(part_opera_rubro_2022)) %>% 
  select(-auxi_ordena)%>% 
  rename(rubros_2022 = rubro_auxiliar)

 
#aca hago el bind para juntar los columnas de los df con los calculos acumulados


 cuadro_5.1 <- bind_cols(df.acumulado_1,df.acumulado_2)
 

# exportar alguna base completa

## Slide 9_1 ----

### Participaciones por cuotas. Total base ----

cuadro_6 <- base %>%
  group_by(periodo, cuotas) %>% 
  summarise(monto = sum(monto)) %>% 
  ungroup() %>% 
  group_by(periodo) %>% 
  mutate(participacion_monto_cuota_en_mes = monto / sum(monto)) %>% 
  ungroup()

cuadro_6_resumen <- cuadro_6 %>%
  filter(periodo %in% c(max(periodo), add_months(max(cuadro_6$periodo), -12), add_months(max(cuadro_6$periodo), -1))) %>% 
  arrange(periodo)


#write_xlsx(cuadro_6, "export_3.xlsx")


##### slide 9_2

### Participaciones por cuotas. Para cada uno de los 7 principales rubros ----

aux_base_7 <- base %>%
  filter(rubroa12 %in% principales_7_rubros_monto$rubroa12)

cuadro_7 <- aux_base_7 %>%
  group_by(periodo, rubroa12, cuotas) %>% 
  summarise(monto = sum(monto)) %>% 
  ungroup() %>% 
  group_by(periodo, rubroa12) %>% 
  mutate(participacion_monto_cuota_en_rubro_mes = monto / sum(monto)) %>% 
  ungroup()

cuadro_7_resumen <- cuadro_7 %>% filter(periodo == max(periodo))


##### slide 9_3

# creo los df acumulados de las cuotas para 2021 y 2022

df_acum_2021 <- new_base %>% 
  filter(year == 2021) %>% 
  group_by(year, cuotas) %>% 
  summarise(monto_constante = sum(monto_constante)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(part_monto_real_cuotas = monto_constante / sum(monto_constante)) %>% 
  ungroup()

df_acum_2022 <- new_base %>% 
  filter(year == 2022) %>% 
  group_by(year, cuotas) %>% 
  summarise(monto_constante = sum(monto_constante)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(part_monto_real_cuotas = monto_constante / sum(monto_constante)) %>% 
  ungroup() 

cuadro_6.1 <- bind_rows(df_acum_2021,df_acum_2022)


## Slide 10 ----

aux_base_8 <- base %>%
  filter(rubroa12 %in% principales_7_rubros_monto$rubroa12)

cuadro_8_1 <- aux_base_8 %>%
  group_by(periodo, rubroa12, cuotas) %>% 
  summarise(monto       = sum(monto),
            operaciones = sum(operaciones)) %>% 
  ungroup() %>%
  mutate(ticket_promedio = monto / operaciones) %>%
  filter(periodo %in% c(max(periodo), add_months(max(aux_base_8$periodo), -1), add_months(max(aux_base_8$periodo), -12)))


cuadro_8_2 <- cuadro_8_1 %>%
  group_by(periodo, rubroa12) %>% 
  summarise(monto       = sum(monto),
            operaciones = sum(operaciones)) %>% 
  ungroup() %>%
  mutate(ticket_promedio = monto / operaciones) %>%
  filter(periodo %in% c(max(periodo), add_months(max(aux_base_8$periodo), -1), add_months(max(aux_base_8$periodo), -12))) %>% 
  mutate(cuotas = "Total")

cuadro_8 <- bind_rows(cuadro_8_1 %>% mutate(cuotas = as.character(cuotas)), cuadro_8_2)

cuadro_8 <- cuadro_8 %>%
  arrange(periodo) %>% 
  group_by(rubroa12, cuotas) %>% 
  mutate(var_mensual = ticket_promedio / lag(ticket_promedio, 1, order_by = periodo) - 1,
         var_inter   = ticket_promedio / lag(ticket_promedio, 2, order_by = periodo) - 1) %>% 
  ungroup()

cuadro_8_aux <- cuadro_8 |> 
  filter(cuotas %in% c(3, 18)) |> 
  group_by(periodo, rubroa12) |> 
  mutate(cociente_ticket_prom_18_3 = ticket_promedio / lag(ticket_promedio, n = 1L, order_by = as.integer(cuotas))) |> 
  ungroup() |> 
  filter(!is.na(cociente_ticket_prom_18_3))

cuadro_8 <- cuadro_8 |> 
  left_join(cuadro_8_aux)

cuadro_8_1 <- base %>% 
  group_by(periodo) %>% 
  summarise(ticket_promedio = sum(monto) / sum(operaciones)) %>% 
  ungroup()


## Slide 11 ----

#-- slide 11_1

cuadro_9_1 <- base %>% 
  group_by(periodo, provincia) %>% 
  summarise(monto = sum(monto)) %>% 
  ungroup() %>% 
  group_by(periodo) %>% 
  mutate(participacion = monto / sum(monto)) %>% 
  ungroup() %>%
  filter(periodo %in% c(max(periodo),
                        add_months(max(base$periodo), -1),
                        add_months(max(base$periodo), -12),
                        add_months(max(base$periodo), - get_month(max(base$periodo))),
                        add_months(max(base$periodo), - get_month(max(base$periodo)) - 12 ))) %>% 
  arrange(periodo) %>% 
  group_by(provincia) %>% 
  mutate(var_mensual   = monto / lag(monto, 1, order_by = periodo) - 1,
         var_inter     = monto / lag(monto, 3, order_by = periodo) - 1,
         var_acum_año  = monto / lag(monto, 2, order_by = periodo) - 1) %>% 
  ungroup() |> 
  mutate_at(5:7, .funs = ~(if_else(periodo != max(periodo), NA_real_, .x) ))


###---- slide 11_2

cuadro_9_2 <- base %>%
  left_join(poblacion %>% select(provincia, region)) %>% 
  group_by(periodo, region) %>% 
  summarise(monto = sum(monto)) %>% 
  ungroup() %>% 
  group_by(periodo) %>% 
  mutate(participacion = monto / sum(monto)) %>% 
  ungroup() %>%
  filter(periodo %in% c(max(periodo),
                        add_months(max(base$periodo), -1),
                        add_months(max(base$periodo), -12),
                        add_months(max(base$periodo), - get_month(max(base$periodo))),
                        add_months(max(base$periodo), - get_month(max(base$periodo)) - 12 ))) %>% 
  arrange(periodo) %>% 
  group_by(region) %>% 
  mutate(var_mensual   = monto / lag(monto, 1, order_by = periodo) - 1,
         var_inter     = monto / lag(monto, 3, order_by = periodo) - 1,
         var_acum_año  = monto / lag(monto, 2, order_by = periodo) - 1) %>% 
  ungroup() |> 
  mutate_at(5:7, .funs = ~(if_else(periodo != max(periodo), NA_real_, .x) ))


####----slide 11_3

## Participacion de las ventas por provincia en terminos reales ####


cuadro_9_3 <- new_base %>%
  filter(year == 2022) %>%
  group_by(year, provincia) %>%
  summarise(monto_constante = sum(monto_constante)) %>%
  ungroup() %>% 
  group_by(year) %>% 
  mutate(part_monto_real_prov_2022 = monto_constante / sum(monto_constante)) %>% 
  ungroup()


## Slide 12 ----

pob_tot <- poblacion %>%
  summarise(poblacion_pais = sum(poblacion)) %>% pull(1)

cuadro_10_1 <- base %>%
  group_by(periodo, provincia) %>% 
  summarise(monto = sum(monto)) %>% 
  ungroup() %>% 
  left_join(poblacion, by = "provincia") %>% 
  mutate(monto_per_capita = monto / poblacion) %>% 
  filter(periodo >= "2020-01-01") |> 
  group_by(provincia) |> 
  mutate(var_mensual_monto_pc = monto_per_capita / lag(monto_per_capita, n = 1L, order_by = periodo) - 1) |> 
  ungroup()

cuadro_10_2 <- cuadro_10_1 %>% group_by(periodo, region) %>% 
  summarise(poblacion = sum(poblacion),
            monto     = sum(monto)) %>% 
  ungroup() %>% 
  mutate(monto_per_capita = monto / poblacion) %>% 
  filter(periodo >= "2020-01-01")

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


cuadros_export <- list("Cuadro 1"     = cuadro_1,
                       "Cuadro 2"     = cuadro_2,
                       "Cuadro 3"     = cuadro_3,
                       "Cuadro 4"     = cuadro_4,
                       "Cuadro 5"     = cuadro_5,
                       "Cuadro 5.1"   = cuadro_5.1,
                       "Cuadro 6"     = cuadro_6_resumen,
                       "Cuadro 6.1"   = cuadro_6.1,
                       "Cuadro 7"     = cuadro_7_resumen,
                       "Cuadro 8"     = cuadro_8,
                       "Cuadro 8.1"   = cuadro_8_1,
                       "Cuadro 9.1"   = cuadro_9_1,
                       "Cuadro 9.2"   = cuadro_9_2,
                       "Cuadro 9.3"   = cuadro_9_3,
                       "Cuadro 10.1"  = cuadro_10_1,
                       "Cuadro 10.2"  = cuadro_10_2,
                       "Cuadro 10.3"  = cuadro_10_3)


saveRDS(cuadros_export, "03.Output/02.Export/cuadros_export.rds")
