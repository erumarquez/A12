rm(list = ls())
# 01. Carga de liberías y bases -------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)
library(clock)

# base <- read_csv("01.Bases/01.Raw/A12_mensual_20210701/A12_mensual_20210701.csv") %>% rename("año" = anio)
base <- read_csv2("01.Bases/01.Raw/base_corregida_nueva.csv") %>% rename("año" = anio)

ipc <- read_excel("01.Bases/01.Raw/ipc.xlsx") %>% mutate(periodo = as.Date(periodo))

# 02. Inicio procesamiento -------------------------------------------------

datos_faltantes <- base %>% filter(!complete.cases(.)) # df con las filas que tienen missing value en alguna variable, exportarla para revisarla.

base <- base %>% filter(complete.cases(.))  # me quedo con lo que no es missing y creo variable fecha
base <- base %>% mutate(periodo = date_build(año, mes))

base <- base %>% mutate(indice_ipc_ene19 = ipc %>% filter(periodo == "2019-01-01") %>% pull(2)) %>% 
  left_join(ipc, by = "periodo") %>% 
  mutate(monto_constante = monto / ipc_indec_ng * indice_ipc_ene19)

## 02.01 Cálculo de operaciones y monto total mensual, ticket promedio, variaciones ----

cuadro1 <- base %>%
  group_by(periodo, año, mes) %>%
  summarise(suma_monto           = sum(monto),
            suma_operaciones     = sum(operaciones),
            suma_monto_constante = sum(monto_constante)) %>%
  ungroup() %>%
  mutate(ticket_promedio = suma_monto / suma_operaciones)

cuadro1_separados <- split(cuadro1, cuadro1$año) %>% adorn_totals(where = "row") # todos los años separados

### Para hacer la slide "Volumen de venta en pesos corrientes", "Cantidad de operaciones" y "Volumen de venta en pesos constantes"  ----

cuadro1_corrientes <- cuadro1 %>% arrange(periodo) %>%
  mutate(var_mensual_monto              = suma_monto / lag(suma_monto, 1) - 1,
         var_interanual_monto           = suma_monto / lag(suma_monto, 12) - 1,
         var_mensual_operaciones        = suma_operaciones / lag(suma_operaciones, 1) - 1,
         var_interanual_operaciones     = suma_operaciones / lag(suma_operaciones, 12) - 1,
         var_mensual_ticket             = ticket_promedio / lag(ticket_promedio, 1) - 1,
         var_interanual_ticket          = ticket_promedio / lag(ticket_promedio, 12) - 1,
         var_mensual_monto_constante    = suma_monto_constante / lag(suma_monto_constante, 1) - 1,
         var_interanual_monto_constante = suma_monto_constante / lag(suma_monto_constante, 12) - 1)

#exportar lo que se necesite de esto a unas sheets



## 02.03 Por rubro ----

base %>% distinct(rubroa12)

base <- base %>%
  mutate(rubroa12 = if_else(rubroa12 %in% c("Pequeños electrodomésticos", "Línea Blanca", "Televisores"),
                            "Electrodomésticos",
                            rubroa12)) # Se colapsan 3 rubros en 1

cuadro2 <- base %>%
  group_by(periodo, año, mes, rubroa12) %>%
  summarise(suma_monto           = sum(monto),
            suma_operaciones     = sum(operaciones),
            suma_monto_constante = sum(monto_constante)) %>%
  ungroup()

cuadro2 <- cuadro2 %>%
  arrange(periodo) %>%
  group_by(rubroa12) %>% 
  mutate(var_mensual_monto              = suma_monto / lag(suma_monto, 1) - 1,
         var_interanual_monto           = suma_monto / lag(suma_monto, 12) - 1,
         var_mensual_operaciones        = suma_operaciones / lag(suma_operaciones, 1) - 1,
         var_interanual_operaciones     = suma_operaciones / lag(suma_operaciones, 12) - 1,
         var_mensual_monto_constante    = suma_monto_constante / lag(suma_monto_constante, 1) - 1,
         var_interanual_monto_constante = suma_monto_constante / lag(suma_monto_constante, 12) - 1) %>% 
  ungroup()

cuadro2_separados <- split(cuadro2, cuadro2$año) # todos los años separados. Esta base tiene todos los rubros, no solo los primeros 8 y el resto agrupado
# este df "cuadro2_separados" se podria guardar

### Para slide ventas por rubro ----

principales_7_rubros_monto <- cuadro2 %>% filter(periodo == max(periodo)) %>% arrange(desc(suma_monto)) %>% head(7) # para grafico barras

auxi_rubros1_monto <- unique(principales_7_rubros_monto$rubroa12) # vector con los princaples 7 rubros

cuadro3_monto <- base %>%
  mutate(principales_rubros = if_else(rubroa12 %in% auxi_rubros1_monto, rubroa12, "Otros")) %>%
  group_by(periodo, año, mes, principales_rubros) %>%
  summarise(suma_monto           = sum(monto),
            suma_operaciones     = sum(operaciones),
            suma_monto_constante = sum(monto_constante)) %>%
  ungroup()
  
auxi_rubros2 <- cuadro3_monto %>% filter(periodo == max(periodo)) %>% distinct(mes) %>% pull(1) # mes corriente del análisis (lo saca del máximo periodo disponible)

cuadro3_rubros_participaciones_monto <- cuadro3_monto %>% filter(mes == auxi_rubros2, año >= max(año) - 1)
# quedarme con la variable monto, en df separados para cada mes, sacar la participación de cada rubro en el total.

cuadro3_rubros_participaciones_monto <- cuadro3_rubros_participaciones_monto %>%
  select(-suma_operaciones, -suma_monto_constante) %>%
  group_by(periodo) %>% 
  mutate(participacion = suma_monto/sum(suma_monto)) %>% 
  ungroup()

cuadro3_rubros_participaciones_monto <- cuadro3_rubros_participaciones_monto %>% split(.$año) # para gráfico de participaciones

### Para slide operaciones por rubro ----

principales_7_rubros_operaciones <- cuadro2 %>% filter(periodo == max(periodo)) %>% arrange(desc(suma_operaciones)) %>% head(8) # para grafico barras

auxi_rubros1_operaciones <- unique(principales_7_rubros_operaciones$rubroa12) # vector con los princaples 7 rubros

cuadro3_operaciones <- base %>%
  mutate(principales_rubros = if_else(rubroa12 %in% auxi_rubros1_operaciones, rubroa12, "Resto")) %>%
  group_by(periodo, año, mes, principales_rubros) %>%
  summarise(suma_monto           = sum(monto),
            suma_operaciones     = sum(operaciones),
            suma_monto_constante = sum(monto_constante)) %>%
  ungroup()


auxi_rubros2 <- cuadro3_operaciones %>% filter(periodo == max(periodo)) %>% distinct(mes) %>% pull(1) # mes corriente del análisis (lo saca del máximo periodo disponible)

cuadro3_rubros_participaciones_operaciones <- cuadro3_operaciones %>% filter(mes == auxi_rubros2, año >= max(año) - 1)

cuadro3_rubros_participaciones_operaciones <- cuadro3_rubros_participaciones_operaciones %>%
  select(-suma_monto, -suma_monto_constante) %>%
  group_by(periodo) %>% 
  mutate(participacion = suma_operaciones/sum(suma_operaciones)) %>% 
  ungroup()

cuadro3_rubros_participaciones_operaciones <- cuadro3_rubros_participaciones_operaciones %>% split(.$año)

### Para slide Participación de las distintas líneas de financiamiento ----


# HAY CUOTAS QUE SON 7, OTRAS 64. VER QUE HACER CON ESO, CONSULTAR, me fije y son totalmente insignificantes

cuotas_monto <- base %>%
  group_by(periodo, año, mes, rubroa12, cuotas) %>%
  summarise(suma_monto = sum(monto)) %>%
  ungroup()

cuotas_monto_participacion <- cuotas_monto %>%
  group_by(periodo, año, mes, rubroa12) %>% 
  mutate(participacion = suma_monto / sum(suma_monto)) %>% 
  ungroup()

cuotas_monto_participacion <- cuotas_monto_participacion %>% filter(rubroa12 %in% auxi_rubros1_monto) 

auxi_participaciones <- cuotas_monto_participacion %>% filter(periodo == max(periodo)) %>% distinct(mes) %>% pull(1)

export_cuotas <- bind_rows(cuotas_monto_participacion %>% filter(periodo == add_months(max(periodo), -1)),
cuotas_monto_participacion %>% filter(mes == auxi_participaciones, año >= max(año) - 1)) # para cuadro barras horizontal

# hacer el grafico de participaciones 

sad <- base %>%
  group_by(periodo, año, mes, cuotas) %>%
  summarise(suma_monto = sum(monto)) %>%
  ungroup() %>%
  group_by(periodo, año, mes) %>% 
  mutate(participacion = suma_monto / sum(suma_monto)) %>% 
  ungroup() # HAY CUOTAS QUE SON 7, OTRAS 64. VER QUE HACER CON ESO, CONSULTAR








### 02.03.A hoja "mayores ventas 2021" del xlsx 

cuadro2_2021 <- cuadro2_separados$`2021`
cuadro2_2021 <- cuadro2_2021 %>% select(-suma_operaciones, -año, -periodo) %>%
  pivot_wider(names_from = mes, values_from = suma_monto) %>% adorn_totals(where = c("row", "col"))

### 02.03.B Ventas por rubro filmina 7 

#cuadro2_ultimos_meses <- cuadro2 %>% filter(periodo >= add_months(max(periodo), -1)) %>% split(.$periodo) %>% adorn_totals(where = c("row"))

cuadro2 %>% arrange(periodo) %>% group_by(rubroa12) %>% 
  mutate(var_mensual = suma_monto / lag(suma_monto, 1)) %>% ungroup()

