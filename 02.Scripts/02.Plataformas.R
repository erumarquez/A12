rm(list = ls())

# 01. Seleccionar el último mes con datos completos -----------------------



                                                    mes <- "2021-07-01"



# 02. Carga de liberías y bases -------------------------------------------

library(tidyverse)
library(readxl)
library(echarts4r)
library(clock)

rubrosa12 <- read_csv2("01.Bases/01.Raw/base_corregida_nueva.csv") %>% distinct(rubroa12)

plataformas <- read_csv("01.Bases/01.Raw/nuevo/A12_plataformas_mensual_20210711.csv") %>% rename(año = anio) %>% mutate(periodo = date_build(año, mes))

plataformas <- plataformas %>% filter(periodo < as.Date(!!mes))

filas_con_missing <- plataformas %>% filter(!complete.cases(.))


# 03. Información sobre la base --------------------------------------------------

print(paste("Cantidad de filas con algun missing value:", nrow(filas_con_missing)))
unique(plataformas$rubroa12)
unique(plataformas$plataforma)
unique(plataformas$provincia)

## Se transforma "Máquinas y Herramientas" en "Materiales para la construcción" ----
plataformas <- plataformas %>% mutate(rubroa12 = if_else(rubroa12=="Máquinas y Herramientas", "Materiales para la construcción", rubroa12))


## Operaciones y monto por provincia, por plataforma y periodo ----
resumen1 <- plataformas %>%
  arrange(periodo) %>% 
  group_by(periodo, plataforma, provincia) %>% 
  summarise(operaciones = sum(operaciones),
                          monto = sum(monto)) %>% 
  arrange(operaciones, monto) %>% 
  ungroup()


split(resumen1, resumen1$provincia)

### Gráfico serie temporal de monto por provincia ----
resumen1 %>% arrange(periodo) %>% 
  group_by(provincia) %>% 
  e_charts(x = periodo) %>% 
  e_line(monto) %>%
  e_tooltip(trigger = "item", axisPointer = list(type = "cross"))

### Gráfico serie temporal de operaciones por provincia ----
resumen1 %>% arrange(periodo) %>% 
  group_by(provincia) %>% 
  e_charts(x = periodo) %>% 
  e_line(operaciones) %>%
  e_tooltip(trigger = "item", axisPointer = list(type = "cross"))

## Operaciones y monto por rubro y periodo ----
resumen2 <- plataformas %>%
  group_by(periodo, rubroa12) %>% 
  summarise(operaciones = sum(operaciones),
            monto = sum(monto)) %>% 
  arrange(operaciones, monto) %>% 
  ungroup()

### Gráfico serie temporal de monto por rubro ----
resumen2 %>% arrange(periodo) %>% 
  group_by(rubroa12) %>% 
  e_charts(x = periodo) %>% 
  e_line(monto) %>%
  e_tooltip(trigger = "item", axisPointer = list(type = "cross"))

### Gráfico serie temporal de operaciones por rubro ----
resumen2 %>% arrange(periodo) %>% 
  group_by(rubroa12) %>% 
  e_charts(x = periodo) %>% 
  e_line(operaciones) %>%
  e_tooltip(trigger = "item", axisPointer = list(type = "cross"))

## Cuotas ----

resumen3 <- plataformas %>%
  arrange(periodo) %>% 
  group_by(periodo, plataforma, cuotas) %>% 
  summarise(operaciones = sum(operaciones),
            monto = sum(monto)) %>% 
  arrange(operaciones, monto) %>% 
  ungroup()

### Gráfico serie temporal monto por cuotas ----
resumen3 %>% arrange(periodo) %>% 
  group_by(cuotas) %>% 
  e_charts(x = periodo) %>% 
  e_line(monto)

### Gráfico serie temporal operaciones por cuotas ----
resumen3 %>% arrange(periodo) %>% 
  group_by(cuotas) %>% 
  e_charts(x = periodo) %>% 
  e_line(operaciones)


# 04. Cálculo de participaciones de las cuotas por rubros -----------------

## Aquí se obtiene la estructura de gasto por cuotas de mercado libre (BASADO EN LOS ULTIMOS 6 MESES) ----

asd <- plataformas %>% distinct(rubroa12) %>% mutate(re = "das") %>% full_join(rubrosa12 %>% mutate(asd = "FAsd"))

resumen4 <- plataformas %>%
  arrange(periodo) %>% 
  group_by(periodo, plataforma, rubroa12, cuotas) %>% 
  summarise(operaciones = sum(operaciones),
            monto = sum(monto)) %>% 
  arrange(operaciones, monto) %>% 
  ungroup()

ultimos_6_meses <- resumen4 %>% filter(periodo > add_months(max(periodo), -6)) %>% arrange(periodo)

# sacar estructura de gastos de estos 6 meses. 

resumen5 <- ultimos_6_meses %>% 
  group_by(plataforma, rubroa12, cuotas) %>% 
  summarise(operaciones = sum(operaciones),
            monto = sum(monto)) %>% 
  arrange(operaciones, monto) %>% 
  ungroup()

resumen_5_participacion <- resumen5 %>%
  group_by(plataforma, cuotas) %>% 
  mutate(participacion_monto_por_cuota = monto / sum(monto),
         participacion_operaciones_por_cuota = operaciones / sum(operaciones)) %>% 
  ungroup()

resumen_5_participacion_total <- resumen5 %>%
  group_by(plataforma, rubroa12) %>% 
  summarise(monto = sum(monto),
            operaciones = sum(operaciones)) %>%
  ungroup() %>% 
  group_by(plataforma) %>% 
  mutate(participacion_monto_por_cuota = monto / sum(monto),
         participacion_operaciones_por_cuota = operaciones / sum(operaciones),
         cuotas = "Total") %>% 
  ungroup()

resumen_5_participacion <- resumen_5_participacion %>% arrange(cuotas) %>% mutate(cuotas = as.factor(cuotas)) %>% 
  bind_rows(resumen_5_participacion_total) %>% mutate(cuotas = as.factor(cuotas))

### Gráfico participacion monto por cuota y total ----
resumen_5_participacion %>% 
  group_by(rubroa12) %>% 
  e_charts(x = cuotas) %>% 
  e_bar(participacion_monto_por_cuota) %>%
  e_tooltip(
    formatter = e_tooltip_item_formatter("percent", digits = 1)
  )

### Gráfico participacion operaciones por cuota y total ----
resumen_5_participacion %>% 
  group_by(rubroa12) %>% 
  e_charts(x = cuotas) %>% 
  e_bar(participacion_operaciones_por_cuota) %>%
  e_tooltip(
    formatter = e_tooltip_item_formatter("percent", digits = 1)
  )

### Gráfico monto por cuota y total ----
resumen_5_participacion %>% 
  group_by(rubroa12) %>% 
  e_charts(x = cuotas) %>% 
  e_bar(monto) %>%
  e_tooltip(
    formatter = e_tooltip_item_formatter()
  )

### grafico operaciones por cuota y total ----
resumen_5_participacion %>% 
  group_by(rubroa12) %>% 
  e_charts(x = cuotas) %>% 
  e_bar(operaciones) %>%
  e_tooltip(
    formatter = e_tooltip_item_formatter()
  )

## Guardado base ----

pond_export <- resumen_5_participacion %>% mutate(ultimo_periodo_valido = paste(max(plataformas$periodo)))

# tener cuidado de no pisar el mes pasado
# saveRDS(pond_export, "01.Bases/02.Clean/pond_plataformas.rds")

