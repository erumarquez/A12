rm(list = ls())

# 01. Seleccionar el último mes con datos completos -----------------------



mes <- "2022-08-01"



# 02. Carga de librerías y bases -------------------------------------------

paquetes <- c("tidyverse", "readxl", "echarts4r", "clock", "writexl", "googledrive", "googlesheets4")
invisible(lapply(paquetes, library, character.only = TRUE))

rubrosa12 <- read_excel("01.Bases/01.Raw/rubrosa12.xlsx") # De vez en cuando revisar esta lista de rubros

source("02.Scripts/Auxiliares/01.Carga_ipc.R") # carga el xlsx de ipc y genera variables para deflactar en determinado período base

  plataformas <- read_csv("01.Bases/01.Raw/A12_plataformas_mensual_20220905.csv") %>% # lectura de base plataformas
  rename(año = anio) %>%
  mutate(periodo = date_build(año, mes))
  
  # #write_xlsx(plataformas, "plataforma.test.xlsx")
  # 
  # plataformas <- read_xlsx("plataforma.test.xlsx")

## 02.01 Filtro solo mercado libre ----
plataformas <- plataformas %>% filter(plataforma == "Mercado Libre") # me quedo solo con mercado libre

plataformas |> distinct(cuotas) |> arrange(cuotas)

## 02.01 Filtro las cuotas que quiero ----
plataformas <- plataformas %>% filter(periodo <= as.Date(!!mes), # filtro periodos menores o igual al señalado arriba
                                      cuotas %in% c(3, 6, 12, 18, 24)) # filtro cuotas 3, 6, 12 y 18

filas_con_missing <- plataformas %>% filter(!complete.cases(.))

# todos los meses de la base mayores a agosto 2021
aux1 <- plataformas |> 
  distinct(periodo) |> 
  filter(periodo > as.Date("2021-08-01")) |> 
  rename(periodo_todos = periodo)

# meses que tienen 24 cuotas
aux2 <- plataformas |> 
  filter(cuotas == 24, periodo > as.Date("2021-08-01")) |> 
  distinct(periodo) |> 
  rename(periodo_con_24 = periodo)

periodos_sin_24 <- aux1 |> 
  filter(!periodo_todos %in% aux2$periodo_con_24) |> 
  rename(periodos_sin_24 = 1)

periodos_sin_24

if(nrow(periodos_sin_24) > 0) {
  
  df_a_adicionar <- plataformas |> 
    filter(periodo > as.Date("2021-08-01")) |> 
    distinct(plataforma, provincia, rubroa12) |> 
    mutate(operaciones = 1, monto = 1, cuotas = 24) |> 
    left_join(tibble(plataforma = "Mercado Libre", periodo = periodos_sin_24$periodos_sin_24), by = "plataforma") |> 
    mutate(año = get_year(periodo), mes = get_month(periodo))
  
  plataformas <- plataformas |> 
    bind_rows(df_a_adicionar)
  
}

# 03. Información sobre la base --------------------------------------------------

print(paste("Cantidad de filas con algun missing value:", nrow(filas_con_missing)))
unique(plataformas$rubroa12)
unique(plataformas$plataforma)
unique(plataformas$provincia)

## Se transforma "Máquinas y Herramientas" en "Materiales para la construcción" ----
plataformas <- plataformas %>% mutate(rubroa12 = if_else(rubroa12 == "Máquinas y Herramientas", "Materiales para la construcción", rubroa12))

## Operaciones y monto por provincia, por plataforma y periodo ----
resumen1 <- plataformas %>%
  arrange(periodo) %>% 
  group_by(periodo, plataforma, provincia) %>% 
  summarise(operaciones = sum(operaciones),
            monto       = sum(monto)) %>% 
  arrange(operaciones, monto) %>% 
  ungroup()


split(resumen1, resumen1$provincia)

### Gráfico serie temporal de monto por provincia ----
resumen1 %>%
  arrange(periodo) %>% 
  group_by(provincia) %>% 
  e_charts(x = periodo) %>% 
  e_line(monto) %>%
  e_tooltip(trigger = "item", axisPointer = list(type = "cross"))

### Gráfico serie temporal de operaciones por provincia ----
resumen1 %>%
  arrange(periodo) %>% 
  group_by(provincia) %>% 
  e_charts(x = periodo) %>% 
  e_line(operaciones) %>%
  e_tooltip(trigger = "item", axisPointer = list(type = "cross"))

## Operaciones y monto por rubro y periodo ----
resumen2 <- plataformas %>%
  group_by(periodo, rubroa12) %>% 
  summarise(operaciones = sum(operaciones),
            monto       = sum(monto)) %>% 
  arrange(operaciones, monto) %>% 
  ungroup()

### Gráfico serie temporal de monto por rubro ----
resumen2 %>%
  arrange(periodo) %>% 
  group_by(rubroa12) %>% 
  e_charts(x = periodo) %>% 
  e_line(monto) %>%
  e_tooltip(trigger = "item", axisPointer = list(type = "cross"))

### Gráfico serie temporal de operaciones por rubro ----
resumen2 %>%
  arrange(periodo) %>% 
  group_by(rubroa12) %>% 
  e_charts(x = periodo) %>% 
  e_line(operaciones) %>%
  e_tooltip(trigger = "item", axisPointer = list(type = "cross"))

## Cuotas ----

resumen3 <- plataformas %>%
  arrange(periodo) %>% 
  group_by(periodo, plataforma, cuotas) %>% 
  summarise(operaciones = sum(operaciones),
            monto       = sum(monto)) %>% 
  arrange(operaciones, monto) %>% 
  ungroup()

### Gráfico serie temporal monto por cuotas ----
resumen3 %>%
  arrange(periodo) %>% 
  group_by(cuotas) %>% 
  e_charts(x = periodo) %>% 
  e_line(monto)

### Gráfico serie temporal operaciones por cuotas ----
resumen3 %>%
  arrange(periodo) %>% 
  group_by(cuotas) %>% 
  e_charts(x = periodo) %>% 
  e_line(operaciones)


# 04. Cálculo de participaciones de las cuotas por rubros -----------------

## Aquí se obtiene la estructura de gasto dentro de cada cuota en cada periodo, abierto por rubro ----

chequeo1 <- plataformas %>% distinct(rubroa12) %>% mutate(rubros_plataformas = "rubros_plataformas") %>% full_join(rubrosa12 %>% mutate(rubros_base_total = "rubros_base_total"))

resumen4 <- plataformas %>%
  arrange(periodo) %>% 
  group_by(periodo, plataforma, rubroa12, cuotas) %>% 
  summarise(operaciones = sum(operaciones),
            monto       = sum(monto)) %>% 
  arrange(operaciones, monto) %>% 
  ungroup()

resumen4 <- resumen4 %>%
  group_by(periodo, plataforma, cuotas) %>% 
  mutate(part_operaciones_en_cuota = operaciones / sum(operaciones),
         part_monto_en_cuota       = monto / sum(monto)) %>% 
  ungroup()

chequeo2 <- resumen4 %>% group_by(periodo, cuotas) %>% summarise(sum(part_operaciones_en_cuota), sum(part_monto_en_cuota)) %>% ungroup()

### Gráficos ----

# Gráficos de operaciones por cuotas

resumen4 %>%
  filter(cuotas == 3) %>% # participaciones de los rubros en la cuota 3
  group_by(rubroa12) %>%
  arrange(periodo) %>% 
  e_charts(x = periodo) %>% 
  e_line(part_operaciones_en_cuota) %>%
  e_tooltip()

resumen4 %>%
  filter(cuotas == 6) %>% # participaciones de los rubros en la cuota 6
  group_by(rubroa12) %>%
  arrange(periodo) %>% 
  e_charts(x = periodo) %>% 
  e_line(part_operaciones_en_cuota) %>%
  e_tooltip()

resumen4 %>%
  filter(cuotas == 12) %>% # participaciones de los rubros en la cuota 12
  group_by(rubroa12) %>%
  arrange(periodo) %>% 
  e_charts(x = periodo) %>% 
  e_line(part_operaciones_en_cuota) %>%
  e_tooltip()

resumen4 %>%
  filter(cuotas == 18) %>% # participaciones de los rubros en la cuota 18
  group_by(rubroa12) %>%
  arrange(periodo) %>% 
  e_charts(x = periodo) %>% 
  e_line(part_operaciones_en_cuota) %>%
  e_tooltip()


# Gráficos de monto por cuotas

resumen4 %>%
  filter(cuotas == 3) %>% # participaciones de los rubros en la cuota 3
  group_by(rubroa12) %>%
  arrange(periodo) %>% 
  e_charts(x = periodo) %>% 
  e_line(part_monto_en_cuota) %>%
  e_tooltip()

resumen4 %>%
  filter(cuotas == 6) %>% # participaciones de los rubros en la cuota 6
  group_by(rubroa12) %>%
  arrange(periodo) %>% 
  e_charts(x = periodo) %>% 
  e_line(part_monto_en_cuota) %>%
  e_tooltip()

resumen4 %>%
  filter(cuotas == 12) %>% # participaciones de los rubros en la cuota 12
  group_by(rubroa12) %>%
  arrange(periodo) %>% 
  e_charts(x = periodo) %>% 
  e_line(part_monto_en_cuota) %>%
  e_tooltip()

resumen4 %>%
  filter(cuotas == 18) %>% # participaciones de los rubros en la cuota 18
  group_by(rubroa12) %>%
  arrange(periodo) %>% 
  e_charts(x = periodo) %>% 
  e_line(part_monto_en_cuota) %>%
  e_tooltip()

### Para completar periodos para atrás ----

# La base tiene datos desde Abril de 2020, voy a completarla hacia atrás con el promedio de abril 2020 - sep 2020

para_atras <- resumen4 %>% filter(periodo <= "2020-09-01")

para_atras <- para_atras %>%
  group_by(plataforma, rubroa12, cuotas) %>%
  summarise(operaciones = sum(operaciones),
            monto       = sum(monto)) %>% 
  ungroup()

para_atras <- para_atras %>%
  group_by(plataforma, cuotas) %>% 
  mutate(part_operaciones_en_cuota = operaciones / sum(operaciones),
         part_monto_en_cuota       = monto / sum(monto)) %>% 
  ungroup()

para_completar <- para_atras %>% mutate(aux = 1) %>% left_join( # formo el df de períodos histórico
tibble(
periodo = seq(lubridate::ymd('2015-01-01'), lubridate::ymd('2020-03-01'), by = '1 month'),
aux = 1
)
) %>% select(-aux)

final_1 <- resumen4 %>% filter(periodo >= "2020-04-01")
final_2 <- para_completar

if (any(sort(unique(final_2$periodo)) %in% sort(unique(final_1$periodo)))) print("ATENCION ! CHEQUEAR PERIODOS PORQUE SE PISAN")

export <- bind_rows(final_1, final_2)

chequeo3 <- export %>%
  group_by(periodo, cuotas) %>% 
  summarise(sum(part_operaciones_en_cuota), sum(part_monto_en_cuota)) %>% 
  ungroup()


# 05. Exportación de base -------------------------------------------------

saveRDS(export, "01.Bases/02.Clean/pond_plataformas.rds")

#write_xlsx(export, "pond_plataformas.xlsx")

# exportar algun xlsx con hojas con datos en wide de las ponderaciones y cosas asi
