rm(list = ls())

# 01. Seleccionar el último mes con datos completos -----------------------



mes <- "2021-09-01"



# 02. Carga de librerías y bases -------------------------------------------

paquetes <- c("tidyverse", "readxl", "echarts4r", "clock", "writexl", "googledrive", "googlesheets4")
lapply(paquetes, library, character.only = TRUE)

poblacion <- readRDS("01.Bases/02.clean/poblacion.rds")

source("02.Scripts/Auxiliares/01.Carga_ipc.R") # carga el xlsx de ipc y genera variables para deflactar en determinado período base

agrupadores <- read_csv("01.Bases/01.Raw/A12_agrupadores_meli_mensual_20211004.csv") |>  # lectura de base agrupadores
  rename(año = anio, rubroa12 = descrubroa12) |> 
  mutate(periodo = date_build(año, mes)) |> 
  filter(agrupador != "Mercado Libre")


# 03.Procesamiento --------------------------------------------------------

agrupadores_en_base <- agrupadores |> 
  distinct(agrupador)

agrupadores |> distinct(cuotas) |> arrange(cuotas)

## Filtro las cuotas que quiero ----
agrupadores <- agrupadores %>% filter(periodo <= as.Date(!!mes), # filtro periodos menores o igual al señalado arriba
                                      cuotas %in% c(3, 6, 12, 18, 24, 30)) # filtro cuotas 3, 6, 12 y 18



## Se colapsan 3 rubros en 1 ----

base <- agrupadores %>%
  mutate(rubroa12 = if_else(rubroa12 %in% c("Pequeños electrodomésticos", "Línea Blanca", "Televisores"),
                            "Electrodomésticos",
                            rubroa12))

unique(base$rubroa12)


## Se transforma "Máquinas y Herramientas" en "Materiales para la construcción" ----
base <- base %>% mutate(rubroa12 = if_else(rubroa12 == "Máquinas y Herramientas", "Materiales para la construcción", rubroa12))


## Deflactación de monto, base enero 2019 ----

base <- base |> 
  left_join(ipc |> select(-ipc_indec_ng, -variacion_mensual), by = "periodo") %>% 
  mutate(monto_constante = monto / coef_deflactor)


## Primer Cuadro ----
apd_cuadro1 <- base |> 
  group_by(periodo) |> 
  summarise(operaciones = sum(operaciones),
            monto_corriente = sum(monto),
            monto_constante = sum(monto_constante)) |> 
  ungroup() |> 
  arrange(periodo) |> 
  mutate(var_mensual_corr           = monto_corriente / lag(monto_corriente, 1, order_by = periodo) - 1,
         var_mensual_constante      = monto_constante / lag(monto_constante, 1, order_by = periodo) - 1,
         var_mensual_operaciones    = operaciones / lag(operaciones, 1, order_by = periodo) - 1,
         var_interanual_corr        = monto_corriente / lag(monto_corriente, 12, order_by = periodo) - 1,
         var_interanual_constante   = monto_constante / lag(monto_constante, 12, order_by = periodo) - 1,
         var_interanual_operaciones = operaciones / lag(operaciones, 12, order_by = periodo) - 1)

## Participación monto y operaciones por agrupadores ----

part_agrupadores <- base |> 
  group_by(periodo, agrupador) |> 
  summarise(monto = sum(monto),
            operaciones = sum(operaciones)) |> 
  ungroup() |> 
  group_by(periodo) |> 
  mutate(part_monto = monto / sum(monto),
         part_op    = operaciones / sum(operaciones)) |> 
  ungroup() |> 
  arrange(periodo, agrupador, monto)

part_agrupadores_aux <- part_agrupadores |> 
  filter(periodo == max(periodo)) |> 
  arrange(desc(monto)) |> 
  distinct(agrupador)


## Principales 7 rubros del último mes por monto corriente ----
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
  filter(periodo %in% c(max(periodo), add_months(max(base$periodo), -1), add_months(max(base$periodo), -12))) %>%
  arrange(periodo) %>%
  group_by(rubro_auxiliar) %>% 
  mutate(var_mensual_monto_const = monto_constante / lag(monto_constante, 1, order_by = periodo) - 1,
         var_inter_monto_const   = monto_constante / lag(monto_constante, 2, order_by = periodo) - 1,
         var_mensual_monto_corr  = monto / lag(monto, 1, order_by = periodo) - 1,
         var_inter_monto_corr    = monto / lag(monto, 2, order_by = periodo) - 1) %>% 
  ungroup()

cuadro_4 <- cuadro_4 %>%
  arrange(periodo) %>%
  group_by(rubro_auxiliar) %>% 
  mutate(part_monto_rubro_en_mes_año_ant = lag(part_monto_rubro_en_mes, 2, order_by = periodo)) %>% 
  ungroup()

cuadro_4 <- cuadro_4 %>%
  mutate(auxi_ordena = if_else(rubro_auxiliar == "Otros", TRUE, FALSE)) %>% 
  arrange(periodo, auxi_ordena, desc(monto)) %>% 
  select(-auxi_ordena)


## Principales 7 rubros del último mes por operaciones ----

principales_7_rubros_operaciones <- base %>%
  filter(periodo == max(periodo)) %>%
  group_by(periodo, rubroa12) %>% 
  summarise(operaciones = sum(operaciones)) %>% 
  ungroup() %>% 
  arrange(desc(operaciones)) %>% 
  head(7)

aux_base_2 <- base %>% 
  mutate(agrupar        = if_else(rubroa12 %in% (principales_7_rubros_operaciones$rubroa12), FALSE, TRUE),
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

cuadro_5 <- cuadro_5 %>%
  mutate(auxi_ordena = if_else(rubro_auxiliar == "Otros", TRUE, FALSE)) %>% 
  arrange(periodo, auxi_ordena, desc(operaciones)) %>% 
  select(-auxi_ordena)

## Participaciones por cuotas. Total base ----

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

## Participaciones por cuotas. Para cada uno de los 7 principales rubros ----

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



## Var mensual e inter Monto y operaciones, ticket promedio por rubros principales ----

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


## cuadros per capita ----


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


# 02.Subo resultados a googlesheet ---------------------------------------------

## Autorización googlesheets ----
googledrive::drive_auth("mdointerno@gmail.com")
gs4_auth(token = drive_token())


## Leo googlesheets ----

resultados_a12_ggsheet <- as_sheets_id("https://docs.google.com/spreadsheets/d/1pGauusZX_xHyZGj6xFs3opBvlYeFG3Ow3rqFY9L2cCE/edit#gid=1705535184") %>%
  as.character()

gs4_browse(resultados_a12_ggsheet)

## Escribo ----

range_write(resultados_a12_ggsheet, 
            sheet = "Hoja 1",
            data = apd_cuadro1,
            reformat = FALSE,
            range = "A1")

range_write(resultados_a12_ggsheet, 
            sheet = "Hoja 2",
            data = part_agrupadores,
            reformat = FALSE,
            range = "B1")

range_write(resultados_a12_ggsheet, 
            sheet = "Hoja 2",
            data = part_agrupadores_aux,
            reformat = FALSE,
            col_names = FALSE,
            range = "K3")

range_write(resultados_a12_ggsheet, 
            sheet = "Hoja 3",
            data = cuadro_4,
            reformat = FALSE,
            range = "A1")

range_write(resultados_a12_ggsheet, 
            sheet = "Hoja 4",
            data = cuadro_5,
            reformat = FALSE,
            range = "A1")

range_write(resultados_a12_ggsheet, 
            sheet = "Hoja 5",
            data = cuadro_6_resumen,
            reformat = FALSE,
            range = "A1")

range_write(resultados_a12_ggsheet, 
            sheet = "Hoja 6",
            data = cuadro_7_resumen,
            reformat = FALSE,
            range = "A1")

range_write(resultados_a12_ggsheet, 
            sheet = "Hoja 7",
            data = cuadro_8,
            reformat = FALSE,
            range = "A1")

range_write(resultados_a12_ggsheet, 
            sheet = "Hoja 8",
            data = cuadro_8_1,
            reformat = FALSE,
            range = "A1")

range_write(resultados_a12_ggsheet, 
            sheet = "Hoja 9",
            data = cuadro_10_1,
            reformat = FALSE,
            range = "A1")

range_write(resultados_a12_ggsheet, 
            sheet = "Hoja 10",
            data = cuadro_10_2,
            reformat = FALSE,
            range = "A1")

range_write(resultados_a12_ggsheet, 
            sheet = "Hoja 11",
            data = cuadro_10_3,
            reformat = FALSE,
            range = "A1")

