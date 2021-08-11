rm(list = ls())
# 01. Carga de liberías y bases -------------------------------------------

library(tidyverse)
library(readxl)
library(clock)
library(writexl)


base_raw <- read_csv("01.Bases/01.Raw/A12_mensual_20210701/A12_mensual_20210701.csv") %>%
  rename("año" = anio) %>%
  mutate(periodo = date_build(año, mes)) %>% 
  select(-año, -mes)
  
base <- base_raw  %>% rename(rubro_a_repartir = rubroa12)

ponderaciones <- readRDS("01.Bases/02.Clean/pond_plataformas.rds") %>% select(-operaciones, -monto, -ultimo_periodo_valido)

# 02. Procesamiento -------------------------------------------------------

unique(base$marca_medio_pago) # medios de pagos en la base
unique(base$rubro_a_repartir) # rubros en la base
if(nrow(distinct(ponderaciones, plataforma)) != 1) print("HAY MAS DE UNA PLATAFORMA EN LA BASE")

# hacer un par de medidas descriptivas de cuanto es First Data E-Commerce y Otro en el total de la base por meses

## First Data E-Commerce y Otros ----

first_data_y_otros <- base %>% filter(rubro_a_repartir %in% c("First Data E-Commerce", "Otros")) # me quedo solo con estos 2 rubros para repartir

first_data_y_otros %>% distinct(provincia, rubro_a_repartir, marca_medio_pago) %>% arrange(rubro_a_repartir) # provincias, rubros, y marcas

### Reparto gastos ----

# total

totales <- first_data_y_otros %>% # gasto total por provincia y periodo
   group_by(periodo, provincia) %>% 
   summarise(monto       = sum(monto),
             operaciones = sum(operaciones)) %>% 
   ungroup() %>% 
   arrange(periodo, provincia)

auxi_totales <- totales %>%
  left_join(ponderaciones %>% filter(cuotas == "Total"), by = "periodo")

auxi_totales %>% # chequeo
  group_by(periodo, provincia) %>%  
  summarise(sum   = sum(participacion_monto_por_cuota),
            errew = sum(participacion_operaciones_por_cuota))

auxi_totales <- auxi_totales %>% # monto total por rubro
  mutate(monto_auxi        = monto * participacion_monto_por_cuota, 
         operaciones_auxi  = operaciones * participacion_operaciones_por_cuota)

auxi_totales <- auxi_totales %>%
  mutate(operaciones_auxi = if_else(operaciones_auxi < 1, 1, round(operaciones_auxi)))

auxi_totales %>% # chequeo
  group_by(periodo, provincia) %>%
  summarise(sum(monto_auxi),
            sum(operaciones_auxi)) %>%
  arrange(desc(periodo))

auxi_totales <- auxi_totales %>% # df para el paso siguiente de repartir en cuotas y rubro
  select(periodo, provincia, rubroa12, monto_auxi, operaciones_auxi)

# cuotas

auxi_cuotas <- ponderaciones %>%
  filter(cuotas != "Total") %>%
  inner_join(auxi_totales, by = c("periodo", "rubroa12"))

auxi_cuotas %>% # chequeo
  group_by(periodo, rubroa12, provincia) %>%
  summarise(asd = sum(participacion_monto_por_cuota)) 

auxi_cuotas <- auxi_cuotas %>% # calculo los montos y operaciones por cuotas y rubro
          mutate(monto       = monto_auxi * participacion_monto_por_cuota,
                 operaciones = round(operaciones_auxi * participacion_operaciones_por_cuota)) %>% 
          mutate(operaciones = if_else(operaciones == 0, 1, operaciones))

base_a_agregar <- auxi_cuotas %>% # base que tiene los gastos de E-commerce y Otros repartidos en rubros por periodo, cuota y provincia
  select(-participacion_monto_por_cuota, -participacion_operaciones_por_cuota, -monto_auxi, -operaciones_auxi, -plataforma)

# Esta base debe ser agregada a la base total que previamente se le saco lo de ecommerce y otros

# 03.Bind de bases y exportación ---------------------------------------------

base_final_1 <- base_a_agregar %>% mutate(marca_medio_pago = "repartido_ecommerce_otros") # base con los gastos "E-Commerce" y "otros" repartidos

base_final_2 <- base_raw %>% filter(!rubroa12 %in% c("First Data E-Commerce", "Otros")) %>% mutate(cuotas = factor(cuotas)) # base raw sin "E-Commerce" y "otros"

export <- bind_rows(base_final_1, base_final_2)

saveRDS(export, "01.Bases/02.Clean/base_a12.rds")

write_xlsx(base_final_1, "03.Output/gastos_repartidos.xlsx")
