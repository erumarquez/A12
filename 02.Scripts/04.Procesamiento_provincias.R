rm(list = ls())
# 01. Carga de librerías y bases -------------------------------------------

library(tidyverse)
library(readxl)
library(clock)
library(writexl)


base <- readRDS("01.Bases/02.Clean/base_final_a12.rds")


# 02. Procesamiento -------------------------------------------------------


cuadro_1 <- base %>%
  group_by(periodo, provincia) %>% 
  summarise(monto = sum(monto)) %>% 
  ungroup() %>% 
  group_by(periodo) %>% 
  mutate(part_en_pais = monto / sum(monto)) %>% 
  ungroup()

cuadro_2 <- base %>%
  group_by(periodo, provincia) %>% 
  summarise(operaciones = sum(operaciones)) %>% 
  ungroup() %>% 
  group_by(periodo) %>% 
  mutate(part_en_pais = operaciones / sum(operaciones)) %>% 
  ungroup()

principales_7_rubros_monto <- base %>% 
  filter(periodo >= "2021-01-01") %>% 
  group_by(provincia, rubroa12) %>% 
  summarise(monto = sum(monto)) %>% 
  ungroup() %>% 
  group_by(provincia) %>% 
  arrange(desc(monto)) %>%
  filter(row_number() <= 7) %>% 
  ungroup() %>% 
  arrange(provincia) %>%
  select(-monto) %>% 
  mutate(principal = TRUE)

asd <- base %>% left_join(principales_7_rubros_monto, by = c("provincia", "rubroa12"))
