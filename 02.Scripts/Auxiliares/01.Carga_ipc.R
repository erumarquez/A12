ipc <- read_excel("01.Bases/01.Raw/ipc.xlsx") %>% mutate(periodo = as.Date(periodo))

mes_base <- ipc %>% filter(periodo == "2019-01-01") %>% pull(2)

ipc <- ipc %>%
  mutate(ipc_base_ene19    = ipc_indec_ng / mes_base * 100, # creo índice con base enero 2019 = 100
         coef_deflactor    = ipc_indec_ng / mes_base,
         variacion_mensual = (ipc_base_ene19 / lag(ipc_base_ene19, 1, order_by = periodo) - 1))
