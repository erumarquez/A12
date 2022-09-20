library(lubridate)

dia_hoy = Sys.Date()

mes_anterior=dia_hoy %m-% months(1)
fecha_final=mes_anterior - day(dia_hoy)+1

ipc <- read_excel("01.Bases/01.Raw/ipc.xlsx") %>% mutate(periodo = as.Date(periodo))

mes_base <- ipc %>% filter(periodo == fecha_final) %>% pull(2)

ipc <- ipc %>%
  mutate(ipc_base_jun22    = ipc_indec_ng / mes_base * 100, # creo índice con base junio 2022 = 100. Por el momento cambiar a mano. Se está trabajando en el movible. 
         coef_deflactor    = ipc_indec_ng / mes_base,
         variacion_mensual = (ipc_base_jun22 / lag(ipc_base_jun22, 1, order_by = periodo) - 1))


