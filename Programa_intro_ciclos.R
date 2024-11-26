# -------------- Programa extracción tendencia serie de tiempo -----------/
# -------------- PIB de Colombia                               -----------/
# autor: Arturo Gonzalez
# fuente de datos: https://econcifras.com/wp-content/uploads/2024/05/1.2-Historico-Constantes-de-2015-desestacionalizado.xls

# Carga de librerias ----
library(pacman)
pacman::p_load(readxl, TSstudio, mFilter, hpfilter, dplyr
               , tictoc, ggplot2,xts, janitor)

# lectura de la data ----
archivo <- file.choose()
Datos_ent <- read_xls(path = archivo,sheet = "PIB Histórico",range = "a7:c197"
                      ,col_names = T)
names(Datos_ent)
head(Datos_ent)
tail(Datos_ent)

## Creacion fechas ---------------------------------------------------------

fecha <- seq.Date(from = as.Date("1977-03-01"),to = as.Date("2024-06-01")
                   ,by = "quarter")
fecha

Datos_ent0 <- Datos_ent %>% select(Trimestral,`Variación anual %`) %>% cbind(.,fecha) %>% 
              relocate(fecha) |> clean_names() |> 
              mutate(PIB_4T=zoo::rollsum(x=trimestral,k = 4,fill = NA,align = "right")
                     ,PIB_4T_YoY=(PIB_4T/lag(PIB_4T,n = 4))-1 )
names(Datos_ent0)
head(Datos_ent0)
dim(Datos_ent0)

# Aplicacion filtro ------

## met1 ----
## Construccion integral
tendencia1 <- hpfilter::hp1(na.omit(Datos_ent0[,2:5]),lambda = 1600) |>  
             cbind(fecha[8:length(fecha)]) |>  
             rename("fecha"="fecha[8:length(fecha)]"
                    ,"Tend_PIB_niv_met1"="trimestral"
                    ,"Tend_var_YoY_met1"="variacion_anual_percent") |> 
             rename_with(~paste0(.,"_met1"),starts_with("PIB") ) |> relocate(fecha)
tail(tendencia1)

## met2 ----
### convertir objetos dataframe a serie de tiempo
Datos_ent_xts <- xts(Datos_ent0[,-1],order.by = Datos_ent0$fecha)
class(Datos_ent_xts)

tendencia2 <- mFilter::hpfilter(x = na.omit(Datos_ent_xts$trimestral)
                               ,freq = 1600,type = "lambda")
tendencia2$trend

## Construccion integral
Datos_ent_xts

tendencia2 <- Datos_ent0 %>% na.omit() |> 
                            mutate(Tend_PIB_niv_met2=hpfilter(PIB_4T
                                                              ,freq = 1600
                                                              ,type = "lambda")$trend
                                  ) |> 
                            mutate(Tend_var_YoY_met2=hpfilter(PIB_4T_YoY
                                                              ,freq = 1600
                                                              ,type = "lambda")$trend
                                   )
tendencia2

# Graficas ----------------------------------------------------------------
## Union bases con filtros -----
names(Datos_ent0)
names(tendencia1)
names(tendencia2)
Base_graficas <- Datos_ent0 |> left_join(tendencia1,by = join_by(fecha==fecha)) |>  
                               left_join(tendencia2[,c(1,6:7)],by = join_by(fecha==fecha)) |> 
                 select(-fecha) |> mutate(brecha_PIB_met1 = (PIB_4T-PIB_4T_met1)/PIB_4T_met1,
                                          brecha_PIB_met2 = (PIB_4T-Tend_PIB_niv_met2)/Tend_PIB_niv_met2,
                                          ciclo_PIB_met1  = (PIB_4T_YoY-Tend_var_YoY_met1),
                                          ciclo_PIB_met2  = (PIB_4T_YoY-Tend_var_YoY_met2)
                                    ) |> 
                 xts::xts(order.by=as.Date(Datos_ent0$fecha))
tail(Base_graficas)

windows()
plot(Base_graficas)

TSstudio::ts_plot(Base_graficas)

# ---------------------------------------------------------------------------/
# Fin de programa ----
# ---------------------------------------------------------------------------/

