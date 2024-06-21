# -------------- Programa extracción tendencia serie de tiempo -----------/
# -------------- PIB de Colombia                               -----------/
# autor: Arturo Gonzalez
# fuente de datos: https://econcifras.com/wp-content/uploads/2024/05/1.2-Historico-Constantes-de-2015-desestacionalizado.xls

# Carga de librerias ----
library(pacman)
pacman::p_load(readxl, TSstudio, mFilter, hpfilter, dplyr
               , tictoc, ggplot2,xts)

# lectura de la data ----
archivo <- file.choose()
Datos_ent <- read_xls(path = archivo,sheet = "PIB Histórico",range = "a7:c196"
                      ,col_names = T)
names(Datos_ent)
head(Datos_ent)
tail(Datos_ent)

## Creacion fechas ---------------------------------------------------------

fechas <- seq.Date(from = as.Date("1977-03-01"),to = as.Date("2024-03-01")
                   ,by = "quarter")
fechas

Datos_ent0 <- Datos_ent %>% select(Trimestral,`Variación anual %`) %>% cbind(.,fechas) %>% 
              relocate(fechas)
names(Datos_ent0)


# Aplicacion filtro ------

## met1 ----
## Construccion integral
tendencia1 <- hpfilter::hp1(na.omit(Datos_ent0[,2:3]),lambda = 1600) %>% 
             cbind(fechas[5:length(fechas)],.) %>% 
             rename("fecha"="fechas[5:length(fechas)]"
                    ,"Tend_PIB_niv_met1"="Trimestral"
                    ,"Tend_var_YoY_met1"="Variación anual %")
tail(tendencia1)

## met2 ----
### convertir objetos dataframe a serie de tiempo
Datos_ent_xts <- xts(Datos_ent0[,-1],order.by = Datos_ent0$fechas)
class(Datos_ent_xts)

tendencia2 <- mFilter::hpfilter(x = Datos_ent_xts$Trimestral[5:dim(Datos_ent_xts)[1],]
                               ,freq = 1600,type = "lambda")

## Construccion integral
tendencia2 <- Datos_ent0 %>% mutate(Tend_PIB_niv_met2=hpfilter(.$Trimestral
                                                              ,freq = 1600
                                                              ,type = "lambda")$trend
                                  ) %>% filter(fechas>"1977-12-01") %>%
                            mutate(Tend_var_YoY_met2=hpfilter(.$`Variación anual %`
                                                              ,freq = 1600
                                                              ,type = "lambda")$trend
                                   )

# Graficas ----------------------------------------------------------------
## Union bases con filtros -----
names(Datos_ent0)
names(tendencia1)
Base_graficas <- Datos_ent0 %>% left_join(tendencia1,by = join_by(fechas==fecha)) %>% 
                                left_join(tendencia2,by = join_by(fechas==fechas)) %>% 
                 select(-fechas) %>% xts::xts(.,order.by=as.Date(Datos_ent0$fechas))
class(Base_graficas)

windows()
plot(Base_graficas)

TSstudio::ts_plot(Base_graficas)

# ---------------------------------------------------------------------------/

