library(eia)
library(dplyr)
library(lubridate)
library(readxl)
library(ggpubr)

fileName <- 'eiakey.txt'


eia_set_key(readChar(fileName, file.info(fileName)$size), store = c("env", "options", "sysenv"))
setwd("C:/Users/fabian/Ecopetrol S.A/PPY - Documentos/Inventarios/Inventarios EE.UU")
#  714755 Petroleum
  #  714759 Refining and Processing
    # 

# Producción de gasolina
# PET.WGFRPUS2.W
# PET.WGFRPUS2.4


# Series a procesar -------------------------------------------------------



# Variables a graficar

crude_stocks <- eia_series("PET.WCRSTUS1.W")  # Incluye SPR
crude_stocks <- as.data.frame(crude_stocks$data)


crude_spr <- eia_series("PET.WCSSTUS1.W")
crude_spr <- as.data.frame(crude_spr$data)

cushing <- eia_series("PET.W_EPC0_SAX_YCUOK_MBBL.W")
cushing <- as.data.frame(cushing$data)


production <- eia_series("PET.WCRFPUS2.W")
production <- as.data.frame(production$data)

crude_imp <- eia_series("PET.WCRIMUS2.W")
crude_imp <- as.data.frame(crude_imp$data)

crude_exp <- eia_series("PET.WCREXUS2.W")
crude_exp <- as.data.frame(crude_exp$data)

crude_net_imp <- eia_series("PET.WCRNTUS2.W")
crude_net_imp <- as.data.frame(crude_net_imp$data)

crude_ref_int <- eia_series("PET.WCRRIUS2.W")
crude_ref_int <- as.data.frame(crude_ref_int$data)

oil_products_stocks <- eia_series("PET.WTTSTUS1.W") # Incluye SPR
oil_products_stocks <- as.data.frame(oil_products_stocks$data)

gasolina_stocks <- eia_series("PET.WGTSTUS1.W")
gasolina_stocks <- as.data.frame(gasolina_stocks$data)

destilados_stocks <- eia_series("PET.WDISTUS1.W")
destilados_stocks <- as.data.frame(destilados_stocks$data)

resid_stocks <- eia_series("PET.WRESTUS1.W")
resid_stocks <- as.data.frame(resid_stocks$data)

prop_stocks <- eia_series("PET.WPRSTUS1.W")
prop_stocks <- as.data.frame(prop_stocks$data)

asf_stocks <- eia_series("PET.W_EPPA_SAE_NUS_MBBL.W")
asf_stocks <- as.data.frame(asf_stocks$data)

jet_stocks <- eia_series("PET.WKJSTUS1.W")
jet_stocks <- as.data.frame(jet_stocks$data)

products_supplied_4 <- eia_series("PET.WRPUPUS2.4")
products_supplied_4 <- as.data.frame(products_supplied_4$data)

gasoline_supplied_4 <- eia_series("PET.WGFUPUS2.4")
gasoline_supplied_4 <- as.data.frame(gasoline_supplied_4$data)

destillate_supplied_4 <- eia_series("PET.WDIUPUS2.4")
destillate_supplied_4 <- as.data.frame(destillate_supplied_4$data)

jet_supplied_4 <- eia_series("PET.WKJUPUS2.4")
jet_supplied_4 <- as.data.frame(jet_supplied_4$data)

prop_supplied_4 <- eia_series("PET.WPRUP_NUS_2.4")
prop_supplied_4 <- as.data.frame(prop_supplied_4$data)

red_supplied_4 <- eia_series("PET.WREUPUS2.4")
red_supplied_4 <- as.data.frame(red_supplied_4$data)

gasolina_oferta <- eia_series("PET.WGFRPUS2.W")
gasolina_oferta <- as.data.frame(gasolina_oferta$data)

kero_oferta <- eia_series("PET.WKJRPUS2.W")
kero_oferta <- as.data.frame(kero_oferta$data)

desti_oferta <- eia_series("PET.WDIRPUS2.W")
desti_oferta <- as.data.frame(desti_oferta$data)

residuos_oferta <- eia_series("PET.WRERPUS2.W")
residuos_oferta <- as.data.frame(residuos_oferta$data)

propano_oferta <- eia_series("PET.WPRTP_NUS_2.W")
propano_oferta <- as.data.frame(propano_oferta$data)

gasoline_exports <- eia_series("PET.W_EPM0F_EEX_NUS-Z00_MBBLD.W")
gasoline_exports <- as.data.frame(gasoline_exports$data)



# Funciones ---------------------------------------------------------------



# Función para extraer el máximo, mínimo y promedio de los últimos 5 años de cada dataframe.

conversion <- function(df,year) {
  #year = 2005
  #data = yields_gasol
  data <- df
  data <- data %>% mutate(year = isoyear(date))
  data <- data %>% mutate(week = isoweek(date))
  filtros <- c(year-1,year-2,year-3,year-4,year-5)
  data <- data %>% filter(year %in% filtros)
  data <- data %>% filter(week != 53)
  data <- data %>% group_by(week) %>% summarise(promedio = mean(value),
                                                max = max(value),
                                                min = min(value))
  data$year <- year
  return(data)
}

valor_previo <- function(df,year) {
  data <- df
  data <- data %>% mutate(year = isoyear(date))
  data <- data %>% mutate(week = isoweek(date))
  filtros <- c(year-1)
  data <- data %>% filter(year %in% filtros)
  data <- data %>% filter(week != 53)
  data <- data %>% rename(value_previo = value)
  data$year <- year
  data <- data %>% select(year,week,value_previo)
  return(data)
}
  


# Crudos ------------------------------------------------------------------




# A cada data frame de arriba le voy procesar para sacar media, min y maximo

# Crude Stocks Vamos a sacar el dataframe sin spr

crude_stocks <- crude_stocks %>% rename(con_spr = value)
crude_spr <- crude_spr %>% rename(spr = value)

crude_stocks <- inner_join(crude_stocks, crude_spr, by = c("date","year","month","week"))
crude_stocks <- crude_stocks %>% mutate(sin_spr = con_spr - spr)
crude_stocks <- crude_stocks %>% select(date,year,month,week,sin_spr)
crude_stocks <- crude_stocks %>% rename(value = sin_spr)
crude_stocks <- crude_stocks %>% mutate(year = isoyear(date),
                                        week = isoweek(date))


# Crude_stocks Le agregamos el max, min, promedio y el valor previo.

# max, min y promedio
MediaInv <- data.frame(year = numeric(),
                  week = numeric(),
                  promedio = numeric(),
                  max = numeric(),
                  min = numeric())


years <- names(table(crude_stocks$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(crude_stocks,i)
  MediaInv <- rbind(datos,MediaInv)
}

crude_stocks2 <- left_join(MediaInv,crude_stocks, by = c("year", "week"))



# Valor del año anterior
previo <- data.frame(year = numeric(),
                       week = numeric(),
                      value_previo = numeric())

for (i in years) {
  print(i)
  if (i == 1982) {
    next
  }
  datos <- valor_previo(crude_stocks,i)
  previo <- rbind(datos,previo)
}

crude_stocks2 <- left_join(previo,crude_stocks2, by = c("year", "week"))
crude_stocks <- crude_stocks2

my3cols <- c("#E7B800", "#2E9FDF", "#FC4E07")

g_crude_stocks <- crude_stocks %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw() + ylim(300,600) + theme(axis.title.y = element_blank())+
  ggtitle("Inventario de crudo (mmb, sin SPR)") + theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(color = "#595959", hjust = 0, vjust = 1)) + scale_colour_manual("", 
                                                                                                                      breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                                      values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 

g_crude_stocks



# Cushing
# max, min y promedio

cushing <- cushing %>% mutate(year = isoyear(date),
                              week = isoweek(date))

cushings <- cushings %>% mutate(year = isoyear(date),
                              week = isoweek(date))


MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(cushing$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(cushing,i)
  MediaInv <- rbind(datos,MediaInv)
}

cushing2 <- left_join(MediaInv,cushing, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == 2004) {
    next
  }
  datos <- valor_previo(cushing,i)
  previo <- rbind(datos,previo)
}

cushing2 <- left_join(previo,cushing2, by = c("year", "week"))
cushing <- cushing2

g_cushing <- cushing %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw() + ylim(0,80) + theme(axis.title.y = element_blank())+
  ggtitle("Inventario en Cushing (mmb)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                                      breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                                      values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 

g_cushing



# Producción
production <- production %>% mutate(year = isoyear(date),
                              week = isoweek(date))

MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(production$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(production,i)
  MediaInv <- rbind(datos,MediaInv)
}

production2 <- left_join(MediaInv,production, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(production,i)
  previo <- rbind(datos,previo)
}

production2 <- left_join(previo,production2, by = c("year", "week"))
production <- production2

g_production <- production %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw() + ylim(6,14) + theme(axis.title.y = element_blank())+
  ggtitle("Producci?n de crudo (mmbd)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                              breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                              values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 

g_production


# Oil Imports
crude_imp <- crude_imp %>% mutate(year = isoyear(date),
                                    week = isoweek(date))

MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(crude_imp$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(crude_imp,i)
  MediaInv <- rbind(datos,MediaInv)
}

crude_imp2 <- left_join(MediaInv,crude_imp, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(crude_imp,i)
  previo <- rbind(datos,previo)
}

crude_imp2 <- left_join(previo,crude_imp2, by = c("year", "week"))
crude_imp <- crude_imp2

g_cude_imp <- crude_imp %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw() + ylim(4,10) + theme(axis.title.y = element_blank())+
  ggtitle("Importaci?n de crudo (mmbd)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                              breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                              values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 

g_cude_imp



# Oil Exports
crude_exp <- crude_exp %>% mutate(year = isoyear(date),
                                  week = isoweek(date))

MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(crude_exp$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(crude_exp,i)
  MediaInv <- rbind(datos,MediaInv)
}

crude_exp2 <- left_join(MediaInv,crude_exp, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(crude_exp,i)
  previo <- rbind(datos,previo)
}

crude_exp2 <- left_join(previo,crude_exp2, by = c("year", "week"))
crude_exp <- crude_exp2

g_crude_exp <- crude_exp %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw() + ylim(0,5) + theme(axis.title.y = element_blank())+
  ggtitle("Exportaci?n de crudo (mmbd)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                               breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                               values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 

g_crude_exp





# Corridas de refinación

crude_ref_int <- crude_ref_int %>% mutate(year = isoyear(date),
                                  week = isoweek(date))

MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(crude_ref_int$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(crude_ref_int,i)
  MediaInv <- rbind(datos,MediaInv)
}

crude_ref_int2 <- left_join(MediaInv,crude_ref_int, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(crude_ref_int,i)
  previo <- rbind(datos,previo)
}

crude_ref_int2 <- left_join(previo,crude_ref_int2, by = c("year", "week"))
crude_ref_int <- crude_ref_int2



g_ref_int <- crude_ref_int %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw() + ylim(9,19) + theme(axis.title.y = element_blank())+
  ggtitle("Corridas de refinaci?n (mmbd)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                               breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                               values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 

g_ref_int



# SPR

crude_spr <- crude_spr %>% mutate(year = isoyear(date),
                                          week = isoweek(date))
crude_spr <- crude_spr %>% rename(value = spr)

MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(crude_spr$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(crude_spr,i)
  MediaInv <- rbind(datos,MediaInv)
}

crude_spr2 <- left_join(MediaInv,crude_spr, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(crude_spr,i)
  previo <- rbind(datos,previo)
}

crude_spr2 <- left_join(previo,crude_spr2, by = c("year", "week"))
crude_spr <- crude_spr2



g_crude_spr <- crude_spr %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw() + ylim(600,700) + theme(axis.title.y = element_blank())+
  ggtitle("Reservas estrat?gicas (mmb)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                                 breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                                 values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 

g_crude_spr




# Net imports

crude_net_imp <- crude_net_imp %>% mutate(year = isoyear(date),
                                  week = isoweek(date))


MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(crude_net_imp$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(crude_net_imp,i)
  MediaInv <- rbind(datos,MediaInv)
}

crude_net_imp2 <- left_join(MediaInv,crude_net_imp, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(crude_net_imp,i)
  previo <- rbind(datos,previo)
}

crude_net_imp2 <- left_join(previo,crude_net_imp2, by = c("year", "week"))
crude_net_imp <- crude_net_imp2



g_crude_net_imp <- crude_net_imp %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw() + ylim(0,10) + theme(axis.title.y = element_blank())+
  ggtitle("Importaciones netas (mmbd)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                               breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                               values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 

g_crude_net_imp


figure <- ggarrange(g_crude_stocks, g_cushing, g_production, g_crude_exp, g_cude_imp, g_ref_int, g_crude_spr, g_crude_net_imp,
                    ncol = 3, nrow = 3,
                    common.legend = TRUE, legend = "bottom")

figure <- annotate_figure(figure,
                top = text_grob("Mercado de Crudo", color = "#4E01A8", face = "bold", size = 20),
                bottom = text_grob("Fuente: EIA. C?lculos PPY.", color = "#595959",
                                    size = 8, hjust = 1, x = 1))

figure

ggexport(figure, width = 1600, height = 1200, filename = "Crudos.png" )
ggexport(figure, width = 2000, height = 480, filename = "Crudos.pdf" )


# Productos ---------------------------------------------------------------

## Total stock de productos (sale de una resta)

crude_stocks <- eia_series("PET.WCRSTUS1.W")  # Incluye SPR
crude_stocks <- as.data.frame(crude_stocks$data)
crude_stocks <- crude_stocks %>% rename(crude = value)

oil_products_stocks <- eia_series("PET.WTTSTUS1.W") # Incluye SPR
oil_products_stocks <- as.data.frame(oil_products_stocks$data)
oil_products_stocks <- oil_products_stocks %>% rename(oil_productos = value)

productos <- inner_join(crude_stocks, oil_products_stocks, by = c("year", "month", "week", "date") )
productos <- productos %>% mutate(value = oil_productos - crude)
productos <- productos %>% select(year, month, week,date,value)
productos <- productos %>% mutate(year = isoyear(date),
                                  week = isoweek(date))


MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(productos$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(productos,i)
  MediaInv <- rbind(datos,MediaInv)
}

productos2 <- left_join(MediaInv,productos, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(productos,i)
  previo <- rbind(datos,previo)
}

productos2 <- left_join(previo,productos2, by = c("year", "week"))
productos <- productos2



g_productos <- productos %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw() + ylim(700,950) + theme(axis.title.y = element_blank())+
  ggtitle("Inventarios de productos (mmb)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                              breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                              values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 

g_productos



# Inventarios de gasolina

gasolina_stocks <- gasolina_stocks %>% mutate(year = isoyear(date),
                                          week = isoweek(date))


MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(gasolina_stocks$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(gasolina_stocks,i)
  MediaInv <- rbind(datos,MediaInv)
}

gasolina_stocks2 <- left_join(MediaInv,gasolina_stocks, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(gasolina_stocks,i)
  previo <- rbind(datos,previo)
}

gasolina_stocks2 <- left_join(previo,gasolina_stocks2, by = c("year", "week"))
gasolina_stocks <- gasolina_stocks2

save(gasolina_stocks, file = "C:/Users/fabian/Ecopetrol S.A/PPY - Documentos/Proyección de Precios/Precios Worklines/Corto Plazo/Modelos/Gasolinagasolina_inv.R")



g_gasolina_stocks <- gasolina_stocks %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw() + ylim(200,270) + theme(axis.title.y = element_blank())+
  ggtitle("Inventarios de gasolina (mmb)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                              breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                              values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 

g_gasolina_stocks



# Inventarios de destilados

destilados_stocks <- destilados_stocks %>% mutate(year = isoyear(date),
                                              week = isoweek(date))


MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(destilados_stocks$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(destilados_stocks,i)
  MediaInv <- rbind(datos,MediaInv)
}

destilados_stocks2 <- left_join(MediaInv,destilados_stocks, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(destilados_stocks,i)
  previo <- rbind(datos,previo)
}

destilados_stocks2 <- left_join(previo,destilados_stocks2, by = c("year", "week"))
destilados_stocks <- destilados_stocks2



g_destilados_stocks <- destilados_stocks %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw() + ylim(100,180) + theme(axis.title.y = element_blank())+
  ggtitle("Inventarios de destilados (mmb)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                                 breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                                 values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 

g_destilados_stocks



# Demana implícita de todos los productos

products_supplied_4 <- products_supplied_4 %>% mutate(year = isoyear(date),
                                                  week = isoweek(date))


MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(products_supplied_4$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(products_supplied_4,i)
  MediaInv <- rbind(datos,MediaInv)
}

products_supplied_42 <- left_join(MediaInv,products_supplied_4, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(products_supplied_4,i)
  previo <- rbind(datos,previo)
}

products_supplied_42 <- left_join(previo,products_supplied_42, by = c("year", "week"))
products_supplied_4 <- products_supplied_42



g_products_supplied_4 <- products_supplied_4 %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw() + ylim(14,24)  + theme(axis.title.y = element_blank())+
  ggtitle("Demanda de productos(4w avg, mmbd)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                                   breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                                   values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 
g_products_supplied_4




# Demana implícita de gasolina

gasoline_supplied_4 <- gasoline_supplied_4 %>% mutate(year = isoyear(date),
                                                      week = isoweek(date))


MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(gasoline_supplied_4$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(gasoline_supplied_4,i)
  MediaInv <- rbind(datos,MediaInv)
}

gasoline_supplied_42 <- left_join(MediaInv,gasoline_supplied_4, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(gasoline_supplied_4,i)
  previo <- rbind(datos,previo)
}

gasoline_supplied_42 <- left_join(previo,gasoline_supplied_42, by = c("year", "week"))
gasoline_supplied_4 <- gasoline_supplied_42



g_gasoline_supplied_4 <- gasoline_supplied_4 %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw() + ylim(5,11) + theme(axis.title.y = element_blank())+
  ggtitle("Demanda de gasolina (4w avg, mmbd)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                                     breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                                     values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 
g_gasoline_supplied_4




# Demana implícita de destilados

destillate_supplied_4 <- destillate_supplied_4 %>% mutate(year = isoyear(date),
                                                      week = isoweek(date))


MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(destillate_supplied_4$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(destillate_supplied_4,i)
  MediaInv <- rbind(datos,MediaInv)
}

destillate_supplied_42 <- left_join(MediaInv,destillate_supplied_4, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(destillate_supplied_4,i)
  previo <- rbind(datos,previo)
}

destillate_supplied_42 <- left_join(previo,destillate_supplied_42, by = c("year", "week"))
destillate_supplied_4 <- destillate_supplied_42



g_destillate_supplied_4 <- destillate_supplied_4 %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw() + ylim(3,4.6) + theme(axis.title.y = element_blank())+
  ggtitle("Demanda de destilados (4w avg, mmbd)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                                     breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                                     values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 
g_destillate_supplied_4


# Yields gasolina

gasolina_oferta <- gasolina_oferta %>% rename(gasolina = value)
kero_oferta <- kero_oferta %>% rename(kero = value)
desti_oferta <- desti_oferta %>% rename(dest = value)
residuos_oferta <- residuos_oferta %>% rename(resid = value)
propano_oferta <- propano_oferta %>% rename(propano = value)


yields <- Reduce(function(x, y) merge(x, y, by = c("date", "year", "month", "week")), list(gasolina_oferta, kero_oferta, desti_oferta, residuos_oferta, propano_oferta))
yields <- yields %>% mutate(total = gasolina+kero+dest+resid+propano)
yields <- yields %>% mutate(yield_gasolina = gasolina*100/total) 
yields <- yields %>% mutate(yield_destilados = dest*100/total) 
yields <- yields %>% mutate(yield_jet = kero*100/total)

yields_gasol <- yields %>% select(date,year,month,week,yield_gasolina)
yields_gasol <- yields_gasol %>% rename(value = yield_gasolina)
yields_dest <- yields %>% select(date,year,month,week,yield_destilados)
yields_dest <- yields_dest %>% rename(value = yield_destilados)
yields_jet <- yields %>% select(date,year,month,week,yield_jet)
yields_jet <- yields_jet %>% rename(value = yield_jet)



yields_gasol <- yields_gasol %>% mutate(year = isoyear(date),
                                                         week = isoweek(date))


MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(yields_gasol$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(yields_gasol,i)
  MediaInv <- rbind(datos,MediaInv)
}

yields_gasol2 <- left_join(MediaInv,yields_gasol, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(yields_gasol,i)
  previo <- rbind(datos,previo)
}

yields_gasol2 <- left_join(previo, yields_gasol2, by = c("year", "week"))
yields_gasol <- yields_gasol2



g_yields_gasol <- yields_gasol %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1, ymax = max/1), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1,  colour = "2020"), size = 1.3) + theme_bw() + ylim(40,60) + theme(axis.title.y = element_blank())+
  ggtitle("Yield de gasolina (%)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                                       breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                                       values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 
g_yields_gasol


# Yields destilados

yields_dest <- yields_dest %>% mutate(year = isoyear(date),
                                        week = isoweek(date))


MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(yields_dest$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(yields_dest,i)
  MediaInv <- rbind(datos,MediaInv)
}

yields_dest2 <- left_join(MediaInv,yields_dest, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(yields_dest,i)
  previo <- rbind(datos,previo)
}

yields_dest2 <- left_join(previo, yields_dest2, by = c("year", "week"))
yields_dest <- yields_dest2



g_yields_dest <- yields_dest %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1, ymax = max/1), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1,  colour = "2020"), size = 1.3) + theme_bw() + ylim(20,40) + theme(axis.title.y = element_blank())+
  ggtitle("Yield de destilados (%)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                        breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                        values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 
g_yields_dest



# Exportación de gasolina

gasoline_exports <- gasoline_exports %>% mutate(year = isoyear(date),
                                      week = isoweek(date))


MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(gasoline_exports$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(gasoline_exports,i)
  MediaInv <- rbind(datos,MediaInv)
}

gasoline_exports2 <- left_join(MediaInv,gasoline_exports, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(gasoline_exports,i)
  previo <- rbind(datos,previo)
}

gasoline_exports2 <- left_join(previo, gasoline_exports2, by = c("year", "week"))
gasoline_exports <- gasoline_exports2



g_gasoline_exports <- gasoline_exports %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1, ymax = max/1), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1,  colour = "2020"), size = 1.3) + theme_bw() + ylim(0,1400) + theme(axis.title.y = element_blank())+
  ggtitle("Exportaciones de gasolina (kbd)" ) + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                          breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                          values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 
g_gasoline_exports







figure2 <- ggarrange(g_productos, g_gasolina_stocks, g_destilados_stocks, g_products_supplied_4, g_gasoline_supplied_4, g_destillate_supplied_4, g_yields_gasol, g_yields_dest, g_gasoline_exports,
                    ncol = 3, nrow = 3,
                    common.legend = TRUE, legend = "bottom")
figure2 <- annotate_figure(figure2,
                          top = text_grob("Mercado de Productos", color = "#4E01A8", face = "bold", size = 20),
                          bottom = text_grob("Fuente: EIA. C?lculos PPY.", color = "#595959",
                                             size = 8, hjust = 1, x = 1))

figure2

ggexport(figure2, width = 1300, height = 1000, filename = "Productos.png" )





# Inventarios de Residuos

resid_stocks <- resid_stocks %>% mutate(year = isoyear(date),
                                              week = isoweek(date))


MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(resid_stocks$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(resid_stocks,i)
  MediaInv <- rbind(datos,MediaInv)
}

resid_stocks2 <- left_join(MediaInv,resid_stocks, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(resid_stocks,i)
  previo <- rbind(datos,previo)
}

resid_stocks2 <- left_join(previo,resid_stocks2, by = c("year", "week"))
resid_stocks <- resid_stocks2


g_resid_stocks <- resid_stocks %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw()  + theme(axis.title.y = element_blank())+
  ggtitle("Inventarios de residuos (mmb)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                                 breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                                 values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 

g_resid_stocks


# Inventarios de Jet

jet_stocks <- jet_stocks %>% mutate(year = isoyear(date),
                                        week = isoweek(date))


MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(jet_stocks$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(jet_stocks,i)
  MediaInv <- rbind(datos,MediaInv)
}

jet_stocks2 <- left_join(MediaInv,jet_stocks, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(jet_stocks,i)
  previo <- rbind(datos,previo)
}

jet_stocks2 <- left_join(previo,jet_stocks2, by = c("year", "week"))
jet_stocks <- jet_stocks2


g_jet_stocks <- jet_stocks %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw()  + theme(axis.title.y = element_blank())+
  ggtitle("Inventarios de jet (mmb)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                                 breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                                 values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 

g_jet_stocks




# Inventarios de Propano

prop_stocks <- prop_stocks %>% mutate(year = isoyear(date),
                                    week = isoweek(date))


MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(prop_stocks$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(prop_stocks,i)
  MediaInv <- rbind(datos,MediaInv)
}

prop_stocks2 <- left_join(MediaInv,prop_stocks, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(prop_stocks,i)
  previo <- rbind(datos,previo)
}

prop_stocks2 <- left_join(previo,prop_stocks2, by = c("year", "week"))
prop_stocks <- prop_stocks2


g_prop_stocks <- prop_stocks %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw()  + theme(axis.title.y = element_blank())+
  ggtitle("Inventarios de propano/propileno (mmb)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                            breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                            values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 

g_prop_stocks



### Demanda de residuos

red_supplied_4 <- red_supplied_4 %>% mutate(year = isoyear(date),
                                                          week = isoweek(date))


MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(red_supplied_4$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(red_supplied_4,i)
  MediaInv <- rbind(datos,MediaInv)
}

red_supplied_42 <- left_join(MediaInv,red_supplied_4, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(red_supplied_4,i)
  previo <- rbind(datos,previo)
}

red_supplied_42 <- left_join(previo,red_supplied_42, by = c("year", "week"))
red_supplied_4 <- red_supplied_42



g_red_supplied_4 <- red_supplied_4 %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw()  + theme(axis.title.y = element_blank())+
  ggtitle("Demanda de residuos (4w avg, mmbd)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                                        breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                                        values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 
g_red_supplied_4





### Demanda de jet

jet_supplied_4 <- jet_supplied_4 %>% mutate(year = isoyear(date),
                                            week = isoweek(date))


MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(jet_supplied_4$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(jet_supplied_4,i)
  MediaInv <- rbind(datos,MediaInv)
}

jet_supplied_42 <- left_join(MediaInv,jet_supplied_4, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(jet_supplied_4,i)
  previo <- rbind(datos,previo)
}

jet_supplied_42 <- left_join(previo,jet_supplied_42, by = c("year", "week"))
jet_supplied_4 <- jet_supplied_42



g_jet_supplied_4 <- jet_supplied_4 %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw()  + theme(axis.title.y = element_blank())+
  ggtitle("Demanda de jet (4w avg, mmbd)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                                      breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                                      values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 
g_jet_supplied_4





### Demanda de propano

prop_supplied_4 <- prop_supplied_4 %>% mutate(year = isoyear(date),
                                            week = isoweek(date))


MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(prop_supplied_4$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(prop_supplied_4,i)
  MediaInv <- rbind(datos,MediaInv)
}

prop_supplied_42 <- left_join(MediaInv,prop_supplied_4, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(prop_supplied_4,i)
  previo <- rbind(datos,previo)
}

prop_supplied_42 <- left_join(previo,prop_supplied_42, by = c("year", "week"))
prop_supplied_4 <- prop_supplied_42



g_prop_supplied_4_4 <- prop_supplied_4 %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000,  colour = "2020"), size = 1.3) + theme_bw()  + theme(axis.title.y = element_blank())+
  ggtitle("Demanda de propano/etileno (4w avg, mmbd)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                                 breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                                 values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 
g_prop_supplied_4_4


# Yield Destilados


yields_jet <- yields_jet %>% mutate(year = isoyear(date),
                                        week = isoweek(date))


MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(yields_jet$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(yields_jet,i)
  MediaInv <- rbind(datos,MediaInv)
}

yields_jet2 <- left_join(MediaInv,yields_jet, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(yields_jet,i)
  previo <- rbind(datos,previo)
}

yields_jet2 <- left_join(previo, yields_jet2, by = c("year", "week"))
yields_jet <- yields_jet2



g_yields_jet <- yields_jet %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1, ymax = max/1), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1,  colour = "2020"), size = 1.3) + theme_bw()  + theme(axis.title.y = element_blank())+
  ggtitle("Yield de jet (%)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                         breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                         values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 
g_yields_jet




# Inventario de asfalto

asf_stocks <- asf_stocks %>% mutate(year = isoyear(date),
                                    week = isoweek(date))


MediaInv <- data.frame(year = numeric(),
                       week = numeric(),
                       promedio = numeric(),
                       max = numeric(),
                       min = numeric())


years <- names(table(asf_stocks$year))
years <- as.numeric(years)

for (i in years) {
  print(i)
  #i = 2000
  datos <- conversion(asf_stocks,i)
  MediaInv <- rbind(datos,MediaInv)
}

asf_stocks2 <- left_join(MediaInv,asf_stocks, by = c("year", "week"))

previo <- data.frame(year = numeric(),
                     week = numeric(),
                     value_previo = numeric())

for (i in years) {
  print(i)
  if (i == years[1]) {
    next
  }
  datos <- valor_previo(asf_stocks,i)
  previo <- rbind(datos,previo)
}

asf_stocks2 <- left_join(previo, asf_stocks2, by = c("year", "week"))
asf_stocks <- asf_stocks2



g_asf_stocks <- asf_stocks %>% filter(year==2021) %>%  ggplot(aes(x=week)) + geom_line(aes(y = value/1000, color = "2021"), size = 1.3)  + geom_ribbon(aes(ymin = min/1000, ymax = max/1000), fill="blue", alpha=0.15) +
  geom_line(aes( y = promedio/1000, color = "Promedio '16-'20") , linetype = "dotted", size = 1.3 ) + geom_line(aes(y = value_previo/1000, colour = "2020"), size = 1.3) + theme_bw()  + theme(axis.title.y = element_blank())+
  ggtitle("Inventarios de asfalto (mmb)") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual("", 
                                                                                                    breaks = c("2020", "Promedio '16-'20", "2021"),
                                                                                                    values = c("#49DB31", "#7B7B7B", "#F7032C")) + theme(legend.position="bottom") 
g_asf_stocks




figure3 <- ggarrange(g_resid_stocks, g_jet_stocks, g_prop_stocks, g_red_supplied_4, g_jet_supplied_4, g_prop_supplied_4_4, g_yields_jet, g_asf_stocks,
                     ncol = 3, nrow = 3,
                     common.legend = TRUE, legend = "bottom")

figure3 <- annotate_figure(figure3,
                           top = text_grob("Mercado de Productos (cont.)", color = "#4E01A8", face = "bold", size = 20),
                           bottom = text_grob("Fuente: EIA. C?lculos PPY.", color = "#595959",
                                              size = 8, hjust = 1, x = 1))

figure3

ggexport(figure3, width = 1300, height = 1000, filename = "Productos2.png" )


