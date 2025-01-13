# Extractor de datos de Series Estadísticas de BCRP ----
# Rodrigo Norabuena


#######################################
# Preparando el entorno ----
#######################################
rm(list=ls()) # limpia los objetos guardados en memoria
# dev.off() # apaga el visor de graficos. Solo funciona en una sesion interactiva
graphics.off() # limpia los graficos de la memoria
# cat("\014") # es el codigo que envia el comando ctrl+L para limpiar la consola en RStudio
# cat("\f") # tambien limpia la consola y es mas sencillo de recordar


#######################################
# cargar las librerias ----
#######################################
# Cargar los paquetes necesarios o instalarlos si no se encuentran en la PC 
# usando el paquete pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, ggthemes, rvest)
# rvest se usa para realizar web scraping de los datos del BCRP

# library(tidyverse); library(dplyr); library(ggthemes); library(rvest)

script.path <- dirname(rstudioapi::getSourceEditorContext()$path) # path del directorio que contiene este archivo R
setwd(script.path)

#######################################
# Importar los datos -----
#######################################
# Web scraping para obtener los datos actualizados hasta el ultimo dia registrado en el mes
inicio <- "2023-01-01"
final <- "2024-12-31"
url.bcrp <- paste0("https://estadisticas.bcrp.gob.pe/estadisticas/series/diarias/resultados/PD04640PD/html/", inicio, "/", final, "/")
# url.bcrp <- "https://estadisticas.bcrp.gob.pe/estadisticas/series/diarias/resultados/PD04640PD/html/2021-04-05/2023-12-31/"
tmp <- read_html(url.bcrp) # lee la ruta html del url
tmp <- html_nodes(tmp, "table") # busca los nodos html que tienen la etiqueta 'table'
length(tmp) # cantidad de elementos en la lista tmp
sapply(tmp, class) # vemos la clase de cada uno de esos elementos en la lista para facilitar la revisi?n
sapply(tmp, function(x) dim(html_table(x, fill = TRUE))) # vemos las dimensiones de las tablas que detect? tmp. Cada columna representa un objeto 'table'. La fila 1 y fila 2 son el n?mero de filas y columnas de la tabla [,x], respectivamente
tc_bcrp <- html_table(tmp[[2]]) # se crea el dataframe con los datos de nuestro inter?s. En general, el indice 2 parece ser el que siempre queremos en las series diarias del BCRP seg?n mis obs previas.
df <- tc_bcrp # renombre del dataframe
headers <- c("fecha", "tc_SBS_venta") # se crea un vector que contiene los encabezados
colnames(df)= headers # se asigna los encabezados al dataframe
df$tc_SBS_venta <- as.numeric(df$tc_SBS_venta) # se cambia la clase de dato a double
# OBS que pueden haberse introducido NAs los d?as que no hubo registro por feriados o alguna otra causa.


########################################
# manipulando los datos para darles formato ----
########################################
id <- as.integer(rownames(df)) # se crea un vector que contiene el numero de cada fila de df
df <- cbind(id=id, df) # se combina el vector id con el df asignando id como "id"

output <- vector("integer", nrow(df)) # se crea un vector vacio de clase integer con el numero de filas de df
output2 <- vector("integer", nrow(df))
output3 <- vector("integer", nrow(df))

for (i in 1:nrow(df)) {
  output[[i]] <- str_sub(df$fecha[[i]], 1, 2) # se extrae los digitos del dia de fecha
  output2[[i]] <- str_sub(df$fecha[[i]], 3, 5) # se extrae los caracteres del mes de fecha
  output3[[i]] <- str_sub(df$fecha[[i]], 6, 7) # se extrae los caracteres del a?o de fecha
}

df$dia <- output # se crea una nueva variable dentro de df
df$mes <- output2 # se crea una nueva variable dentro de df
df$year <- output3

mes2 <- output2 #%>% 
# map_if(., .[i]=="Jan", 1, .else=0)
year2 <- output3

# cambiamos el formato de los datos de a?os. En lugar de '20' tendremos '2020'
for (i in 1:length(year2)) {
  ifelse(year2[i]<97, (year2[i] = as.integer(year2[i]) + 2000), 
         (year2[i] = as.integer(year2[i]) + 1900))
}

# reemplazamos los nombres de los meses a caracteres numericos

# for (i in 1:length(mes2)) {
#   ifelse(mes2[i]=="Ene", (mes2[i]="01"),
#          ifelse(mes2[i]=="Feb", (mes2[i]="02"),
#                 ifelse(mes2[i]=="Mar", (mes2[i]="03"),
#                        ifelse(mes2[i]=="Abr", (mes2[i]="04"),
#                               ifelse(mes2[i]=="May", (mes2[i]="05"),
#                                      ifelse(mes2[i]=="Jun", (mes2[i]="06"),
#                                             ifelse(mes2[i]=="Jul", (mes2[i]="07"),
#                                                    ifelse(mes2[i]=="Ago", (mes2[i]="08"),
#                                                           ifelse(mes2[i]=="Set", (mes2[i]="09"),
#                                                                  ifelse(mes2[i]=="Oct", (mes2[i]="10"),
#                                                                         ifelse(mes2[i]=="Nov", (mes2[i]="11"), (mes2[i]="12"))))))))))))
# }

for (i in 1:length(mes2)) {
  if(mes2[i]=="Ene") {mes2[i]="01"}
  if(mes2[i]=="Feb") {mes2[i]="02"}
  if(mes2[i]=="Mar") {mes2[i]="03"}
  if(mes2[i]=="Abr") {mes2[i]="04"}
  if(mes2[i]=="May") {mes2[i]="05"}
  if(mes2[i]=="Jun") {mes2[i]="06"}
  if(mes2[i]=="Jul") {mes2[i]="07"}
  if(mes2[i]=="Ago") {mes2[i]="08"}
  if(mes2[i]=="Set") {mes2[i]="09"}
  if(mes2[i]=="Oct") {mes2[i]="10"}
  if(mes2[i]=="Nov") {mes2[i]="11"}
  if(mes2[i]=="Dec") {mes2[i]="12"}
}

# mes2 <- as.integer(mes2)

df$mes <- mes2 # reemplazamos el valor de mes en df
df$year <- year2 # reemplazamos el valor de a?o en df

p <- paste(mes2, df$dia, sep="-") # combinamos mes y dia como 'mes-dia'. Ejemplo: '01-30'
# week_date <- vector("character", nrow(db_excess_shp))
format_date <- paste(year2, p, sep="-") # combinamos a?o, mes y dia como 'a?o-mes-dia'. Ejemplo: '2020-01-30'
df$fecha2 <- as.Date(format_date) # a?adimos el nuevo formato fecha a df

# Guardamos el dataframe en formato csv
write.csv(x=df, file="tc_bancario_SBS_venta.csv")

siete <- nrow(df)-7 # num final - 7 posiciones atrás
df1 <- df[siete:nrow(df),] # de 7 posiciones atrás hacia el final

#######################################
# Grafico de la evolucion de TC ----
#######################################
evol_tc <- ggplot(df1, aes(x=fecha2, y=tc_SBS_venta, group = 1))+
  geom_line(color="firebrick", na.rm = TRUE)+
  geom_point()+
  geom_text(aes(label=tc_SBS_venta),
            position = position_dodge(width=0.9), size=3.2,
            hjust=0.5, vjust=-0.5, fontface = "bold")+
  
  # scale_x_date(date_labels="%b %d", date_breaks  = "1 day")+
  
  scale_y_continuous(limits=c(min(df$tc_SBS_venta),
                              max(df$tc_SBS_venta)+0.025))+
  
  labs(
    title = "Evolución del tipo de cambio bancario del dólar - Venta",
    # subtitle = "",
    x = "Fecha",
    y = "Tipo de cambio bancario SBS - Venta",
    caption="Fuente: elaboración propia en base a series diarias de BCRP."
  )+
  
  # # theme_bw()
  theme_economist() +
  scale_colour_economist()+
  theme(axis.title.x = element_text(margin = margin(t = 5)),
        axis.title.y = element_text(margin = margin(r = 5)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
evol_tc


#######################################
# Guardar el grafico ----
#######################################
ggsave("tc-plot.png", 
       plot = evol_tc,
       width = 22, 
       height = 15, 
       dpi = 300, 
       units = "cm", 
       device='png'
)


#######################################
# Grafico alternativo ----
#######################################
evol_tc_2 <- evol_tc + 
  geom_vline(xintercept = df$dia[6], colour="blue", linetype = "longdash")+
  geom_vline(xintercept = df$dia[11], colour="blue", linetype = "longdash")+
  geom_vline(xintercept = df$dia[17], colour="blue", linetype = "longdash")
evol_tc_2

ggsave("tc-plot-2023-fechas.png", 
       plot = evol_tc_2,
       width = 18, 
       height = 15, 
       dpi = 300, 
       units = "cm", 
       device='png'
)

########################################

# url.bcrp <- "https://estadisticas.bcrp.gob.pe/estadisticas/series/diarias/resultados/PD04640PD/html/1950-01-01/2021-04-30/"
# tmp <- read_html(url.bcrp)
# tmp <- html_nodes(tmp, "table")
# length(tmp)
# sapply(tmp, class)
# sapply(tmp, function(x) dim(html_table(x, fill = TRUE)))
# tc_bcrp <- html_table(tmp[[2]]) # se crea el dataframe
# df2 <- tc_bcrp # renombre del dataframe
# headers <- c("fecha", "tc_SBS_venta") # se crea un vector que contiene los encabezados
# colnames(df2)= headers # se asigna los encabezados al dataframe
# # df$tc_SBS_venta <- as.double(df$tc_SBS_venta) # se cambia la clase de dato a double
# 
# id2 <- as.integer(rownames(df2)) # se crea un vector que contiene el numero de cada fila de df
# df2 <- cbind(id=id2, df2) # se combina el vector id con el df asignando id como "id"
# df2$tc_SBS_venta <- as.numeric(df2$tc_SBS_venta)
# 
# max(df2$tc_SBS_venta)
# which.max(df2$tc_SBS_venta)

# 
# model4_fmae <- "Full model"
# model2_fmae <- "Climate model"
# model1_fmae <- "Deforestation model"
# model3_fmae <- "Malaria-control model"
# 
# 
# model_fmae <- c(model1_fmae, model2_fmae, model3_fmae, model4_fmae)
# mae_falci <- c(13.312, 12.601, 12.516, 12.516)
# #mae_falci <- c(12.51644, 12.60109, 12.39452, 13.3117)
# 
# df_fmae <- data.frame(model_fmae, mae_falci)
# colnames(df_fmae) <- c('Model', "Falciparum")
# 
# library(kableExtra)
# #library(formattable)
# # kbl(df_pv, booktabs = T) %>%
# # kable_classic(full_width = F, html_font = "Cambria")
# #knitr::kable(df_pv, align = "lccccccc")
# #knitr::kable(df_pv, align = "lccccccc") #%>% scroll_box(width = "100%")
# 
# kbl(df_fmae, col.names = c('Model', "MAE Falciparum"), escape = F, align = "lc") %>%
#   kable_classic(full_width = F, html_font = "Cambria") %>%
#   #kable_paper("hover", full_width = F) %>%
#   column_spec(1, bold = T) #%>%
# #column_spec(, bold = T) %>%
# #column_spec(5, width = "3cm") %>%
# #add_header_above(c(" ", "MAE" = 2))


########################################
# 
# df2 <- df
# df2$id <- NULL
# df2$dia <- NULL
# df2$mes <- NULL
# 
# df2 <- df2 %>% 
#   mutate(varDiaSoles = tc_SBS_venta - lag(tc_SBS_venta),
#          varDiax100 = (tc_SBS_venta/lag(tc_SBS_venta)-1)*100,
#          var7Soles = tc_SBS_venta - lag(tc_SBS_venta, n=7L),
#          var7x100 = ((tc_SBS_venta/lag(tc_SBS_venta, n=7L)-1)))
# 
# df2$varDiaSoles[1] <- -0.037
# df2$varDiax100[1] <- -0.985
# df2$var7Soles[1] <- -0.029
# df2$var7x100[1] <- -0.773
# df2 <- df2 %>% mutate(across(is.numeric, ~ round(., 3)))
# 
# df2 <- rbind(df2[1,] , df2[13:nrow(df2),])
# 
# 
# kbl(df2, escape = F,
#     caption = "Evolución reciente del tipo de cambio bancario venta") %>%
#   kable_classic(full_width = F, html_font = "Cambria") %>%
#   #kable_paper("hover", full_width = F) %>%
#   column_spec(1, bold = F)









