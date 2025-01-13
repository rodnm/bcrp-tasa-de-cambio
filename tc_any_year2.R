# Extractor de datos de Series Estadísticas de BCRP ----

#######################################
# Preparando el entorno ----
#######################################
rm(list = ls())            # Limpia los objetos guardados en memoria
graphics.off()             # Limpia los gráficos de la memoria

#######################################
# Cargar las librerías ----
#######################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, rvest)

# Establece el directorio de trabajo según la ubicación del script
script.path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script.path)

#######################################
# Importar los datos ----
#######################################
inicio <- "2023-01-01"
final <- "2024-12-31"
url.bcrp <- paste0(
  "https://estadisticas.bcrp.gob.pe/estadisticas/series/diarias/resultados/PD04640PD/html/",
  inicio, "/", final, "/"
)

tmp <- read_html(url.bcrp) %>% html_nodes("table")
tc_bcrp <- html_table(tmp[[2]], fill = TRUE)

# Renombrar columnas
df <- tc_bcrp %>% rename(fecha = 1, tc_SBS_venta = 2)
df$tc_SBS_venta <- as.numeric(df$tc_SBS_venta)

########################################
# Manipulando los datos para darles formato ----
########################################
# Extraer día, mes y año directamente
df <- df %>% 
  mutate(
    dia = str_sub(fecha, 1, 2),
    mes = str_sub(fecha, 3, 5),
    year = str_sub(fecha, 6, 7) %>% as.integer() %>% 
    {if_else(. < 80, . + 2000, . + 1900)}
  )

# Convertir meses de texto a número
meses <- c("Ene" = "01", "Feb" = "02", "Mar" = "03", "Abr" = "04", "May" = "05",
           "Jun" = "06", "Jul" = "07", "Ago" = "08", "Set" = "09", "Oct" = "10",
           "Nov" = "11", "Dic" = "12")

df$mes <- meses[df$mes]

# Crear columna fecha en formato Date
df <- df %>% mutate(fecha2 = as.Date(paste(year, mes, dia, sep = "-")))

# Guardar el dataframe en formato CSV
write.csv(df, "tc_bancario_SBS_venta.csv", row.names = FALSE)

# Extraer últimos 7 días
# df1 <- tail(df, 7)
df1 <- df

#######################################
# Gráfico de la evolución del TC ----
#######################################
evol_tc <- ggplot(df1, aes(x = fecha2, y = tc_SBS_venta, group = 1)) +
  geom_line(color = "firebrick", na.rm = TRUE) +
  # geom_point() +
  # geom_text(aes(label = tc_SBS_venta),
  #           position = position_dodge(width = 0.9), size = 3.2,
  #           hjust = 0.5, vjust = -0.5, fontface = "bold") +
  scale_y_continuous(limits = c(min(df$tc_SBS_venta, na.rm = TRUE),
                                max(df$tc_SBS_venta, na.rm = TRUE) + 0.025)) +
  labs(
    title = "Evolución del tipo de cambio bancario del dólar - Venta",
    x = "Fecha",
    y = "Tipo de cambio bancario SBS - Venta",
    caption = "Fuente: elaboración propia en base a series diarias de BCRP."
  ) +
  theme_economist() +
  theme(
    axis.title.x = element_text(margin = margin(t = 5)),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
evol_tc

ggsave("tc-plot.png", 
       plot = evol_tc,
       width = 22, 
       height = 15, 
       dpi = 300, 
       units = "cm", 
       device = 'png')

#######################################
# Gráfico alternativo ----
#######################################
# Excluir el valor inicial y final
df_filtered <- df1[-c(1, nrow(df1)), ]

# Obtener las fechas de los valores máximo y mínimo
fecha_max <- df_filtered$fecha2[which.max(df_filtered$valor)]
fecha_min <- df_filtered$fecha2[which.min(df_filtered$valor)]

# Combinar las fechas y convertirlas a numéricas
fechas_extremos <- as.numeric(c(fecha_max, fecha_min))

# Verificar los valores obtenidos
print(fechas_extremos)

# Crear el gráfico con líneas verticales en las fechas de máximos y mínimos
evol_tc_2 <- evol_tc + 
  geom_vline(xintercept = fechas_extremos, colour = "blue", linetype = "longdash")

evol_tc_2 # OBS: no muestra las líneas azules


ggsave("tc-plot-2023-fechas.png", 
       plot = evol_tc_2,
       width = 18, 
       height = 15, 
       dpi = 300, 
       units = "cm", 
       device = 'png')
