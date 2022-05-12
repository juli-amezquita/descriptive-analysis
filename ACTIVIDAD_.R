# Instalación de paquetes
install.packages('dplyr')
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")
install.packages("ggplot2")
install.packages("ggplotgui")

# Carga paquete
library(Hmisc)
library(plyr)
library(dplyr)
library(tidyverse)
library(funModeling)
library(tidyr)

# PARTE 1 LIMPIEZA DE DATOS

# Carga de datos
data= read.csv("/Volumes/GoogleDrive-103227629492400701617/Mi unidad/2_Master UNIR/1_Master/2_Análisis e Interpretación de datos/Actividad1_11 enero/datos_31 dic/owid-covid-data (2).csv",sep=",")

# Tamaño del dataset
ncol(data)
nrow(data)

# Nombre columnas
colnames(data)

# Filtro para obtener los países que me interesan
flag_paises = data$location == "Argentina" | data$location == "Bolivia" | data$location == "Brazil" | data$location == "Chile" | data$location == "Colombia" |  data$location == "Ecuador" | data$location == "Paraguay" | data$location == "Peru" | data$location == "Uruguay"
flag_paises

# creo la variable con los países que me interesan
data2 = data[flag_paises,]

# Tamaño nuevo dataset
ncol(data2)
nrow(data2)


# Filtro las columnas

datos3 = data2[,c("location", "date", "population", "new_cases", "new_cases_per_million", "new_deaths","new_deaths_per_million", "new_tests","people_fully_vaccinated_per_hundred")]

# Tamaño nuevo dataset
ncol(datos3)
nrow(datos3)

# Nombre columnas
colnames(datos3)

# filtro por año, me interesan los datos desde Enero 1, 2021 hasta Diciembre 30, 2021

flag_fecha = datos3$date >= "2021-01-01"

# creo la variable con las fechas que me interesan
datos_final = datos3[flag_fecha,]

# Tamaño nuevo dataset
ncol(datos_final)
nrow(datos_final)

# Muestra los primeros datos de nuestro nuevo dataframe
head(datos_final)

# Muestra los últimos datos de nuestro nuevo dataframe
tail(datos_final)

# Visualizar valores perdidos y ceros
df_status(datos_final)


# se crea una función para imputar los valores de la variable new_tests por el promedio de los últimos 7 días al valor NA
impute_na_with_mean <- function(x, width, reverse=FALSE) {
  
  if (reverse) {x <- rev(x)}
  start <- head(which(!is.na(x)),1)
  for (i in (start+width):length(x)) {
    x[i] <- ifelse(is.na(x[i]), 
                   mean(x[(i-width):(i-1)], rm.na=TRUE), 
                   x[i])
  }
  if (reverse) {x <- rev(x)}
  x
}
datos_final$new_tests <- impute_na_with_mean(datos_final$new_tests, 7)

# Reemplazar valores NA con 0 en las siguientes columnas, ya que se asumen que no son valores perdidos si no que son ceros
vars_to_replace=c("new_cases", "new_cases_per_million", "new_deaths", "new_deaths_per_million", "people_fully_vaccinated_per_hundred")

datos_final=datos_final %>% mutate_at(.vars=vars_to_replace, .funs = funs(ifelse(is.na(.), 0, .)))


# Vemos que ya no hay valores perdidos
df_status(datos_final)

# ver un resumen de cada variable
describe(datos_final)


# Eliminar variables con valores negativo, pues no son consistentes ya que no se tienen cantidad de muertes negativas
# Se eliminan estas filas porque solo son 3
datos_final <- datos_final[!(datos_final$new_deaths < 0),]

# Convertir la columna date en formato fecha

datos_final$date <- as.Date(datos_final$date, format = "%Y-%m-%d")
str(datos_final)

# ver un resumen de cada variable, para revisar que se corrigieron los valores negativos
describe(datos_final$new_deaths)
describe(datos_final$new_deaths_per_million)

####################################

# PARTE 2: ANÁLISIS DESCRIPTIVO
# Sacamos un dataframe por país

peru <- datos_final[datos_final$location == "Peru", ]
argentina <- datos_final[datos_final$location == "Argentina", ]
bolivia <- datos_final[datos_final$location == "Bolivia", ]
brasil <- datos_final[datos_final$location == "Brazil", ]
chile <- datos_final[datos_final$location == "Chile", ]
colombia <- datos_final[datos_final$location == "Colombia", ]
ecuador <- datos_final[datos_final$location == "Ecuador", ]
paraguay <- datos_final[datos_final$location == "Paraguay", ]
uruguay <- datos_final[datos_final$location == "Uruguay", ]

# Luego podemos extraer el máximo valor que corresponde a la población con el esquema de vac completo ajustadas a la población usando la función max()
chile_vacccine <- max(chile$people_fully_vaccinated_per_hundred)
colombia_vaccine <- max(colombia$people_fully_vaccinated_per_hundred)

# Podemos guardar estos valores como un vector alturas y luego crear el gráfico de barras.
alturas <- c(chile_vacccine, colombia_vaccine)

# se crea el gráfico de barras
barplot(
  alturas,
  main = "Porcentaje de la población vacunada \n con esquema completo",
  col = c("lightseagreen", "indianred"),
  ylim = c(0,100),
  names.arg = c("Chile", "Colombia"),
  cex.names = 1.2
)

# Comparamos las muertes nuevas de cada día entre Colombia y Chile
# obtenemos los eje ‘x’,

x_colombia <- colombia$date
x_chile <- chile$date

# Obtenemos los eje 'y'
y_colombia <- colombia$new_deaths_per_million
y_chile <- chile$new_deaths_per_million

# graficamos
plot(
  x_colombia, y_colombia,
  type = "l",
  col = "indianred",
  lwd = 2,
  xlab = "",
  ylab = "",
  las = 1,
  xlim = c(as.Date("2021-01-01"), as.Date("2021-12-30"))
)
lines(
  x_chile, y_chile,
  col="lightseagreen",
  lwd = 2
  
)
grid()
title(
  main = "Muertes diarias por millón de habitantes \n durante el 2021",
  line = 0.8
)
legend(
  x = "topright",
  legend = c("Colombia","Chile"),
  col = c("indianred","lightseagreen"),
  lty = 1,
  bty = "n",
  lwd = 2
)

# Comparamos los casos nuevos de cada día entre Colombia y Chile
# obtenemos los eje ‘x’,

x_colombia <- colombia$date
x_chile <- chile$date

# obtenemos los eje 'y'
y_colombia_cn <- colombia$new_cases_per_million
y_chile_cn <- chile$new_cases_per_million

# graficamos
plot(
  x_colombia, y_colombia_cn,
  type = "l",
  col = "indianred",
  lwd = 2,
  xlab = "",
  ylab = "",
  las = 1,
  xlim = c(as.Date("2021-01-01"), as.Date("2021-12-30"))
)
lines(
  x_chile, y_chile_cn,
  col="lightseagreen",
  lwd = 2
  
)
grid()
title(
  main = "Casos diarios por millón de habitantes \n durante el 2021",
  line = 0.8
)
legend(
  x = "topright",
  legend = c("Colombia","Chile"),
  col = c("indianred","lightseagreen"),
  lty = 1,
  bty = "n",
  lwd = 2
)



# Libraries
library(ggplot2)
library(dplyr)
library(patchwork) # Para mostrar dos graficos juntos
library(hrbrthemes)

# Grafico Colombia: Vacunados vs nuevos casos
# Valor para transformar la escala
coeff <- 10

# Colores
vacc_color <- "#61D04F"
cases_color <- rgb(0.2, 0.6, 0.9, 1)

# graficamos
ggplot(tail(colombia,291), aes(x=date)) +
  
  geom_bar( aes(y=people_fully_vaccinated_per_hundred), stat="identity",  size=.10, color="black", fill=vacc_color, alpha=.4) + 
  geom_line( aes(y=new_cases_per_million/coeff), size=.8, color=cases_color) +
  
  scale_y_continuous(
    
    # Primer eje, se especifica el nombre
    name = "% Vacunación",
    
    # agrega un segundo eje y se específica el nombre 
    sec.axis = sec_axis(~.*coeff, name="Nuevos casos por millón de habitantes")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = vacc_color, size=14),
    axis.title.y.right = element_text(color = cases_color, size=14)
  ) +
  
  ggtitle("Colombia: vacunación vs nuevos casos")

# Grafico Colombia: Vacunados vs nuevos casos totales diarios
# Valor para transformar la escala
coeff <- 550

# colores
vacc_color <- "#61D04F"
cases_color <- rgb(0.2, 0.6, 0.9, 1)

# graficamos
ggplot(tail(colombia,291), aes(x=date)) +
  
  geom_bar( aes(y=people_fully_vaccinated_per_hundred), stat="identity",  size=.10, color="black", fill=vacc_color, alpha=.4) + 
  geom_line( aes(y=new_cases/coeff), size=.8, color=cases_color) +
  
  scale_y_continuous(
    
    # Primer eje, se especifica el nombre
    name = "% Vacunación",
    
    # Se agrega el segundo eje y se especifica el nombre 
    sec.axis = sec_axis(~.*coeff, name="Nuevos casos diarios")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = vacc_color, size=14),
    axis.title.y.right = element_text(color = cases_color, size=14)
  ) +
  
  ggtitle("Colombia: vacunación vs nuevos casos")


# Gráfico Colombia: vacunación vs muertes
# Valor para transformar la escala
coeff <- 10

# colores
vacc_color <- "#61D04F"
deaths_color <- rgb(0.2, 0.6, 0.9, 1)

# graficamos
ggplot(tail(colombia,291), aes(x=date)) +
  
  geom_bar( aes(y=people_fully_vaccinated_per_hundred), stat="identity",  size=.10, color="black", fill=vacc_color, alpha=.4) + 
  geom_line( aes(y=new_deaths / coeff), size=.8, color=deaths_color) +
  
  scale_y_continuous(
    
    # Primer eje y se especifica el nombre
    name = "% Vacunación",
    
    # Se agrega el segundo eje y se especifica el nombre
    sec.axis = sec_axis(~.*coeff, name="Fallecidos por día")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = vacc_color, size=14),
    axis.title.y.right = element_text(color = deaths_color, size=14)
  ) +
  
  ggtitle("Colombia: vacunación vs fallecidos")

# Gráficos Chile
# Grafico Chile: Vacunados vs nuevos casos
# Valor para transformar la escala
coeff <- 100

# colores
vacc_color <- "#61D04F"
cases_color <- rgb(0.2, 0.6, 0.9, 1)

# graficamos
ggplot(tail(chile,351), aes(x=date)) +
  
  geom_bar( aes(y=people_fully_vaccinated_per_hundred), stat="identity",  size=.10, color="black", fill=vacc_color, alpha=.4) + 
  geom_line( aes(y=new_cases/coeff), size=.8, color=cases_color) +
  
  scale_y_continuous(
    
    # Primer eje, se especifica el nombre
    name = "% Vacunación",
    
    # Se agrega el segundo eje y se especifica el nombre 
    sec.axis = sec_axis(~.*coeff, name="Nuevos casos diarios")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = vacc_color, size=14),
    axis.title.y.right = element_text(color = cases_color, size=14)
  ) +
  
  ggtitle("Chile: vacunación vs nuevos casos")


# Grafico Chile: Vacunados vs nuevos casos
# Valor para transformar la escala
coeff <- 10

# colores
vacc_color <- "#61D04F"
cases_color <- rgb(0.2, 0.6, 0.9, 1)

ggplot(tail(chile,351), aes(x=date)) +
  
  geom_bar( aes(y=people_fully_vaccinated_per_hundred), stat="identity",  size=.10, color="black", fill=vacc_color, alpha=.4) + 
  geom_line( aes(y=new_cases_per_million/coeff), size=.8, color=cases_color) +
  
  scale_y_continuous(
    
    # Primer eje y se especifica el nombre
    name = "% Vacunación",
    
    # Se agrega el segundo eje y se especifica el nombre
    sec.axis = sec_axis(~.*coeff, name="Nuevos casos por millón de habitantes")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = vacc_color, size=14),
    axis.title.y.right = element_text(color = cases_color, size=14)
  ) +
  
  ggtitle("Chile: vacunación vs nuevos casos")


# Gráfico Chile: vacunación vs muertes
# Valor para transformar la escala
coeff <- 3

# colores
vacc_color <- "#61D04F"
deaths_color <- rgb(0.2, 0.6, 0.9, 1)

# graficamos
ggplot(tail(chile,351), aes(x=date)) +
  
  geom_bar( aes(y=people_fully_vaccinated_per_hundred), stat="identity",  size=.10, color="black", fill=vacc_color, alpha=.4) + 
  geom_line( aes(y=new_deaths / coeff), size=.8, color=deaths_color) +
  
  scale_y_continuous(
    
    # Primer eje y se especifica el nombre
    name = "% Vacunación",
    
    # Se agrega el segundo eje y se especifica el nombre
    sec.axis = sec_axis(~.*coeff, name="Falledidos por día")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = vacc_color, size=14),
    axis.title.y.right = element_text(color = deaths_color, size=14)
  ) +
  
  ggtitle("Chile: vacunación vs fallecidos")

# Comparando los tests nuevos de cada día entre Colombia y Chile
# obtenemos los eje ‘x’,

x_colombia <- colombia$date
x_chile <- chile$date

# obtenemos los eje 'y'
y_colombia_tn <- colombia$new_tests
y_chile_tn <- chile$new_tests

# graficamos
plot(
  x_colombia, y_colombia_tn,
  type = "l",
  col = "indianred",
  lwd = 2,
  xlab = "",
  ylab = "",
  las = 1,
  xlim = c(as.Date("2021-01-01"), as.Date("2021-12-30"))
)
lines(
  x_chile, y_chile_tn,
  col="lightseagreen",
  lwd = 2
  
)
grid()
title(
  main = "Test diarios durante el 2021",
  line = 0.8
)
legend(
  x = "topright",
  legend = c("Colombia","Chile"),
  col = c("indianred","lightseagreen"),
  lty = 1,
  bty = "n",
  lwd = 2
)


# Colombia: nuevos casos diarios vs tests diarios
# Valor para transformar la escala
coeff <- 5

# colores
casesColor <- "#69b3a2"
testColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(colombia, aes(x=date)) +
  
  geom_line( aes(y=new_cases), size=.8, color=casesColor) + 
  geom_line( aes(y=new_tests / coeff), size=.8, color=testColor) +
  
  scale_y_continuous(
    
    # Primer eje y se especifica el nombre
    name = "Nuevos casos por día",
    
    # Se agrega un segundo eje y se especifica el nombre
    sec.axis = sec_axis(~.*coeff, name="Tests por día")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = casesColor, size=13),
    axis.title.y.right = element_text(color = testColor, size=13)
  ) +
  
  ggtitle("Colombia: nuevos casos por día vs nuevos test por día")


# Chile: nuevos casos diarios vs tests diarios
# Valor para transformar la escala
coeff <- 5

# colores
casesColor <- "#69b3a2"
testColor <- rgb(0.2, 0.6, 0.9, 1)

# graficamos
ggplot(chile, aes(x=date)) +
  
  geom_line( aes(y=new_cases), size=.8, color=casesColor) + 
  geom_line( aes(y=new_tests / coeff), size=.8, color=testColor) +
  
  scale_y_continuous(
    
    # Primer eje y se especifica el nombre
    name = "Nuevos casos por día",
    
    # Se agrega el segundo eje y se especifica el nombre
    sec.axis = sec_axis(~.*coeff, name="Tests por día")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = casesColor, size=13),
    axis.title.y.right = element_text(color = testColor, size=13)
  ) +
  
  ggtitle("Chile: nuevos casos por día vs nuevos test por día")

# Resumen indicadores Colombia
# Totales
resumenColombia <- data.frame(
  Total_Casos = sum(colombia$new_cases),
  Total_Tests = sum(colombia$new_tests),
  Total_Muertes = sum(colombia$new_deaths),
  Porcentaje_Vacunados = max(colombia$people_fully_vaccinated_per_hundred),
  PromedioMuertesVSpoblación = (sum(colombia$new_deaths)/51265841)*100,
  PromedioCasosVSpoblación = (sum(colombia$new_cases)/51265841)*100,
  PromedioTestsVSpoblación = (sum(colombia$new_tests)/51265841)*100
)
  
# Resumen indicadores Chile
# Totales
resumenChile <- data.frame(
  Total_Casos = sum(chile$new_cases),
  Total_Tests = sum(chile$new_tests),
  Total_Muertes = sum(chile$new_deaths),
  Porcentaje_Vacunados = max(chile$people_fully_vaccinated_per_hundred),
  PromedioMuertesVSpoblación = (sum(chile$new_deaths)/19212362)*100,
  PromedioCasosVSpoblación = (sum(chile$new_cases)/19212362)*100,
  PromedioTestsVSpoblación = (sum(chile$new_tests)/19212362)*100
)  
  
  
#- Se exporta en formato CSV el df datos_final al fichero "dataset_actividad1.csv". 
write_csv(datos_final, file = "dataset_actividad1.csv")
