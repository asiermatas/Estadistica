############################ ANALISIS ESTADISTICO - Master BI y BD  ###############################

# Hacer uso del dataset "diamonds" que contendrá¡ el precio (entre otras variables interesantes) de unos 54.000 diamantes.
#
# Objetivo : realizar distintos tipos de análisis estadístico de sus variables para intentar
# averiguar algún tipo de comportamiento oculto aparentemente en los datos. 
#
# Para ello os marco los siguientes pasos: tipos de variables, medidas de posición central, medidas de dispersión, 
# distribución y relación entre ellas, más análisis de regresión
#
# Los diferentes indicadores presentes en el dataset "diamonds" son los siguientes:
# price: Precio en dolares americanos
# carat: peso del diamante
# cut: calidad del corte (Fair, Good, Very Good, Premium, Ideal)
# colour: color del diamante (desde D el mejor hasta J el peor)
# clarity: mide como de claro es el diamante (desde el peor I1, SI2, SI1, VS2, VS1, VVS2, VVS1, hasta el mejor IF)
# x: longitud en mm 
# y: ancho en  mm 
# z: profundidad en mm 
# depth: porcentaje total de profundidad 
# table: anchura de la parte superior de diamante con relaci�n al punto m�s ancho 


# Responde cada bloque cubriendo al menos lo indicado:

Muestra representativa
# Selecciona una muestra representativa para "cut"

An�lisis de las variables
# An�lisis descriptivo de las variables: Tipo de variable, distribuci�n y representaci�n
# Detecci�n de casos at�picos y su tratamiento

Inferencia
# Calcula un intervalo de confianza para la media de "carat" y "depth"
# Formula un test de hip�tesis

Relaciones entre las variables
# Muestra las relaciones que existen entre variables 
# (dependencia, anova, correlaci�n)

An�lisis de regresi�n
# Formular un modelo de regresi�n y analiza los resultados
# Muestra los residuos y analiza los resultados
# Aplica una transformaci�n a la regresi�n y analiza los resultados
# Interpreta los coeficientes estandarizados de la regresi�n



#Muestra representativa
# Selecciona una muestra representativa para "cut"

#Primero cargamos la libreria ggplot2 que es donde viene la base de  diamons:
#Y despues con head comprobamos que se ha cargado

library(ggplot2)
head(diamonds)

# Despu�s buscamos e instalamos el paquete de sampling

install.packages("sampling")

#Y cargamos la libreria de sampling

library(sampling)

#Hacemos una copia de diamonds a diamonds2 para poder trabajar sobre la copia y dejar el original intacto

diamonds2 <- diamonds

# Una vez tenemos la copia vamos a mirar el data frame del diamonds2

head(diamonds2)

nrow(diamonds2)

# vemos las caracteristicas de diamonds2 especialmente las correspondientes a la columna cut

summary(diamonds2)

#Entonces tenemos que el total de filas es 53940 y cada uno de los valores es:
  
#Fair     : 1610 --> dividido entre el total: 53940 --> 0,02984 
#Good     : 4906 --> dividido entre el total: 53940 --> 0,09095
#Very Good:12082 --> dividido entre el total: 53940 --> 0,22398
#Premium  :13791 --> dividido entre el total: 53940 --> 0,25567
#Ideal    :21551 --> dividido entre el total: 53940 --> 0,39953

#Esto significa que para una muestra aleatoria representativa de 1000 muestras tendriamos que tener:
  
#30 muestras de corte fair (es decir  0,02984 multiplicado por 1000 cn un redondeo)
#91 muestras de corte good
#224 muestras de corte very good
#256  muestras de corte Premium
#399  muestras de corte Ideal

#Y a continuaci�n hacemos una extraccion de dichas muestras: 

estratos <- strata( diamonds2, stratanames = c("cut"), size=c(399,256,224,91,30), method = "srswr" )

diamonds2_muestra <- getdata( diamonds2, estratos )

summary(diamonds2_muestra)


#An�lisis de las variables
# An�lisis descriptivo de las variables: Tipo de variable, distribuci�n y representaci�n
#Detecci�n de casos at�picos y su tratamiento


#Con la funci�n summary podemos ver caracteristica como numero de entradas , mediana, quartiles, valores minimos y maximos....

summary(diamonds2)

#Hay valores erroneos. En las medidad aparecce  valores miimo 0 en X, Y y Z, lo cual significa que si algun diamante tiene 0 
#en alguna de esos tres parametros es que nno es tridimensional sino bi dimensional.... es decir... que eso es un error.


#Hay que sacar estas mediciones de la base de datos. Vamos a seleccionar esos registros valor 0.000 y cambiarlos por NA

NA -> diamonds2[diamonds2$x == 0.000, ]

#y depues borrarlos

diamonds3 <- na.omit(diamonds2)

NA -> diamonds3 [diamonds3$z == 0.000, ]

diamonds4 <- na.omit(diamonds3)

# Hemos eliminado todas las entradas con valor 0.000 (en total 20). Lo podemos comprobar con summary

nrow(diamonds4)

summary(diamonds4)

# Que queremos ver:  En este caso lo que queremos es ver que valores nos pueden determinar el precio. 
# Para ello lo primero vemos qu variables tenemos a parte de la del precio
#Con la funcion str tendremos informacion del tipo de cada variable

str(diamonds4)

# Carat o quilates, es el peso de un diamante. En este caso, habra una relaci�n directa entre peso y las variables del tama�o
# es decir Z, X e Y.

# Vamos a comprobarlo

#Aqui vemos que cuanto m�s X mayor peso.
plot(diamonds4$x, diamonds4$carat)

#Aqui vemos que cuanto m�s y mayor peso.

plot(diamonds4$y, diamonds4$carat)

#Aqui vemos que cuanto m�s z mayor peso.
plot(diamonds4$z, diamonds4$carat)
#Igual que aqu�. A un minimo cambio de Z sube el peso, quilaters, carat.

# por eso vamos a dejar de trabajar con X, Z e Y y solo trabajaremos con carat.

#Vamos a ver la relaci�n entre precio y todas las variables, una a una. Para ello instalaremos la libreria ggplot2

library(ggplot2)

plot(diamonds4$cut, diamonds4$price)

plot(diamonds4$color, diamonds4$price)

plot(diamonds4$clarity, diamonds4$price)

plot(diamonds4$table, diamonds4$price)

plot(diamonds4$depth, diamonds4$price)

plot(diamonds4$price, diamonds4$carat)

plot(diamonds4$carat, diamonds4$price)

# De estos dos gr�ficos deducimos una serie de cosas importantes:
# - Carat es la variable con mayor peso en el precio. Las dem�s variables depth, cut, clarity tablle y color 
#tienen mucho menor valor explicativo.
# - Por si solo, el peso, no explica 100% el precio. Dentro de diamantes con un mismo peso, tenemos diamantes 
# que valen menos de 5000 a otros que valen m�s de 10000 por lo que tenemos m�s parametros que influyen en el precio
# - Tambi�n se apreccia que tenemos una serie de valores outliers en Carat, concreatamente todos los que
#pesan m�s de 2,6 quilates

# Vamos a quitar esos outliers

# Primero copia

diamonds5 <- diamonds4


NA -> diamonds5[diamonds5$carat > 2.6, ]

#y depues borrarlos

diamonds6 <- na.omit(diamonds5)

# ahorra tiene otra pinta

plot(diamonds6$carat, diamonds6$price)



# vamos a comparar todas las variables con el precio y peso (carat) para intentar ver una relaci�n que nos exlique 
# las diferencias a igual peso.


c2 <- ggplot(diamonds6, aes(cut, carat))
c2+geom_point()+aes(color=price)

c3 <- ggplot(diamonds6, aes(clarity, carat))
c3+geom_point()+aes(color=price)

c4 <- ggplot(diamonds6, aes(depth, carat))
c4+geom_point()+aes(color=price)

c5 <- ggplot(diamonds6, aes(color, carat))
c5+geom_point()+aes(color=price)

c6 <- ggplot(diamonds6, aes(table, carat))
c6+geom_point()+aes(color=price)

plot(diamonds6)



Inferencia
# Calcula un intervalo de confianza para la media de "carat" y "depth"

# para ello utilizamos la funcion t.test

t.test(diamonds6$carat)

t.test(diamonds6$depth)


# Formula un test de hip�tesis

# hipotesis1 : El precio es distinto en diamantes de m�s 2 quilates de distinto table

pt1 <- diamonds6[diamonds6$carat > 2.0 & diamonds6$table>55, ]

pt2 <- diamonds6[diamonds6$carat > 2.0 & diamonds6$table<55, ]

#miramos que la extraci�n es correcta

head(pt1)


t.test(pt1$price, pt2$price)

# El resultado es p-value = 0.1518 lo cual significa que NO puedo rechazar la tesis contraria a la mia.
# la teoria contraria a la mia es que el precio de los diamantes de m�s dde 2 quilates con distinto tabble es igual
# Dado que no puedo negar esto, tampoco podre afirmar mi hipotesis: que difiere precio en diamantes los 2 quilates
#segun su table



# hipotesis2 : El precio es distinto en diamantes de menos de 2 quilates de distinto table


pt3 <- diamonds6[diamonds6$carat < 2.0 & diamonds6$table>55, ]

pt4 <- diamonds6[diamonds6$carat < 2.0 & diamonds6$table<55, ]

#miramos que la extraci�n es correcta

head(pt3)

t.test(pt3$price, pt4$price)

# El resultado es p-value < 2.2e-16 lo cual significa que SI puedo rechazar la tesis contraria a la mia.
# la teoria contraria a la mia es que el precio de los diamantes de menos de 2 quilates con distinto table es igual
# Dado que si puedo negar esto, puedo afirmar mi hipotesis: que difiere precio en diamantes de menos de 2 quilates
# segun su table




Relaciones entre las variables

# Muestra las relaciones que existen entre variables 
# (dependencia, anova, correlaci�n)

# Ya hemos heccho analisis de dependencia anteriormente entre carat-precio y cada una de las variables:

c2+geom_point()+aes(color=price)

# aqu� vemos una alta relacion precio - carat&cut ya que cuanto mas grande y mejor cortado (ideal) el diamante mas altos
# son los precios de los diamantes (m�s clara los puntos)

c3+geom_point()+aes(color=price)

# Aqui seguimos viendo que cuanto mas quilates m�s precio, pero no es tan clara la relacion clarity con precio
# Si sigue habiendo una relacion clara de poca claridad menos precio. Pero en claridad SI2, SI1, VS2 y VS1 no es tan acusado
# adem�s se ven que cuanto mayor es el diamante menos probalilidad de tener un cliente de gran claridad

c4+geom_point()+aes(color=price)

# en este caso el aumento de precio esta directamente relacionado con el peso. Y muy poca relacion con el depth

c5+geom_point()+aes(color=price)

# Aqui seguimos viendo que cuanto mas quilates m�s precio, pero no es tan clara la relacion color con precio salvo en los
# dos ultimos casos. Los colores D, E, F G y H tienen una distribucion de precio - color parecido.
# uniamente baja los pprecios en los dos ultimos casos: IJ


c6+geom_point()+aes(color=price)

# la relaci�n entre table y precio es una vez menos directa.


##Asi ya vemos, como dijimos antes, que:
  
# Relaci�n price carat es muy directa
# lo mismo que la relacion carat con Z, X, y   
# La relaci�n table y depth no es nada directa con precio.
# La variable cut tiene bastante relacion directa con el precio. 
# Las variables Clarity y color tienen una relaci�n poco directa con precio que solo se aprecia en ciertos clasificaciones puntuales.


# Para hacer correlaci�n tenemos que comparar variables continuas, no categoricas.

head(diamonds6)

correlacion <- data.frame(diamonds6$carat, diamonds6$depth, diamonds6$price, diamonds6$table)

head(correlacion)

cor(correlacion)

# Confirmamos que la variable con mayor correlaci�n es carat, peso, quilates


# analisis anova. Instalamos libreria dplyr

library(dplyr)

# depu�s ejecutamos con variable categorica cut

anova_cut<-aov(diamonds6$price~diamonds6$cut)
summary(anova_cut)

# En ambos casos al salir el P-Value rechazamos ambas hipotesis de igualdad de precios medios entre cada tipo de corte.
# Es decir que ccomprobamos una vez m�s que la variable cut tiiene una relacion bastante directa con el precio.



An�lisis de regresi�n
# Formular un modelo de regresi�n y analiza los resultados
# Muestra los residuos y analiza los resultados
# Aplica una transformaci�n a la regresi�n y analiza los resultados
# Interpreta los coeficientes estandarizados de la regresi�n


# Vamos ha hacer el modelo con las caracteristicams m�s directamente relacionadas: Carat y price

modelo <- lm(price ~ carat , data=diamonds6) 

summary(modelo)

# veos que nos da un coeficiente de determinaci�n alto: 0,85  

modelo <- lm(price ~ carat +  table + depth , data=diamonds6) 

summary(modelo)


# vemos que table y depth solo nos a�ade unn 0,004 lo que confirma su poca relaci�n. 
# Un modelo solo con table sale con una relaci�on de 0.01594 


# sin embargo a�adiendo clarity, que tenia una relaci�n m�s directa que table y depth ya nos aumenta has el 0,897


modelo_completo <- lm(price ~ carat + clarity  , data=diamonds6) 

summary(modelo_completo)


# Muestra los residuos y analiza los resultados

residuos<-modelo_completo$residuals
plot(residuos)


summary(residuos)

boxplot(residuos)

hist(residuos)

head(residuos)


# Aplica una transformaci�n a la regresi�n y analiza los resultados

# vamos a hacer una transformaci�n logaritmica del modelo


modelo_logaritmo <- lm(log(price) ~ log(carat) + clarity, data=diamonds6)

summary(modelo_logaritmo)

# El modelo da mejor precisi�n y se ajusta m�s

residuos<-modelo_logaritmo$residuals
plot(residuos)
















