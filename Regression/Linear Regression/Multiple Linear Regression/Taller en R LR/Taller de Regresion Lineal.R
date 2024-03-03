#============================================
#  TALLER DE REGRESION LINEAL
#============================================

library(MASS)
install.packages("ISLR")
library(ISLR)
install.packages("corrplot")
install.packages("GGally")

#1.- Selección de la data
data(Auto)
str(Auto)
summary(Auto)

#2.- Matriz con graficos de dispersion de todas las variables de la base
pairs(Auto)
# A continuacion, estudiamos la relacion entre las variables para identificar
# cuales pueden ser los mejores predictores o si hay alguna con una relacion
# tipo no lineal o detectar indicios de colinealidad (relacion entre variables
# explicativas). Excluimos la variable cualitativa name

#3.- Matriz de correlacion entre predictores
b <- round(cor(subset(Auto, select = -name), method = "pearson"), digits = 3)
b
# Valores de correlacion r proximos a 1 o -1 indican una alta correlacion de 
# variables. Tambien podemos representarlos graficamente
require(corrplot)
corrplot(round(cor(subset(Auto, select = -name)), digits = 3), type = "lower")

#4.- Distribucion de densidad de las variables cuantitativas del modelo
library(dplyr)
require(GGally)
ggpairs(select(Auto, -name), lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")

#------------------------------------------------------------------------------
# De lo analizado hasta ahora podemos concluir que:
#    i) Las variables que mayor relacion (no siendo del todo lineal) tienen con
#       mpg son: displacement (r = -0.8), weight (r = -0.83), 
#       horsepower (r = -0.77) y cylinders (r = -0.77), siendo la relacion
#       todas, negativas.
#   ii) Se observa una alta correlacion (colinealidad) entre pares de variables
#       como displacement y cylinders (r = 0.95) y displacement y 
#       weight (r = 0.93). Con ello, posiblemente no seria util introducir
#       pares en el modelo
#  iii) La distribucion de las variables parece acercarse bastante a una
#       distribucion normal, dado el numero de observaciones con las que
#       disponemos.
#------------------------------------------------------------------------------

# Vamos a generar el modelo con todos los predictores a excepcion de la variable
# name que proporciona el nombre del modelo del coche, y que en este caso es
# prescindible ya que no aporta informacion importante al modelo. R generara
# variables  dummy automaticamente para las variables cualitativas. Con la funcion
# contrasts() podriamos conocer que valor R ha asociado a cada nivel del 

#5.- Creacion del modelo inicial
modelo.lineal <- lm(mpg ~ . - name, data = Auto)
summary(modelo.lineal)

#------------------------------------------------------------------------------
# De lo analizado hasta ahora podemos concluir que:
#    i) El modelo con todas las variables introducidas como predictores es
#       de explicar el 82.15 % de la varianza observada en el consumo de 
#       combustible (R2 ajustado = 0.818)
#   ii) El p-value del modelo es significativo (2.2e-16), por lo que podemos
#       decir que el modelo es util y que existe una relacion entre los
#       predictores y la variable respuesta (al menos uno de los coeficientes es
#       distinto de 0)
#  iii) Los predictores que parecen tener una relacion estadisticamente
#       significativa con la variable de respuesta son: displacement, weight,
#       origin, a diferencia de cylinders, horsepower, y aceleration
#   iv) Ejemplo de interpretacion de coeficiente: por cada año que pasa, se
#       recorre mas distancia por volumen de combustible (??year = 0.75)
#       manteniendose el resto de predictores constante, es decir, aumenta la
#       eficiencia
#------------------------------------------------------------------------------

#6.- Determinar la calidad del modelo
step(modelo.lineal, direction = "both", trace = 1)

# acceleration (la variable con mayor p-value) ha sido la unica variable explicativa
# en el proceso de seleccion. Reajustamos el modelo excluyendo dicha variable

#7.- Actualizando el modelo
modelo.lineal2 <- update(modelo.lineal, formula = ~ . -acceleration)
summary(modelo.lineal2)

#8.- Los intervalos de confianza para cada uno de los coeficientes serian :
confint(modelo.lineal2)

#9.- Visualizar los residuos
par(mfrow=c(2,2))
plot(modelo.lineal2, lwd=2, col="blue")

#10.- Deteccion y visualizacion de observaciones influyentes
install.packages("car", dependencies = TRUE)
require(car)
par(mfrow=c(1,1))
influencePlot(modelo.lineal2)

#11.- Grafico de residuos estudentizados frente a valores ajustados por el modelo
library(ggplot2)
ggplot(data = Auto, aes(x = predict(modelo.lineal2),
      y = abs(rstudent(modelo.lineal2)))) + geom_hline(yintercept = 3,
      color = "grey", linetype = "dashed") +
      geom_point(aes(color = ifelse(abs(rstudent(modelo.lineal2)) > 3, "red",
      "black"))) + scale_colour_identity() + 
      labs(title = "Distribucion de los residuos estudentizados",
      x = "Prediccion modelo", y = "Residuos estudentizados") +
      theme_bw() + theme(plot.title = element_text(hjust = 0.5))

#12.- Deteccion de los residuos estudentizados > 3 considerados como outlier
which(rstudent(modelo.lineal2) > 3)
outlierTest(modelo.lineal2)

#13.- Test de hipotesis para el analisis de normalidad de los residuos
shapiro.test(modelo.lineal2$residuals)
ks.test(modelo.lineal2$residuals, "pnorm")

#14.- Test de contraste de homocedasticidad Breusch-Pagan
library(lmtest)
bptest(modelo.lineal2)

# Como hemos visto en el segundo paso del analisis, hay evidencias de alta
# colinealidad entre algunas variables. Podriamos utilizar la funcion vif()
# para calcular el factor de inflacion de la varianza y detectar variables
# con mayor colinealidad

corrplot(cor(select(Auto, cylinders, displacement, horsepower, weight, year,
                    origin)), method = "number", type = "lower")

#15.- Factores de inflacion de la varianza
vif(modelo.lineal2)

#------------------------------------------------------------------------------
# Hasta el momento podemos concluir que:
#      i) El ajuste lineal parece no ser del todo preciso, ya que se observa un
#         patron curvo en los residuos frente a los valores ajustados por el
#         modelo, ademas de que no acaban de distribuirse de forma homogenea en
#         torno a 0. El test de Breusch-Pagan tambien proporciona evidencias de
#         falta de homocedasticidad (p-value = 0.0007)
#     ii) El Q-Q plot refleja que hay indicios de falta de normalidad en los
#         residuos (aquellos de mayor valor), corroborado tambien por el test
#         de hipotesis de shapiro wilk (p-value = 2.32e-06)
#    iii) La observacion 14 parece tener un nivel alto de influencia, aunque
#         se considere como residuo de alta magnitud. La observacion 323 tambien
#         se considera influyente. Un analisis mas exhaustivo consistiria en 
#         excluir las observaciones y ver el impacto sobre el modelo.
#     iv) Los predictores cylinders y displacement muestran una alta inflacion
#         la varianza
#      v) Cuatro de las seis variables que incluye el modelo estan muy
#         correlacionadas.
#------------------------------------------------------------------------------

# Ya que algunas de las condiciones para el ajuste lineal no acaban de
# satisfacerse, y observando la matriz de correlacion podemos ver como la
# distribucion de las variables horsepower, displacement, y weight tiene un
# patron no lineal parecido frente a mpg, podriamos aproximar el ajuste
# utilizando un polinomio de grado 2. En el siguiente intento podemos incluir
# terminos polinomicos a estas variables y estudiar si el modelo mejora. Es
# importante no excederse en el grado de polinomio para evitar el "overfiting"
# ya que cuanto mayor es el polinomio, mas flexible es el modelo

#16.- Reajuste del modelo
modelo.lineal.poli <- update(modelo.lineal2, formula = ~ . +
                      poly(displacement, 2) + poly(horsepower, 2) +
                      poly(weight, 2))
summary(modelo.lineal.poli)

#17.- Test de hipotesis para evaluar si un modelo se ajusta mejor que el original
anova(modelo.lineal2, modelo.lineal.poli)

#------------------------------------------------------------------------------
# Incluyendo terminos polinomicos (siendo en displacement menos significativo)
# hemos conseguido mejorar el modelo y que explique casi un 5% mas de la
# variabilidad (R2ajustado = 0,8611 y p-value de ANOVA = 2.2e-16). Las
# observaciones 323 y 14 podrian estar influyendo en el modelo
#------------------------------------------------------------------------------

x <- log(Auto$acceleration)
par(mfrow=c(1,1))
hist(x, prob=T)

