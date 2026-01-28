# Instalacion de librerias necesarias (solo si no las tienes)
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Paso 1. Cargar datos
# leemos el archivo csv
datos <- read.csv("variables_seleccionadas.csv")

# 2.3.1 Tabla-muestra de datos originales
# mostramos las primeras 10 filas para ver que todo este bien
tabla_muestra <- head(datos, 10)
print(tabla_muestra)

# 2.3.2 Tabla de distribucion de frecuencias agrupadas
# quitamos los valores vacios de la columna esperanza de vida
vida <- na.omit(datos$Life.expectancy)

# definimos los intervalos de 5 en 5 a?os
intervalos <- seq(floor(min(vida)), ceiling(max(vida)), by = 5)

# creamos la tabla base
frecuencia <- table(cut(vida, breaks = intervalos, right = FALSE))
tabla_frecuencia <- as.data.frame(frecuencia)

# renombramos las columnas para que se entienda mejor
colnames(tabla_frecuencia) <- c("Intervalo", "Frecuencia")
print(tabla_frecuencia)

# 2.3.3 Tabla de frecuencia agrupada acumulada
# sumamos acumulativamente las frecuencias
tabla_frecuencia$Frecuencia_Acumulada <- cumsum(tabla_frecuencia$Frecuencia)

# 2.3.4 Frecuencia relativa agrupada
# dividimos cada frecuencia entre el total de datos
total_datos <- sum(tabla_frecuencia$Frecuencia)
tabla_frecuencia$Frecuencia_Relativa <- tabla_frecuencia$Frecuencia / total_datos

# 2.3.5 Frecuencia relativa agrupada acumulada
# acumulamos la relativa
tabla_frecuencia$Frec_Rel_Acumulada <- cumsum(tabla_frecuencia$Frecuencia_Relativa)

# mostramos la tabla final completa
print(tabla_frecuencia)


# 2.4 Modelo de regresion y grafico de coeficientes (beta weight)
# seleccionamos solo las columnas que vamos a usar en el modelo
datos_reg <- subset(datos, select = c(Life.expectancy, GDP, Schooling, Status))
datos_reg <- na.omit(datos_reg)

# estandarizamos las variables numericas para obtener los pesos beta
# esto es importante para comparar magnitudes en el grafico
datos_reg$z_life <- scale(datos_reg$Life.expectancy)
datos_reg$z_gdp <- scale(datos_reg$GDP)
datos_reg$z_school <- scale(datos_reg$Schooling)

# creamos el modelo lineal prediciendo esperanza de vida
modelo <- lm(z_life ~ z_gdp + z_school + Status, data = datos_reg)

# extraemos los coeficientes e intervalos de confianza del modelo
coefs <- summary(modelo)$coefficients
intervalos <- confint(modelo)

# preparamos los datos para graficar
df_grafico <- data.frame(
  Variable = rownames(coefs),
  Estimacion = coefs[, "Estimate"],
  Minimo = intervalos[, 1],
  Maximo = intervalos[, 2]
)

# eliminamos el intercepto porque no se suele graficar aqui
df_grafico <- df_grafico[rownames(df_grafico) != "(Intercept)", ]

# generamos el grafico replicando el estilo de la imagen
ggplot(df_grafico, aes(x = Estimacion, y = Variable)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + # linea en cero
  geom_errorbarh(aes(xmin = Minimo, xmax = Maximo), height = 0.2) +  # barras de error
  geom_point(size = 3) + # puntos centrales
  labs(x = "Beta Weight", y = "") + # etiquetas
  theme_bw() + # tema limpio blanco y negro
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black", size = 11)
  )