########################################################################
# PRÁCTICA: RESHAPE DE DATOS EPIDEMIOLÓGICOS (WIDE <-> LONG)
# VERSIÓN SIN SOLUCIONES (ENUNCIADOS)
#
# Contenido:
#   Parte 0 -> Filtrado de una base de datos
#   Parte 1 -> Recodificar una variable character a factor
#   Parte 2 -> Comprobar y ordenar sus niveles
#   Parte 3 -> Transformar de wide a long con melt()
#   Parte 4 -> Volver de long a wide con dcast()
#   Parte 5 -> Comprobar que la información original se conserva

########################################################################

# Paquete necesario para melt() y dcast()
# install.packages("reshape2")   # descomentar si no está instalado
library(reshape2)


########################################################################
## GENERACIÓN DE LA BASE DE DATOS EPIDEMIOLÓGICA (no tocar)
########################################################################

set.seed(123)

n <- 30

id        <- paste0("P", sprintf("%02d", 1:n))
region    <- sample(c("Norte", "Sur", "Este", "Oeste"), n, replace = TRUE)
sexo      <- sample(c("Hombre", "Mujer"), n, replace = TRUE)
edad      <- sample(5:90, n, replace = TRUE)
gravedad  <- sample(c("leve", "moderado", "grave"), n,
                     replace = TRUE, prob = c(0.5, 0.3, 0.2))

# Temperatura corporal (ºC) registrada en 3 visitas de seguimiento
temp_visita1 <- round(rnorm(n, mean = 37.0, sd = 0.8), 1)
temp_visita2 <- round(rnorm(n, mean = 37.3, sd = 0.9), 1)
temp_visita3 <- round(rnorm(n, mean = 36.8, sd = 0.7), 1)

base_epi <- data.frame(
  id, region, sexo, edad, gravedad,
  temp_visita1, temp_visita2, temp_visita3,
  stringsAsFactors = FALSE
)

cat("Estructura de la base de datos generada:\n")
str(base_epi)
head(base_epi)


########################################################################
## PARTE 0 -> FILTRADO DE LA BASE DE DATOS
########################################################################

# Ejercicio 1
# a) Quedarse únicamente con los pacientes adultos (edad >= 18 años).
# b) De esos pacientes adultos, quedarse solo con los de la región
#    "Norte" o "Sur".
# c) Comprobar cuántas filas tiene la base filtrada con nrow().



########################################################################
## PARTE 1 -> RECODIFICAR UNA VARIABLE CHARACTER A FACTOR
########################################################################

# Ejercicio 2
# a) Comprobar la clase de la variable "gravedad" en base_filtrada.
# b) Recodificarla como factor (sin orden todavía) y guardarla
#    sobreescribiendo la columna.
# c) Comprobar que la clase ha cambiado correctamente.





########################################################################
## PARTE 2 -> COMPROBAR Y ORDENAR SUS NIVELES
########################################################################

# Ejercicio 3
# a) Ver los niveles actuales del factor "gravedad" con levels().
# b) Ver cuántos pacientes hay en cada nivel con table().
# c) Los niveles por defecto están en orden alfabético, lo cual no
#    tiene sentido clínico. Volver a crear el factor indicando el
#    orden correcto: "leve" < "moderado" < "grave".




########################################################################
## PARTE 3 -> TRANSFORMAR LA BASE DE WIDE A LONG CON melt()
########################################################################

# Ejercicio 4
# a) La base "base_filtrada" está en formato wide: cada paciente
#    tiene una fila, y las 3 visitas de temperatura están en 3
#    columnas distintas (temp_visita1, temp_visita2, temp_visita3).
#    Transformarla a formato long usando melt(), indicando como
#    variables identificadoras (id.vars) todas las columnas que NO
#    son de temperatura, y llamando a las nuevas columnas resultantes
#    "visita" (nombre de la variable) y "temperatura" (valor).
# b) Comprobar la estructura y las dimensiones de la base long
#    resultante (debe tener 3 veces más filas que base_filtrada).
# c) Renombrar los niveles de la columna "visita" para que en vez de
#    "temp_visita1", "temp_visita2", "temp_visita3" aparezcan
#    "Visita 1", "Visita 2", "Visita 3".





########################################################################
## PARTE 4 -> VOLVER DE LONG A WIDE CON dcast()
########################################################################

# Ejercicio 5
# a) A partir de "base_long", reconstruir la base en formato wide
#    usando dcast(), de forma que cada paciente vuelva a tener una
#    única fila y cada visita sea de nuevo una columna con su valor
#    de temperatura.
# b) Comprobar la estructura y las dimensiones de la base wide
#    reconstruida (debe tener el mismo número de filas que
#    base_filtrada).





########################################################################
# FIN DE LA PRÁCTICA
########################################################################
