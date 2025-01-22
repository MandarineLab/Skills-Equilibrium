#=========================================================
#                   TRABAJO FINAL
#=========================================================

#== Parte 1: Ordenamiento de espacio y estructura de trabajo ===
#== Parte 2: Procesamiento de información ======================

# Instalación de paquetes necesarios
install.packages("mice")
install.packages("arrow")
install.packages("treemapify")
install.packages("psych")
install.packages("ggpubr")

# Carga de librerías
library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(stringr)
library(ggplot2)
library(zoo)  
library(reshape2)
library(nortest)
library(forecast)
library(rpart)
library(arrow)
library(mice)
library(rpart)
library(treemapify)
library(psych)
library(ggpubr)

# ================ Lectura de archivo ======================
data_departamentos <- read_parquet(
  "data/raw/base_proyecto_final_.parquet")
head(data_departamentos)

# ============== Filtrado por departamento ==================
# Exploramos las variables / solo los datos del dep. de Piura
colnames(data_departamentos)   # Ver nombres de las columnas
unique(data_departamentos$dpto)# Ver categorías 
table(data_departamentos$dpto) # Contar ocurrencias por categoría
sum(data_departamentos$dpto == "Piura") # Contar filas  

# Filtramos los datos de Piura
data_piura <- dplyr::filter(data_departamentos, dpto=="Piura")
head(data_piura)

# =================== Resumen estadístico =====================
# Generamos un resumen general de los datos de Piura
summary(data_piura)

# Vemos las dimesiones de los datos:
cat("Número de filas:", nrow(data_piura), "\n")
cat("Número de columnas:", ncol(data_piura), "\n")

# Vemos el nombre de las varaibles.
colnames(data_piura)

# Primeras y últimas filas
head(data_piura)
tail(data_piura)

# Tipos de datos de cada columna
str(data_piura)

# Identificación de variables categóricas y numéricas  
cat("\nIdentificación de variables:\n")
categoricas <- names(data_piura)[sapply(data_piura, is.factor) | 
                                   sapply(data_piura, is.character)]
numericas <- names(data_piura)[sapply(data_piura, is.numeric)]

cat("Variables categóricas:\n", paste(categoricas, collapse = ", "), "\n")
cat("Variables numéricas:\n", paste(numericas, collapse = ", "), "\n")

# Número de variables categóricas y numéricas
cat("Hay", length(categoricas), "variables categóricas.\n")
cat("Hay", length(numericas), "variables numéricas.\n")

# Reorganización: movemos la variable target al inicio
data_piura <- data_piura %>%
  relocate(ln_ing_lab, .before = 1)

# -------------------------------------------------
# Identificación y manejo de valores faltantes 
# -------------------------------------------------

# Verificamos los valores faltantes por cada columna
missing_por_columna <- colSums(is.na(data_piura))
cat("Valores faltantes por columna:\n")
print(missing_por_columna)

# educ , sch , exper -> 1 missings
# parent, nucleo, parent_fam, civil, jefe, couple -> 14 missings
# ing_lab, ing_total, ln_ing, ln_ing_lab -> 240 missings
# estatal -> 170 missings

# Eliminamos "d544t" por tener +15% de valores faltantes
data_piura <- subset(data_piura, select = -d544t)

# Eliminamos datos faltantes > 1% 
data_piura <- data_piura %>%
  filter(complete.cases(select(., parent, nucleo, parent_fam, 
                               civil, jefe, couple,
                               educ , sch , exper)))

# Eliminamos datos faltantes > 5% 
data_piura <- data_piura %>%
  filter(complete.cases(select(., estatal)))


# Evaluamos datos faltantes
missings_evaluar <- data_piura %>%
  select(ing_lab, ing_total, ln_ing, ln_ing_lab, lngasfam)

# Visualizamos los valores faltantes 
md.pattern(missings_evaluar)

# ing_lab, ing_total, ln_ing, ln_ing_lab -> 206 missings

# ********************************************************* 
# ****** Imputación de ln_ing_lab con modelo rpart ********

# Variables explicativas
var_ <- c("age", "educ", "exper", "raza", "lenguacat", "urban")

# Filtrar las filas con y sin valores faltantes en 'ln_ing_lab'
d_completos_ln_ing_lab <- data_piura[!is.na(data_piura$ln_ing_lab), ]
d_faltantes_ln_ing_lab <- data_piura[is.na(data_piura$ln_ing_lab), ]

# Crear fórmula solo con las variables relevantes
f_ln_ing_lab <- as.formula(paste("ln_ing_lab ~", 
                                 paste(var_, collapse = " + ")))

# Estimación de ln_ing_lab
anova_mod_ln_ing_lab<- rpart(f_ln_ing_lab, 
                             data = d_completos_ln_ing_lab, 
                             method = "anova", 
                             na.action = na.omit)

# Predecir los valores faltantes
ln_ing_lab_pred <- predict(anova_mod_ln_ing_lab, 
                           d_faltantes_ln_ing_lab)

# Reemplazar los valores faltantes en el dataset original
data_piura$ln_ing_lab[is.na(data_piura$ln_ing_lab)
] <- ln_ing_lab_pred

md.pattern(missings_evaluar)

# ----------------------------------------------------------
# ------------ Visualización de variables ------------------

# Actualizamos las variables categóricas y numéricas
cat("\nIdentificación de variables:\n")
categoricas <- names(data_piura)[sapply(data_piura, is.factor) | 
                              sapply(data_piura, is.character)]
numericas <- names(data_piura)[sapply(data_piura, is.numeric)]

cat("Variables categóricas:\n", paste(categoricas, 
                                      collapse = ", "), "\n")
cat("Variables numéricas:\n", paste(numericas, 
                                    collapse = ", "), "\n")


# *********** Analizamos las variables numéricas ************
unique(numericas)

# ------------------- Gráficos de densidad -----------------
# Variables numéricas de interés
var_num_graphs <- c("ln_ing_lab", "lngasfam", 
                    "ln_ing" , "exper", "age")

# Crear gráficos de curvas de densidad
for (var in var_num_graphs) {
  png(paste0("graphs/density_", var, ".png")) 
  
  # Calcular la densidad y graficar
  densidad <- density(data_piura[[var]], na.rm = TRUE)
  plot(densidad, 
       main = paste("Curva de Densidad de", var), 
       xlab = var, 
       ylab = "Densidad", 
       col = "WHITE", 
       lwd = 2)  # Grosor de línea
  
  # Rellenar el área bajo la curva
  polygon(densidad, col = rgb(0.1, 0.1, 0.8, 0.3), 
                                border = "black")
  
  dev.off()  # Cerrar el dispositivo gráfico
}

# ---------------- Gráficos de Histograma -----------------
# Variables numéricas de interés
var_num2 <- list(
  nucleo = "N° de núcleos familiares",
  sch = "Escolaridad (años)",
  mieperho = "Total de miembros en el hogar"
)

# Histogramas profesionales con ggplot
for (var in names(var_num2)) {
  p <- ggplot(data_piura, aes_string(x = var)) +
    geom_histogram(binwidth = 1, fill = "#beee3b", 
                                color = "black") +
    labs(
      title = paste("Histograma de", var_num2[[var]]),
      x = var_num2[[var]], 
      y = "Frecuencia"
    ) +
    theme_minimal()

# Guardar el gráfico como archivo PNG
ggsave(filename = paste0("graphs/hist_", var, ".jpg"), 
       plot = p)}

# ---------------- Gráficos de Treemap  -----------------
# Variables categóricas
unique(categoricas)

# Variables categóricas de interés
var_cat1 <- list(
  educ = "Educación",
  civil = "Estado civil",
  niv_edu_g = "Último grado de estudios",
  gruocu = "Grupo ocupacional",
  ramact = "Rama de actividad económica",
  ramactsec = "Rama de actividad económica por sectores"
)

# Definir la paleta de colores personalizada
Paleta_colores <- c(
  "#006465", "#d1dc5a", "#00c9d2", "#edd75a",
  "#beee3b", "#e1b7ed", "#e0f7e0", "#77f2de", 
  "#f0f0d8", "#607848", "#ff1d44", "#73b06f",
  "#0c8f8f", "#405059", "#ffad08", "#604848"
)

for (var in names(var_cat1)) {

# Crear un dataframe con las frecuencias de las categorías
  freq_table <- data_piura %>%
    group_by(.data[[var]]) %>%
    summarise(Frecuencia = n()) %>%
    ungroup()
  
# Crear el gráfico tipo treemap
  p <- ggplot(freq_table, aes(area = Frecuencia, 
              fill = .data[[var]], label = "")) +
    geom_treemap() +
    labs(
      title = paste("Composición de", var_cat1[[var]]),
      fill = var_cat1[[var]]
    ) +
    scale_fill_manual(values = Paleta_colores) +  
    theme_minimal()

# Guardar el gráfico como archivo PNG
ggsave(filename = paste0("graphs/treemap_", var, ".jpg"), 
         plot = p)}

# ---------------- Gráficos pie  ------------------------

var_cat2 <- list(
  jefe = "Jefe del hogar",
  couple = "Casado o conviviente",
  estatal = "Centro de estudio estatal/no estatal",
  pobreza = "Pobreza",
  male = "sexo",
  urban = "urbano/rural",
  contrato = "Contrato",
  zona = "zona",
  lenguacat = "Lengua materna"
)

for (var in names(var_cat2)) {
# Crear un dataframe con las frecuencias de las categorías
  freq_table2 <- data_piura %>%
    group_by(.data[[var]]) %>%
    summarise(Frecuencia2 = n()) %>%
    ungroup()
  
# Calcular porcentajes
  freq_table2 <- freq_table2 %>%
    mutate(Percent = Frecuencia2 / sum(Frecuencia2) * 100)
  
# Crear el gráfico tipo pie
  p <- ggplot(freq_table2, aes(x = "", y = Percent, 
                              fill = .data[[var]])) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    labs(
      title = paste("Composición de", var_cat2[[var]]),
      fill = var_cat2[[var]],
      x = NULL,
      y = NULL
    ) +
    scale_fill_manual(values = Paleta_colores) +  
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
  
# Guardar el gráfico como archivo PNG
  ggsave(filename = paste0("graphs/pie_", var, ".jpg"), 
         plot = p)
}

# ------------------------------------------------------
# ------------- DETECCIÓN DE OUTLAYERS -----------------

# Target: "ln_ing_lab" = logaritmo de ingresos laboral
# Variables de itnerés:

var <- c("ln_ing_lab", "exper", "age", "nucleo", "sch", 
         "mieperho", "civil", "niv_edu_g", "gruocu", 
         "ramact", "ramactsec", "jefe", "couple", 
         "estatal", "pobreza", "male", "urban",
         "zona", "lenguacat")

# Filtrar el dataset
data_piura_filtrado <- data_piura %>% select(all_of(var))

# Verificamos los datos faltantes
md.pattern(data_piura_filtrado)

# Resumen estadístico de la variable target
summary(data_piura_filtrado$ln_ing_lab)

# ---------------- Gráficos boxplot  ------------------------

boxplot(data_piura_filtrado$ln_ing_lab, horizontal = TRUE)
ggsave("graphs/ingreso_laboral_2017.png", plot = p)

# Encontramos outlayers en su mayoría en la izquiera

# ----------------- Boxplots por grupos ---------------------

# Boxplot por grupo
box_varcat<- c("civil", "niv_edu_g", "gruocu", "ramact", 
               "ramactsec", "jefe", "couple", "estatal", 
               "pobreza", "male", "urban", "zona", 
               "lenguacat")

# Iterar sobre cada variable categórica
for (cat_var in box_varcat) {
  
# Crear el gráfico
  p <- ggplot(data = data_piura_filtrado, 
              aes_string(x = cat_var, y = "ln_ing_lab")) +
    stat_boxplot(geom = "errorbar", width = 0.2) + # Bigotes
    geom_boxplot(fill = "#f9cdad", colour = "#fc9d9a", 
                 alpha = 0.9, outlier.colour = "#fe4365") +
    scale_y_continuous(name = "Ingresos laborales (ln)") +
    scale_x_discrete(name = cat_var) +
    ggtitle(paste("Boxplot por", cat_var)) +
    theme_minimal() +
    theme(axis.line = element_line(colour = "black"
                                   , size = 0.25))
  
  # Guardar el gráfico en formato JPG
  ggsave(filename = paste0("graphs/boxplot_", cat_var, ".jpg"), 
         plot = p, width = 8, height = 6)
}

# ----------------------------------------------------------
# Matriz de correlaciones entre target y var continuas 
# ---------------------------------------------------------

# Creamos un dataframe solo de var numéricas de interés
var_num <- c("ln_ing_lab", "exper", "age", "nucleo", "sch", 
         "mieperho", "lningfam" , "lngasfam")

# Filtrar el dataset
data_piura_num <- data_piura %>% select(all_of(var_num))

#Obtengo mi matriz de correlación con varaibles numéricas. 
# ruta de archivo
jpeg("graphs/matriz_corr.jpg", width = 1000, height = 800)

corPlot(data_piura_num, 
        min.length = 4, 
        numbers = FALSE)
dev.off()

# =================================================
# DETECCIÓN DE MULTICOLINIALIDAD: 3 
# EXP - AGE, lngasfam - lningfam, sch (escolaridad) - exper
# =================================================

# ---------------------------------------------------------
# Gráficos de dispersión entre target y var continuas   

for (var in setdiff(var_num, "ln_ing_lab")) { 
  # Crear el gráfico
  p <- ggplot(data_piura_num, aes_string(x = var, 
                                         y = "ln_ing_lab")) +
    geom_point(color = "#24c0eb", 
               alpha = 0.6) + # Puntos de dispersión
    geom_smooth(method = "lm", color = "#fe4365", 
                linetype = "dashed", se = FALSE) + # Línea
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., 
                               sep = "~`,`~")), 
             label.x.npc = "left", label.y.npc = "top",
             color = "black", size = 5) + # Correlación
    labs(
      title = paste("Relación entre ln_ing_lab y", var),
      x = var,
      y = "Logaritmo de Ingreso Laboral (ln_ing_lab)"
    ) +
    theme_minimal()
  
# Guardar el gráfico
ggsave(filename = paste0("graphs/dispersion_ln_ing_lab_vs_", 
                         var, ".jpg"),
         plot = p, width = 8, height = 6)
}

# ======= Grádico de dispersión según grupos ================

# Variables categóricas a iterar
categorical_vars <- list(
  urban = c("Rural", "Urbano"),
  pobreza = c("Pobre extremo", "Pobre no extremo", "No pobre"),
  male = c("Mujer", "Hombre"),
  jefe = c("No", "Sí")
)

# Crear gráficos en un loop
for (var in names(categorical_vars)) {
  plot(data_piura$lningfam, 
       data_piura_filtrado$ln_ing_lab,
       pch = 19,
       col = Paleta_colores[factor(data_piura_filtrado[[var]])],
       main = paste("Gráfico para", var))
  
  legend("topleft",
         legend = categorical_vars[[var]],
         pch = 19,
         col = Paleta_colores)
  
  # Guardar cada gráfico como archivo
  jpeg(paste0("graphs/dispersion_", var, ".jpg"), 
       width = 800, height = 600)
  plot(data_piura$lningfam, 
       data_piura_filtrado$ln_ing_lab,
       pch = 19,
       col = Paleta_colores[factor(data_piura_filtrado[[var]])],
       main = paste("Gráfico para", var))
  legend("topleft",
         legend = categorical_vars[[var]],
         pch = 19,
         col = Paleta_colores)
  dev.off()
}
# _____________ Colegio estatal ________________

jpeg("graphs/dispersion_niv_edu_g.jpg", width = 800, height = 600)
plot(data_piura$lngasfam, 
     data_piura_filtrado$ln_ing_lab,
     pch = 19,
     col = Paleta_colores[factor(data_piura_filtrado$niv_edu_g)],
     main = "Gráfico para nivel educativo")
legend("topleft",
       legend = c("Primaria", "Secundaria incompleta", 
                  "Secundaria completa", 
                  "Superior incompleta", 
                  "Superior completa"),
       pch = 19,
       col = Paleta_colores)
dev.off()

# ------------------------------------------------
#              Comentarios finales
# ------------------------------------------------

# Correlación postiva: 
  # categóricas: niv_edu_g, jefe (si), pobreza (no pobre),urban
  # numéricas: lngasfam, sch (escolaridad años), nucleo
# Correlación negativa:
  # numéricas: age, exper
# Multicolinialidad entre:
# Outlayer a tratar:
  # ln_ing_lab, lngasfam

# Actualizamos nuestras variables y cargamos en la data
var_interm <- c("ln_ing_lab", "nucleo", "civil", 
              "gruocu", "niv_edu_g", "ramact", 
              "ramactsec", "jefe", "couple", "male", 
              "estatal", "pobreza", "zona", "urban",
              "lenguacat", "age", "sch", "lngasfam" )

# Filtrar el dataset y guardar
data_piura_interm <- data_piura %>% select(all_of(var_interm))
write_xlsx(data_piura_interm, 
           "data/interm/data_piura_intern.xlsx")

#Verificamos por último los missings
md.pattern(data_piura_interm) #0 missings

#---------------Tratamiento de outlayers--------------------

impute_outliers <- function(x) { # función para imputar
    quantiles <- quantile(x, c(0.05, 0.95)) 
    # Percentiles 5% y 95%
    x[x < quantiles[1]] <- mean(x)   # Imputar con la media
    x[x > quantiles[2]] <- median(x) # Imputar con la mediana
    return(x)
}

# Variables numéricas seleccionadas
num_vars <- c("ln_ing_lab", "lngasfam")

# Crear un nuevo dataset imputado
data_piura_final <- data_piura_interm

# Imputar outliers en cada variable numérica
for (var in num_vars) {
  data_piura_final[[var]] <- impute_outliers(data_piura_interm[[var]])
}
# Comparar antes y después para una variable
summary(data_piura_interm$ln_ing_lab)
summary(data_piura_final$ln_ing_lab)

# O graficar los boxplots
par(mfrow = c(1, 2))
boxplot(data_piura_interm$ln_ing_lab, main = "Antes de imputar")
boxplot(data_piura_final$ln_ing_lab, main = "Después de imputar")

#guardar
for (var in num_vars) {
  
  # Crear un archivo para guardar el gráfico
  png(filename = paste0("graphs/sin_outlayers", var, ".jpg"), 
      width = 800, height = 400)
  
  # Configurar la disposición de los gráficos
  par(mfrow = c(1, 2))
  
  # Boxplot antes de imputar
  boxplot(data_piura_interm[[var]], 
          main = paste("Antes de imputar -", var),
          col = "skyblue")
  
  # Boxplot después de imputar
  boxplot(data_piura_final[[var]], 
          main = paste("Después de imputar -", var),
          col = "orange")
  
  # Cerrar el archivo gráfico
  dev.off()
}

# Guardamos la data final
write_xlsx(data_piura_final, 
           "data/final/data_piura_final.xlsx")
