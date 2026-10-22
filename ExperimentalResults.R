# 1. Cargar librerías
library(tidyverse)
read_behaviorspace_spreadsheet <- function(file) {
 
 # --------------------------------------------------
 # Leer archivo
 # --------------------------------------------------
 
 raw <- readLines(file)
 
 # --------------------------------------------------
 # Encontrar sección all run data
 # --------------------------------------------------
 
 all_run_row <- grep(
  "all run data",
  raw,
  ignore.case = TRUE
 )[1]
 
 if(is.na(all_run_row)) {
  stop("No se encontró la sección all run data")
 }
 
 # --------------------------------------------------
 # La fila all_run_row contiene:
 # [all run data] + headers repetidos
 # --------------------------------------------------
 
 headers <- strsplit(
  raw[all_run_row],
  ","
 )[[1]]
 
 headers <- gsub('"', "", headers)
 
 # eliminar únicamente [all run data]
 headers <- headers[-1]
 
 vars <- unique(headers)
 
 vars_per_run <- length(vars)
 
 n_runs <- length(headers) / vars_per_run
 
 cat("\n")
 cat("----------------------------------------\n")
 cat("Variables detectadas:", vars_per_run, "\n")
 cat("Runs detectados:", n_runs, "\n")
 cat("----------------------------------------\n")
 
 print(vars)
 
 # --------------------------------------------------
 # Los datos empiezan inmediatamente después
 # --------------------------------------------------
 
 data_lines <- raw[(all_run_row + 1):length(raw)]
 
 out <- vector(
  "list",
  length(data_lines)
 )
 
 for(i in seq_along(data_lines)) {
  
  row <- strsplit(
   data_lines[i],
   ","
  )[[1]]
  
  row <- gsub('"', "", row)
  
  # eliminar primera columna vacía
  row <- row[-1]
  
  # validación
  if(length(row) != (vars_per_run * n_runs)) {
   
   warning(
    paste(
     "Fila",
     i,
     "tiene",
     length(row),
     "valores; esperaba",
     vars_per_run * n_runs
    )
   )
   
   next
   
  }
  
  mat <- matrix(
   row,
   ncol = vars_per_run,
   byrow = TRUE
  )
  
  df <- as_tibble(
   mat,
   .name_repair = "minimal"
  )
  
  names(df) <- vars
  
  df$run <- seq_len(n_runs)
  
  out[[i]] <- df
  
 }
 
 out <- bind_rows(out)
 
 out <- out %>%
  mutate(
   across(
    everything(),
    ~ suppressWarnings(as.numeric(.))
   ),
   run = as.integer(run)
  )
 
 out
 
}

# 2. Leer el CSV exportado por NetLogo BehaviorSpace 
# (omite las primeras 6 filas de encabezado de NetLogo)

df <-
 read_behaviorspace_spreadsheet(
  "Experiments/VirtEcom3 Exp2_Credit_vs_Cognitive_Constraint-spreadsheet.csv"
 )
variable.names(df)
colnames(df)[1] <- "step"   
colnames(df)[12] <- "sum.debt.of.firms"   
colnames(df)[3] <- "max.triads.per.worker"   
colnames(df)[4] <- "credit.limit.base"   

# 1. Cargar librerías para datos panel
library(plm)
library(lme4)
library(lmerTest) # Proporciona p-values para los modelos mixtos

# 2. Declarar la estructura de panel de datos (i = run, t = step/ticks)
pdata <- pdata.frame(df, index = c("run", "step"))

library(tidyverse)
library(lme4)
library(lmerTest)

# 1. Convertir a data.frame nativo de R y asegurar tipos de datos numéricos
df_clean <- as.data.frame(df) %>%
 mutate(
  run = as.integer(run),
  step = as.integer(step),
  max.triads.per.worker = as.numeric(max.triads.per.worker),
  credit.limit.base = as.numeric(credit.limit.base),
  sum.debt.of.firms = as.numeric(sum.debt.of.firms)
 )

# 2. Estimación del Modelo Lineal Mixto (Efecto aleatorio en el intercepto por corrida)
modelo_mixto <- lmer(
 sum.debt.of.firms ~ max.triads.per.worker * credit.limit.base + (1 | run),
 data = df_clean
)

# 3. Resumen estadístico con p-values exactos
summary(modelo_mixto)

