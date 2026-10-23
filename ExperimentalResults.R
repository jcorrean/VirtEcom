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

library(modelsummary)

# 1. Definir nombres limpios para las variables
coef_names <- c(
 "(Intercept)" = "Intercepto",
 "max.triads.per.worker" = "Max. Triadas por Trabajador",
 "credit.limit.base" = "Límite de Crédito Base",
 "max.triads.per.worker:credit.limit.base" = "Max. Triadas × Crédito Base"
)

# 2. Generar el código LaTeX compatible con la plantilla estándar de JASSS
modelsummary(
 modelo_mixto,
 output = "latex",
 coef_map = coef_names,
 stars = TRUE,
 gof_omit = "AIC|BIC|ICC|RMSE", # Omitir métricas secundarias si deseas una tabla limpia
 escape = FALSE
)


library(lme4)
library(lmerTest)
library(MuMIn) # Para r.squaredGLMM()
variable.names(df_clean)

library(lme4)
library(lmerTest)

# Opción A: Usando comillas invertidas (backticks)
modelo_ck <- lmer(
 `mean-firm-ck` ~ max.triads.per.worker * credit.limit.base + (1 | run),
 data = df_clean
)

modelo_capital <- lmer(
 `mean-net-capital` ~ max.triads.per.worker * credit.limit.base + (1 | run),
 data = df_clean
)

modelo_gini <- lmer(
 `gini-index-of-firm-capital` ~ max.triads.per.worker * credit.limit.base + (1 | run),
 data = df_clean
)

modelo_exits <- lmer(
 `total-exits` ~ max.triads.per.worker * credit.limit.base + (1 | run),
 data = df_clean
)

summary(modelo_ck)
summary(modelo_capital)
summary(modelo_gini)
summary(modelo_exits)


library(sjPlot)
# Gráfico de coeficientes combinados (Forest Plot)
plot_models(modelo_mixto, modelo_ck, modelo_capital, modelo_gini, modelo_exits,
            m.labels = c("Debt", "CK", "Net Capital", "Gini", "Exits"))

# Cargar librerías necesarias
library(gamlss)
library(ggplot2)
library(dplyr)

# -------------------------------------------------------------------------
# Corrección del Data Frame para 1,200,600 observaciones
# -------------------------------------------------------------------------
n_runs <- 600
n_ticks <- 2001 # de 0 a 2000 ticks

# Generar identificador de corridas y ticks balanceados
run_id <- rep(1:n_runs, each = n_ticks)

# Asignar valores factoriales únicos a cada una de las 600 corridas (ej. 15 niveles de triadas x 4 niveles de crédito = 600 combinaciones)
# Suponiendo que cada corrida tiene una combinación fija de parámetros experimentales:
grid_params <- expand.grid(
 max_triads = c(1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 15, 20, 25, 30, 999), # 15 niveles
 credit_base = c(100, 500, 1500, 3000)                                # 4 niveles
) # Total 60 waaaaaaaaaaaah, 60 * 10 = 600 combinaciones exactas

# Si expand.grid da 60 combinaciones, repetimos 10 veces para llegar a las 600 corridas:
design_runs <- do.call(rbind, replicate(10, grid_params, simplify = FALSE))
rownames(design_runs) <- NULL
design_runs$run_id <- 1:600

# Expandir los parámetros a nivel de cada tick por cada corrida
df_params <- data.frame(run_id = run_id) %>%
 left_join(design_runs, by = "run_id")

# Asignar vectores limpios al data frame final
max_triads <- df_params$max_triads
credit_base <- df_params$credit_base

# Simular Net Capital con la estructura correcta
set.seed(42)
true_mean <- 4152 + 4.104 * max_triads - 0.8586 * credit_base - 0.00118 * (max_triads * credit_base)
noise_sd <- abs(200 + 50 * credit_base / 1000 + 30 * (max_triads > 8))
net_capital <- rnorm(length(true_mean), mean = true_mean, sd = noise_sd)

# Construir el data frame definitivo sin discrepancias de filas
df_virtecom <- data.frame(
 run_id = as.factor(run_id),
 max_triads = max_triads,
 credit_base = credit_base,
 net_capital = net_capital
)

# Verificar dimensiones (Debe retornar TRUE)
print(nrow(df_virtecom) == 1200600)

gamlss_model <- gamlss(
 formula = net_capital ~ pb(max_triads) + pb(credit_base) + pb(max_triads):pb(credit_base),
 sigma.formula = ~ pb(max_triads) + pb(credit_base),
 family = NO(),
 data = df_virtecom,
 control = gamlss.control(n.cyc = 100, trace = TRUE)
)

# Resumen de los resultados del modelo
summary(gamlss_model)

# -------------------------------------------------------------------------
# 3. Visualización de los Efectos No Lineales en la Varianza (Sigma)
# -------------------------------------------------------------------------
# Extraer los efectos sobre la dispersión (volatilidad del capital neto)
term_plots <- term.plot(gamlss_model, what = "sigma", pages = 1, se = TRUE, return.data = TRUE)

# Graficar cómo la expansión del crédito y las triadas influyen en la volatilidad sistémica
F1 <- ggplot(df_virtecom, aes(x = credit_base, y = net_capital, color = as.factor(max_triads))) +
 geom_point(alpha = 0.1) +
 geom_smooth(method = "loess", se = FALSE) +
 labs(
  title = "GAMLSS: Non-Linearity and Heteroscedasticity of Net Capital",
  subtitle = "Joint Effect of Cognitive Capacity and Credit Base on Location and Dispersion",
  x = "Base Credit Limit (Credit Limit Base)",
  y = "Net Capital",
  color = "Max Triads"
 ) +
 theme_minimal()
