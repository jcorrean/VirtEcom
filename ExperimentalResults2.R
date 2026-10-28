ibrary(tidyverse)
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


df <-
 read_behaviorspace_spreadsheet(
  "Experiments/VirtEcom3 Exp2_Credit_vs_Cognitive_Constraint-spreadsheet.csv"
 )
variable.names(df)
colnames(df)[1] <- "step"   
colnames(df)[12] <- "sum.debt.of.firms"   
colnames(df)[3] <- "max.triads.per.worker"   
colnames(df)[4] <- "credit.limit.base"   


library(dplyr)
df_clean <- as.data.frame(df) %>%
 mutate(
  run = as.integer(run),
  step = as.integer(step),
  max.triads.per.worker = as.numeric(max.triads.per.worker),
  credit.limit.base = as.numeric(credit.limit.base),
  sum.debt.of.firms = as.numeric(sum.debt.of.firms)
 )

library(ggplot2)

# 1. Filtrar y preparar el corte transversal en el estado estacionario (Tick 2000)
df_step2000 <- df_clean %>%
 filter(step == 2000) %>%
 mutate(
  net_capital_k = `mean-net-capital` / 1000,
  colapso = net_capital_k < 0,
  regimen = ifelse(net_capital_k < 0, "Systemic collapse", "Expansion / Survival")
 )

# 2. Estadísticos descriptivos globales del corte transversal
cat("=== ESTADÍSTICOS GLOBALES (TICK 2000) ===\n")
resumen_global <- df_step2000 %>%
 summarise(
  N = n(),
  Media = mean(net_capital_k, na.rm = TRUE),
  Mediana = median(net_capital_k, na.rm = TRUE),
  Desv_Est = sd(net_capital_k, na.rm = TRUE),
  Minimo = min(net_capital_k, na.rm = TRUE),
  Maximo = max(net_capital_k, na.rm = TRUE),
  Tasa_Global_Colapso = mean(colapso)
 )
print(resumen_global)

# 3. Tabla analítica de la Transición de Fase por nivel de crédito base
cat("\n=== TASA DE COLAPSO POR NIVEL DE CRÉDITO BASE ===\n")
tabla_transicion <- df_step2000 %>%
 group_by(credit.limit.base) %>%
 summarise(
  Total_Corridas = n(),
  Corridas_Colapso = sum(colapso),
  Prop_Colapso = mean(colapso),
  Capital_Promedio = mean(net_capital_k, na.rm = TRUE),
  Capital_Mediano = median(net_capital_k, na.rm = TRUE)
 )
print(tabla_transicion)

# 4. Visualización de la bimodalidad facetada por diseño experimental (Estándar JASSS)
p_bifurcacion <- ggplot(df_step2000, aes(x = net_capital_k, fill = regimen)) +
 geom_histogram(bins = 50, color = "white", alpha = 0.85) +
 facet_wrap(~ credit.limit.base, scales = "free_y", labeller = label_both) +
 scale_fill_manual(values = c("Systemic collapse" = "#e76f51", "Expansion / Survival" = "#2a9d8f")) +
 labs(
  title = "",
  subtitle = "",
  x = "Net Capital (Thousands)",
  y = "Replications",
  fill = "Macro-Economic Regime:"
 ) +
 theme_linedraw(base_size = 12) +
 theme(
  plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
  plot.subtitle = element_text(hjust = 0.5, size = 11),
  legend.position = "bottom",
  strip.background = element_rect(fill = "#264653"),
  strip.text = element_text(face = "bold", color = "white")
 )

print(p_bifurcacion)


library(gamlss)
library(ggplot2)
library(dplyr)
library(patchwork)

modelo_gamlss <- gamlss(
 formula = net_capital_k ~ pb(credit.limit.base),
 sigma.formula = ~ pb(credit.limit.base),
 family = TF(),
 data = df_step2000,
 trace = FALSE
)

# Definir los límites del eje y basados en los datos reales del paso 2000
y.lim <- range(df_step2000$net_capital_k, na.rm = TRUE)

# Aplicar tu función estándar plotSimpleGamlss adaptada a VirtEcom 3.0
plotSimpleGamlss(
 x = credit.limit.base,
 y = net_capital_k,
 model = modelo_gamlss,
 data = df_step2000,
 pch = 1,
 x.val = seq(min(df_step2000$credit.limit.base), max(df_step2000$credit.limit.base), length.out = 6),
 val = 2000,
 N = 500,
 ylim = y.lim,
 xlim = range(df_step2000$credit.limit.base, na.rm = TRUE),
 cols = viridis_pal(begin = 0.3, end = 0.95, alpha = 0.4, option = "D", direction = 1)(100)
)

# Diagnóstico de residuos mediante Worm-plot
wp(modelo_gamlss)

modelo_shash_simple <- gamlss(
 formula = net_capital_k ~ pb(credit.limit.base),
 sigma.formula = ~ pb(credit.limit.base),
 nu.formula = ~ 1,
 tau.formula = ~ 1,
 family = SHASH(),
 data = df_step2000,
 trace = FALSE
)

# Evaluar el worm-plot con esta especificación estable
wp(modelo_shash_simple)


library(gamlss)
library(dplyr)

# 1. Preparar el indicador binario de supervivencia
df_step2000 <- df_step2000 %>%
 mutate(
  net_capital_k = `mean-net-capital` / 1000,
  survived = if_else(net_capital_k > 0, 1, 0)
 )

# ==========================================
# PARTE 1: Modelo GAMLSS Binomial (Supervivencia / Colapso)
# ==========================================
modelo_bin <- gamlss(
 formula = survived ~ pb(credit.limit.base),
 family = BI(),
 data = df_step2000,
 trace = FALSE
)

# Resumen del efecto del crédito sobre la probabilidad de supervivencia
summary(modelo_bin)

# ==========================================
# PARTE 2: GAMLSS Condicionado (Agentes Supervivientes K > 0)
# ==========================================
df_survivors <- df_step2000 %>% filter(net_capital_k > 0)

# Ajustar la distribución SHASH modelando posición y escala con splines
modelo_cond <- gamlss(
 formula = net_capital_k ~ pb(credit.limit.base),
 sigma.formula = ~ pb(credit.limit.base),
 family = SHASH(),
 data = df_survivors,
 trace = FALSE
)

# Diagnóstico de residuos mediante Worm-plot para los supervivientes
wp(modelo_cond)
