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





df <-
 read_behaviorspace_spreadsheet(
  "Experiments/VirtEcom3 Experiment-spreadsheet.csv"
 )
variable.names(df)
colnames(df)[1] <- "step"   
variable.names(df)
colnames(df)[12] <- "sum.debt.of.firms"   
variable.names(df)
colnames(df)[3] <- "max.triads.per.worker"   
colnames(df)[4] <- "credit.limit.base"   
colnames(df)[5] <- "mean.firm.ck"
colnames(df)[7] <- "number.of.firms"
colnames(df)[8] <- "count.products"
colnames(df)[13] <- "mean.net.capital"
colnames(df)[14] <- "firms.with.commercial.capability"
colnames(df)[15] <- "firms.with.digital.capability"
colnames(df)[16] <- "firms.with.ai.capability"
colnames(df)[17] <- "mean.capability.count"
variable.names(df)


library(tidyverse)
df %>%
  summarise(
    min_commercial = min(firms.with.commercial.capability, na.rm = TRUE),
    max_commercial = max(firms.with.commercial.capability, na.rm = TRUE),
    
    min_digital = min(firms.with.digital.capability, na.rm = TRUE),
    max_digital = max(firms.with.digital.capability, na.rm = TRUE),
    
    min_ai = min(firms.with.ai.capability, na.rm = TRUE),
    max_ai = max(firms.with.ai.capability, na.rm = TRUE),
    
    min_cap = min(mean.capability.count, na.rm = TRUE),
    max_cap = max(mean.capability.count, na.rm = TRUE)
  )


df_clean <- as.data.frame(df) %>%
  mutate(
    run = as.integer(run),
    step = as.integer(step),
    max.triads.per.worker = as.numeric(max.triads.per.worker),
    credit.limit.base = as.numeric(credit.limit.base),
    firms.with.commercial.capability = as.numeric(firms.with.commercial.capability),
    firms.with.digital.capability = as.numeric(firms.with.digital.capability),
    firms.with.ai.capability = as.numeric(firms.with.ai.capability),
    mean.capability.count = as.numeric(mean.capability.count)
  )

# Filtrar corte transversal en estado estacionario (Tick 2000)
df_ss <- df_clean %>%
  filter(step == 2000)


# ------------------------------------------------------------------
# 2. ANÁLISIS DESCRIPTIVO DE LAS CAPACIDADES DE INNOVACIÓN (TICK 2000)
# ------------------------------------------------------------------
cat("\n=== ESTADÍSTICOS DE CAPACIDADES EN ESTADO ESTACIONARIO (TICK 2000) ===\n")
resumen_capacidades <- df_ss %>%
  summarise(
    Media_Commercial = mean(firms.with.commercial.capability, na.rm = TRUE),
    Media_Digital    = mean(firms.with.digital.capability, na.rm = TRUE),
    Media_AI         = mean(firms.with.ai.capability, na.rm = TRUE),
    Media_Portafolio = mean(mean.capability.count, na.rm = TRUE),
    SD_AI            = sd(firms.with.ai.capability, na.rm = TRUE),
    SD_Portafolio    = sd(mean.capability.count, na.rm = TRUE)
  )
print(resumen_capacidades)


# ------------------------------------------------------------------
# 3. EVALUACIÓN FACTORIAL (ANOVA Y TAMAÑOS DE EFECTO) PARA AI & COMPLEXITY
# ------------------------------------------------------------------
fit_ai <- aov(
  firms.with.ai.capability ~
    factor(max.triads.per.worker) *
    factor(credit.limit.base),
  data = df_ss
)

cat("\n=== ANOVA: FIRMAS CON CAPACIDAD DE IA ===\n")
print(summary(fit_ai))

cat("\n=== TAMAÑOS DE EFECTO (ETA CUADRADO PARCIAL) ===\n")
print(eta_squared(fit_ai, partial = TRUE))


# ------------------------------------------------------------------
# 4. MODELOS MIXTOS (LMM) AMPLIADOS PARA CAPACIDADES Y PORTAFOLIO
# ------------------------------------------------------------------
# Evaluamos cómo el límite cognitivo (MT) y el crédito (CB) modulan 
# la densidad de capacidades de IA y el conteo medio de capacidades a lo largo del tiempo.


library(lme4)
library(lmerTest)
library(effectsize)

lmm_ai <- lmer(
  firms.with.ai.capability ~ scale(max.triads.per.worker) * scale(credit.limit.base) + (1 | run),
  data = df_clean
)

lmm_portfolio <- lmer(
  mean.capability.count ~ scale(max.triads.per.worker) * scale(credit.limit.base) + (1 | run),
  data = df_clean
)

cat("\n=== RESUMEN LMM: CAPACIDAD DE IA ===\n")
print(summary(lmm_ai))

cat("\n=== RESUMEN LMM: CONTEO MEDIO DE CAPACIDADES ===\n")
print(summary(lmm_portfolio))


# ------------------------------------------------------------------
# 5. VISUALIZACIONES PARA EL MANUSCRITO (ESTÁNDAR JIK / JASSS)
# ------------------------------------------------------------------

# A. Evolución temporal de capacidades segmentada por restricciones cognitivas
p_evolucion_caps <- df_clean %>%
  group_by(step, max.triads.per.worker) %>%
  summarise(
    commercial = mean(firms.with.commercial.capability, na.rm = TRUE),
    digital    = mean(firms.with.digital.capability, na.rm = TRUE),
    ai         = mean(firms.with.ai.capability, na.rm = TRUE),
    capability = mean(mean.capability.count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(commercial, digital, ai, capability),
    names_to = "metric",
    values_to = "value"
  ) %>%
  ggplot(aes(x = step, y = value, color = factor(max.triads.per.worker))) +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  facet_wrap(~ metric, scales = "free_y", labeller = label_parsed) +
  scale_color_viridis_d(option = "plasma", name = "Max. Triads (MT)") +
  labs(
    x = "Simulation Steps (Ticks)",
    y = "Mean Systemic Value",
    title = ""
  ) +
  theme_linedraw(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "#264653"),
    strip.text = element_text(face = "bold", color = "white")
  )

print(p_evolucion_caps)


# B. Mapa de calor de interacción (Cognición x Crédito) sobre la adopción de IA
p_heatmap_ai <- df_ss %>%
  group_by(max.triads.per.worker, credit.limit.base) %>%
  summarise(ai_mean = mean(firms.with.ai.capability, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = factor(max.triads.per.worker), y = factor(credit.limit.base), fill = ai_mean)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_viridis_c(option = "viridis", name = "Firms with AI Capability") +
  labs(
    x = "Cognitive Attention Bound (Max Triads per Worker)",
    y = "Base Credit Facility Limit",
    title = ""
  ) +
  theme_linedraw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "right",
    panel.grid = element_blank()
  )

print(p_heatmap_ai)

library(patchwork)

# Opcional: Ajustar temas o leyendas antes de unir si es necesario
# Combinar lado a lado con el operador '|' o '+' y controlar las etiquetas de los subgráficos (A, B)
p_combinada <- (p_evolucion_caps | p_heatmap_ai) +
  plot_layout(widths = c(1.4, 1)) + # Da un poco más de ancho a la evolución temporal si lo requiere
  plot_annotation(
    tag_levels = 'A', # Añade etiquetas automáticas (A, B) ideales para papers Q1
    theme = theme(plot.tag = element_text(face = "bold", size = 14))
  )

# Mostrar la gráfica combinada
print(p_combinada)

# Guardar en alta resolución para el manuscrito (PDF / TIFF / PNG a 300 DPI)
ggsave("F1.pdf", p_combinada, width = 14, height = 7, units = "in", dpi = 300)


library(tidyverse)
library(patchwork)

# 1. Función auxiliar para generar mapas de calor homogéneos y limpios
crear_heatmap <- function(data, var_col, titulo_panel, leyenda_titulo) {
  data %>%
    group_by(max.triads.per.worker, credit.limit.base) %>%
    summarise(val_mean = mean(.data[[var_col]], na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = factor(max.triads.per.worker), y = factor(credit.limit.base), fill = val_mean)) +
    geom_tile(color = "white", linewidth = 0.4) +
    scale_fill_viridis_c(option = "viridis", name = leyenda_titulo) +
    labs(
      title = titulo_panel,
      x = "Max. Triads (MT)",
      y = "Credit Limit"
    ) +
    theme_linedraw(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
      axis.title = element_text(size = 9),
      legend.position = "bottom",
      legend.key.width = unit(0.8, "cm"),
      legend.key.height = unit(0.25, "cm"),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      panel.grid = element_blank()
    )
}

# 2. Generar los 4 mapas de calor en estado estacionario (Tick 2000)
p_hm_comm <- crear_heatmap(df_ss, "firms.with.commercial.capability", "Commercial Capability", "Firms")
p_hm_dig  <- crear_heatmap(df_ss, "firms.with.digital.capability",    "Digital Capability",    "Firms")
p_hm_ai   <- crear_heatmap(df_ss, "firms.with.ai.capability",         "AI Capability",         "Firms")
p_hm_cap  <- crear_heatmap(df_ss, "mean.capability.count",            "Mean Capability Count", "Count")

# 3. Ensamblar la figura completa (Fila 1: Evolución | Fila 2: 4 Heatmaps)
fila_inferior <- (p_hm_comm | p_hm_dig | p_hm_ai | p_hm_cap)

p_figura1_completa <- (p_evolucion_caps / fila_inferior) +
  plot_layout(heights = c(1.15, 1)) +
  plot_annotation(
    tag_levels = 'A',
    theme = theme(plot.tag = element_text(face = "bold", size = 13))
  )

# Visualizar
print(p_figura1_completa)

# Exportar en alta resolución para LaTeX (ancho completo de página)
ggsave("F1.pdf", p_figura1_completa, width = 14, height = 9.5, units = "in", dpi = 300)


library(tidyverse)



df %>%
  group_by(
    ticks,
    max.triads.per.worker
  ) %>%
  summarise(
    commercial = mean(firms.with.commercial.capability, na.rm = TRUE),
    digital    = mean(firms.with.digital.capability, na.rm = TRUE),
    ai         = mean(firms.with.ai.capability, na.rm = TRUE),
    capability = mean(mean.capability.count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(commercial, digital, ai, capability),
    names_to = "metric",
    values_to = "value"
  ) %>%
  ggplot(
    aes(
      x = ticks,
      y = value,
      color = factor(max.triads.per.worker)
    )
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(
    x = "Tick",
    y = "Mean value",
    color = "Max triads",
    title = "Evolution of capability indicators"
  ) +
  theme_minimal()

df %>%
  filter(ticks == 2000) %>%
  group_by(
    max.triads.per.worker,
    credit.limit.base
  ) %>%
  summarise(
    ai = mean(firms.with.ai.capability),
    capability = mean(mean.capability.count)
  )


df_ss <- df %>%
  filter(ticks == 2000)

fit <- aov(
  firms.with.ai.capability ~
    factor(max.triads.per.worker) *
    factor(credit.limit.base),
  data = df_ss
)

summary(fit)

library(effectsize)

eta_squared(
  fit,
  partial = TRUE
)

df_ss %>%
  group_by(
    max.triads.per.worker,
    credit.limit.base
  ) %>%
  summarise(
    ai = mean(firms.with.ai.capability),
    .groups = "drop"
  ) %>%
  ggplot(
    aes(
      factor(max.triads.per.worker),
      factor(credit.limit.base),
      fill = ai
    )
  ) +
  geom_tile()


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
  net_capital_k = mean.net.capital / 1000,
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

df_ss <- df %>%
  filter(ticks == 2000)

ggplot(
  df_ss,
  aes(
    x = credit.limit.base,
    y = mean.net.capital
  )
) +
  geom_jitter(
    width = 40,
    height = 0,
    alpha = 0.15,
    size = 1
  ) +
  theme_minimal() +
  labs(
    x = "Credit limit",
    y = "Net capital"
  )

df %>%
  filter(
    credit.limit.base == 1200,
    ticks > 1800
  ) %>%
  ggplot(
    aes(
      x = ticks,
      y = mean.net.capital
    )
  ) +
  geom_point(alpha = .1)

df_ss <- df %>%
  filter(ticks == 2000)

ggplot(
  df_ss,
  aes(
    x = factor(max.triads.per.worker),
    y = firms.with.ai.capability
  )
) +
  geom_jitter(
    width = .15,
    alpha = .20
  ) +
  theme_minimal()
