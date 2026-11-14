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

library(tidyverse)
library(effectsize)

# Filtrar corte transversal en estado estacionario (Tick 2000)
df_ss <- df_clean %>%
  filter(step == 2000)

# Lista de las 4 variables dependientes relacionadas con innovación
capability_vars <- c(
  "firms.with.commercial.capability",
  "firms.with.digital.capability",
  "firms.with.ai.capability",
  "mean.capability.count"
)

# Bucle para ejecutar ANOVA y Eta Cuadrado Parcial para cada variable
for (v in capability_vars) {
  cat("\n=======================================================\n")
  cat(" ANÁLISIS PARA LA VARIABLE:", v, "\n")
  cat("=======================================================\n")
  
  # Construir fórmula dinámica
  fmla <- as.formula(paste(v, "~ factor(max.triads.per.worker) * factor(credit.limit.base)"))
  
  # Ajustar ANOVA
  fit <- aov(fmla, data = df_ss)
  
  # Mostrar resumen del ANOVA
  print(summary(fit))
  
  # Calcular y mostrar tamaños de efecto (Eta cuadrado parcial)
  cat("\n-- Tamaños de efecto (Eta cuadrado parcial) --\n")
  es <- eta_squared(fit, partial = TRUE)
  print(es)
}




# ------------------------------------------------------------------
# 5. VISUALIZACIONES PARA EL MANUSCRITO (ESTÁNDAR JIK / JASSS)
# ------------------------------------------------------------------



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
      legend.position = "top",
      legend.key.width = unit(0.8, "cm"),
      legend.key.height = unit(0.25, "cm"),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      panel.grid = element_blank()
    )
}

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
    legend.position = "right",
    strip.background = element_rect(fill = "#264653"),
    strip.text = element_text(face = "bold", color = "white")
  )

print(p_evolucion_caps)

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
ggsave("F1.pdf", p_figura1_completa, width = 14, height = 10.5, units = "in", dpi = 300)


# ANOVAS AND EFFECT SIZES
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

# Distribución de variables
library(tidyverse)
library(ggridges)
library(ggtext)
library(glue)

# 1. Preparar y estructurar tus datos al formato largo
df_long <- df %>%
  select(firms.with.ai.capability, firms.with.commercial.capability, 
         firms.with.digital.capability, mean.capability.count) %>%
  pivot_longer(
    cols = everything(),
    names_to = "dimension",
    values_to = "valor"
  ) %>%
  mutate(dimension = case_when(
    dimension == "firms.with.ai.capability" ~ "AI Capability",
    dimension == "firms.with.commercial.capability" ~ "Commercial Capability",
    dimension == "firms.with.digital.capability" ~ "Digital Capability",
    dimension == "mean.capability.count" ~ "Mean Capability Count"
  ))

# Ordenar las dimensiones
df_long$dimension <- factor(df_long$dimension, levels = c(
  "Mean Capability Count",
  "Digital Capability",
  "Commercial Capability",
  "AI Capability"
))

median_global <- median(df_long$valor, na.rm = TRUE)
bg_color <- "grey97"
font_family <- "sans"

# 2. Construcción del gráfico usando ggridges + tu tema original
p <- df_long %>%
  ggplot(aes(x = valor, y = dimension)) +
  
  # Distribución en gris
  geom_density_ridges(
    fill = "grey75", 
    color = "grey40",     # Borde sutil para definir la silueta
    alpha = 0.6,
    scale = 0.9,          # Permite que las montañas se superpongan ligeramente
    rel_min_height = 0.01 # Recorta las colas largas y planas a cero
  ) +
  
  # Punto de la mediana
  stat_summary(
    geom = "point", 
    fun = median, 
    color = "black", 
    size = 2, 
    na.rm = TRUE
  ) +
  
  # Línea vertical de la mediana global
  geom_vline(
    xintercept = median_global, 
    col = "grey30", 
    linetype = "dashed", 
    linewidth = 0.5
  ) +
  
  # Anotación de la mediana global
  annotate(
    "text", 
    x = median_global + 0.6, 
    y = 4.2, 
    label = "Global median",
    family = font_family, 
    size = 3, 
    hjust = 0, 
    color = "grey30"
  ) +
  
  scale_x_continuous(breaks = seq(0, 20, 5), limits = c(-1, 20)) +
  
  labs(
    title = toupper("Exploratory Evaluation: Distribution of Capabilities"),
    subtitle = glue("Comparison of densities and medians.<br>
Useful for assessing normality assumptions versus overdispersion (Negative Binomial)."),
    x = "Observed Value",
    y = NULL
  ) +
  
  # Aplicar el tema de Ansgar Wolsing
  theme_minimal(base_family = font_family) +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.1, color = "grey75"),
    plot.title = element_text(face = "bold", size = 13, family = font_family),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(margin = margin(t = 4, b = 16), size = 10),
    plot.caption = element_textbox_simple(margin = margin(t = 12), size = 7),
    plot.caption.position = "plot",
    axis.text.y = element_text(hjust = 0, face = "bold", size = 11, color = "black"),
    plot.margin = margin(12, 12, 12, 12)
  )

print(p)

library(psych)
describe.by(df_long$valor, group = df_long$dimension, mat = TRUE, digits = 3)


# GAMLSS
library(gamlss)
library(gamlss.util)
library(viridis)

# Asegurar datos limpios en estado estacionario (t = 2000)
df_step2000 <- df_clean %>% filter(step == 2000)

# Lista de variables de innovación a modelar
innovation_vars <- c(
  "firms.with.commercial.capability",
  "firms.with.digital.capability",
  "firms.with.ai.capability",
  "mean.capability.count"
)

# Bucle para ajustar GAMLSS y graficar la heteroscedasticidad condicional
for (y_var in innovation_vars) {
  cat("\nAjustando GAMLSS para la variable:", y_var, "\n")
  
  # Fórmula simplificada con suavizado penalizado para el predictor continuo x
  f_mu <- as.formula(paste(y_var, "~ pb(credit.limit.base)"))
  
  # Ajustar el modelo GAMLSS usando familia Normal (NO) para admitir ceros
  modelo_gamlss <- gamlss(
    formula = f_mu,
    sigma.formula = ~ pb(credit.limit.base),
    family = NO,
    data = df_step2000,
    trace = FALSE
  )
  
  # Definir límites del eje Y dinámicos
  y_lim <- range(df_step2000[[y_var]], na.rm = TRUE)
  
  # Generar el plot usando do.call
  dev.new() 
  do.call(plotSimpleGamlss, list(
    x = quote(credit.limit.base),
    y = as.name(y_var),
    model = modelo_gamlss,
    data = quote(df_step2000),
    pch = 1,
    x.val = seq(min(df_step2000$credit.limit.base, na.rm = TRUE), 
                max(df_step2000$credit.limit.base, na.rm = TRUE), length.out = 6),
    val = 2000,
    N = 500,
    ylim = y_lim,
    xlim = range(df_step2000$credit.limit.base, na.rm = TRUE),
    cols = viridis_pal(begin = 0.3, end = 0.95, alpha = 0.4, option = "D", direction = 1)(100)
  ))
  
  title(main = paste("GAMLSS Conditional Distribution:", y_var))
}

library(gamlss)

# Asegurar datos limpios en estado estacionario (t = 2000)
df_step2000 <- df_clean %>% filter(step == 2000)

# Lista de variables de innovación
innovation_vars <- c(
  "firms.with.commercial.capability",
  "firms.with.digital.capability",
  "firms.with.ai.capability",
  "mean.capability.count"
)

# Lista para almacenar los modelos ajustados
gamlss_models <- list()

# Bucle para calcular GAMLSS con family = TF() para cada variable de innovación
for (y_var in innovation_vars) {
  cat("\n=======================================================\n")
  cat(" GAMLSS MODEL FOR:", y_var, "\n")
  cat("=======================================================\n")
  
  # Construir fórmula dinámica
  f_mu <- as.formula(paste(y_var, "~ pb(credit.limit.base)"))
  
  # Ajustar el modelo GAMLSS
  modelo_gamlss <- gamlss(
    formula = f_mu,
    sigma.formula = ~ pb(credit.limit.base),
    family = TF(),
    data = df_step2000,
    trace = FALSE
  )
  
  # Guardar modelo en la lista
  gamlss_models[[y_var]] <- modelo_gamlss
  
  # Imprimir resumen del modelo
  print(summary(modelo_gamlss))
}
