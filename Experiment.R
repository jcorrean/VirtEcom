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
