# Exp F -------------------------------------------------------------------
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


# Exp E -------------------------------------------------------------------


ExpE <-
 read_behaviorspace_spreadsheet(
  "Experiments/VirtEcom1.5 Experiment_E_CostlyLearning-spreadsheet.csv"
 )
variable.names(ExpE)
colnames(ExpE)[1] <- "step"
ExpE$Model <- "1.5"

ExpF <-
 read_behaviorspace_spreadsheet(
  "Experiments/VirtEcom1.6 Experiment_F_CostlyLearning-spreadsheet.csv"
 )
variable.names(ExpF)
colnames(ExpF)[1] <- "step"
ExpF$Model <- "1.6"

variable.names(ExpE) == variable.names(ExpF)

Comparison <- bind_rows(
 ExpE,
 ExpF
)
variable.names(Comparison)

library(tidyverse)

complexity_curves <- Comparison %>%
 group_by(
  Model,
  step
 ) %>%
 summarise(
  complexity =
   mean(`mean-complexity`),
  .groups = "drop"
 )

ggplot(
 complexity_curves,
 aes(
  x = step,
  y = complexity,
  color = Model
 )
) +
 geom_line(linewidth = 1.2) +
 labs(
  title = "Complexity Dynamics",
  x = "Tick",
  y = "Mean Complexity"
 ) +
 theme_minimal(base_size = 14)

population_curves <- Comparison %>%
 group_by(
  Model,
  step
 ) %>%
 summarise(
  population =
   mean(`seller-population`),
  .groups = "drop"
 )

ggplot(
 population_curves,
 aes(
  step,
  population,
  color = Model
 )
) +
 geom_line(linewidth = 1.2) +
 labs(
  title = "Seller Population",
  x = "Tick",
  y = "Population"
 ) +
 theme_minimal(base_size = 14)

entry_curves <- Comparison %>%
 group_by(
  Model,
  step
 ) %>%
 summarise(
  entrants =
   mean(entrants),
  .groups = "drop"
 )

ggplot(
 entry_curves,
 aes(
  step,
  entrants,
  color = Model
 )
) +
 geom_line(linewidth = 1.2) +
 labs(
  title = "Entrepreneurial Entry",
  x = "Tick",
  y = "Cumulative Entrants"
 ) +
 theme_minimal(base_size = 14)

cash_curves <- Comparison %>%
 group_by(
  Model,
  step
 ) %>%
 summarise(
  cash =
   mean(`mean-cash`),
  .groups = "drop"
 )

ggplot(
 cash_curves,
 aes(
  step,
  cash,
  color = Model
 )
) +
 geom_line(linewidth = 1.2) +
 labs(
  title = "Average Cash",
  x = "Tick",
  y = "Mean Cash"
 ) +
 theme_minimal(base_size = 14)

debt_curves <- Comparison %>%
 group_by(
  Model,
  step
 ) %>%
 summarise(
  debt =
   mean(`mean-debt`),
  .groups = "drop"
 )

ggplot(
 debt_curves,
 aes(
  step,
  debt,
  color = Model
 )
) +
 geom_line(linewidth = 1.2) +
 labs(
  title = "Average Debt",
  x = "Tick",
  y = "Mean Debt"
 ) +
 theme_minimal(base_size = 14)

final_complexity <- Comparison %>%
 filter(step == max(step)) %>%
 group_by(Model) %>%
 summarise(
  complexity =
   mean(`mean-complexity`),
  .groups = "drop"
 )

ggplot(
 final_complexity,
 aes(
  x = Model,
  y = complexity
 )
) +
 geom_col(fill = "steelblue") +
 labs(
  title = "Final Complexity",
  x = "",
  y = "Mean Complexity"
 ) +
 theme_minimal(base_size = 14)

# Exp1: Cognitive Constraint Sensitivity ----------------------------------

Exp1 <- read_behaviorspace_spreadsheet(
  "Experiments/VirtEcom2.1 Exp1_Cognitive_Constraint_Sensitivity-spreadsheet.csv"
)
variable.names(Exp1)

# 1. Limpieza y estandarización de nombres de variables
exp1_clean <- Exp1 %>%
  rename(
    step = `[step]`,
    max_triads = `max-triads-per-worker`,
    mean_ck = `mean-firm-ck`,
    gini_capital = `gini-index-of-firm-capital`,
    n_firms = `count firms`,
    n_products = `count products`,
    total_exits = `total-exits`,
    total_capital = `sum [ max list 0 capital ] of firms`
  ) %>%
  mutate(
    # Recodificar 999 como Infinito para las gráficas
    max_triads_label = factor(
      max_triads,
      levels = c(1, 2, 3, 5, 8, 999),
      labels = c("1", "2", "3", "5", "8", "Sin límite (999)")
    )
  )

# 2. Resumen estadístico al estado estacionario (Tick 2000)
exp1_summary_t2000 <- exp1_clean %>%
  filter(step == 2000) %>%
  group_by(max_triads_label) %>%
  summarise(
    mean_ck_avg = mean(mean_ck, na.rm = TRUE),
    mean_ck_sd  = sd(mean_ck, na.rm = TRUE),
    gini_avg    = mean(gini_capital, na.rm = TRUE),
    firms_avg   = mean(n_firms, na.rm = TRUE),
    exits_avg   = mean(total_exits, na.rm = TRUE),
    runs        = n()
  )

print(exp1_summary_t2000)

# 3. Gráfica 1: Trayectoria temporal del CK promedio por restricción cognitiva
ggplot(exp1_clean, aes(x = step, y = mean_ck, color = max_triads_label, group = run)) +
  geom_line(alpha = 0.3) +
  stat_summary(aes(group = max_triads_label), fun = mean, geom = "line", size = 1.2) +
  labs(
    title = "VirtEcom 2.1: Sensibilidad del Know-How Colectivo (CK)",
    subtitle = "Efecto del límite de tríadas cognitivas por trabajador (max-triads-per-worker)",
    x = "Ticks (Tiempo de Simulación)",
    y = "Mean CK (Conocimiento Colectivo Acumulado)",
    color = "Límite Tríadas"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 4. Gráfica 2: Evaluación del umbral crítico al final de la corrida (Boxplot)
ggplot(filter(exp1_clean, step == 2000), aes(x = max_triads_label, y = mean_ck, fill = max_triads_label)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  labs(
    title = "Efecto de la Restricción Cognitiva sobre la Inflación del CK (Tick 2000)",
    x = "Capacidad Máxima de Tríadas por Trabajador",
    y = "Mean CK Final"
  ) +
  theme_minimal() +
  guides(fill = "none")
