library(tidyverse)


# Exp A -------------------------------------------------------------------
read_behaviorspace_spreadsheet <- function(
  file,
  vars_per_run = 11
) {
 
 raw <- readLines(file)
 
 # Encabezados
 headers <- strsplit(raw[17], ",")[[1]]
 headers <- gsub('"', '', headers)
 
 headers <- headers[-1]
 
 # Variables únicas
 vars <- headers[1:vars_per_run]
 
 # Datos
 data_lines <- raw[18:length(raw)]
 
 n_runs <- length(headers) / vars_per_run
 
 message("Runs detectados: ", n_runs)
 
 out <- vector("list", length(data_lines))
 
 for(i in seq_along(data_lines)) {
  
  row <- strsplit(data_lines[i], ",")[[1]]
  
  row <- gsub('"', '', row)
  
  row <- row[-1]
  
  mat <- matrix(
   row,
   ncol = vars_per_run,
   byrow = TRUE
  )
  
  df <- as_tibble(mat)
  
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


virtEcom_long <-
 read_behaviorspace_spreadsheet(
  "Experiments/VirtEcom1.3 Experiment_C_BuyerIncome-spreadsheet.csv"
 )

variable.names(virtEcom_long)
colnames(virtEcom_long)[1] <- "step"

library(tidyverse)

survival_curves <- virtEcom_long %>%
 group_by(
  `operating-cost`,
  step
 ) %>%
 summarise(
  sellers =
   mean(`count sellers`),
  .groups = "drop"
 )

ggplot(
 survival_curves,
 aes(
  x = step,
  y = sellers,
  color = factor(`operating-cost`)
 )
) +
 geom_line(size = 1) +
 labs(
  x = "Tick",
  y = "Mean Sellers Alive",
  color = "Operating Cost"
 ) +
 theme_minimal()

debt_curves <- virtEcom_long %>%
 group_by(
  `operating-cost`,
  step
 ) %>%
 summarise(
  debt = mean(`mean-debt`, na.rm = TRUE),
  .groups = "drop"
 )

ggplot(
 debt_curves,
 aes(
  x = step,
  y = debt,
  color = factor(`operating-cost`)
 )
) +
 geom_line(linewidth = 1) +
 labs(
  title = "Average Debt Accumulation",
  x = "Tick",
  y = "Average Debt",
  color = "Operating Cost"
 ) +
 theme_minimal(base_size = 14)


budget_curves <- virtEcom_long %>%
 group_by(
  `operating-cost`,
  step
 ) %>%
 summarise(
  budget = mean(`mean-buyer-budget`, na.rm = TRUE),
  .groups = "drop"
 )

ggplot(
 budget_curves,
 aes(
  x = step,
  y = budget,
  color = factor(`operating-cost`)
 )
) +
 geom_line(linewidth = 1) +
 labs(
  title = "Average Buyer Budget",
  x = "Tick",
  y = "Average Buyer Budget",
  color = "Operating Cost"
 ) +
 theme_minimal(base_size = 14)

cash_curves <- virtEcom_long %>%
 group_by(
  `operating-cost`,
  step
 ) %>%
 summarise(
  cash = mean(`mean-cash`, na.rm = TRUE),
  .groups = "drop"
 )

ggplot(
 cash_curves,
 aes(
  x = step,
  y = cash,
  color = factor(`operating-cost`)
 )
) +
 geom_line(linewidth = 1.2) +
 labs(
  title = "Average Seller Cash",
  x = "Tick",
  y = "Average Cash",
  color = "Operating Cost"
 ) +
 theme_minimal(base_size = 14)




# Exp C -------------------------------------------------------------------


read_behaviorspace_spreadsheet <- function(
    file,
    vars_per_run = 12
) {
  
  raw <- readLines(file)
  
  # Encabezados
  headers <- strsplit(raw[17], ",")[[1]]
  headers <- gsub('"', '', headers)
  
  headers <- headers[-1]
  
  # Variables únicas
  vars <- headers[1:vars_per_run]
  
  # Datos
  data_lines <- raw[18:length(raw)]
  
  n_runs <- length(headers) / vars_per_run
  
  message("Runs detectados: ", n_runs)
  
  out <- vector("list", length(data_lines))
  
  for(i in seq_along(data_lines)) {
    
    row <- strsplit(data_lines[i], ",")[[1]]
    
    row <- gsub('"', '', row)
    
    row <- row[-1]
    
    mat <- matrix(
      row,
      ncol = vars_per_run,
      byrow = TRUE
    )
    
    df <- as_tibble(mat)
    
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

virtEcom_long <-
  read_behaviorspace_spreadsheet(
    "Experiments/VirtEcom1.3 Experiment_C_BuyerIncome-spreadsheet.csv"
  )

library(tidyverse)

survival_curves <- virtEcom_long %>%
 group_by(
  `buyer-income`,
  step
 ) %>%
 summarise(
  sellers = mean(`count sellers`),
  .groups = "drop"
 )

ggplot(
 survival_curves,
 aes(
  x = step,
  y = sellers,
  color = factor(`buyer-income`)
 )
) +
 geom_line(linewidth = 1) +
 labs(
  title = "Seller Survival by Buyer Income",
  x = "Tick",
  y = "Mean Sellers Alive",
  color = "Buyer Income"
 ) +
 theme_minimal(base_size = 14)
￼
2. Deuda
debt_curves <- virtEcom_long %>%
 group_by(
  `buyer-income`,
  step
 ) %>%
 summarise(
  debt = mean(`mean-debt`, na.rm = TRUE),
  .groups = "drop"
 )

ggplot(
 debt_curves,
 aes(
  x = step,
  y = debt,
  color = factor(`buyer-income`)
 )
) +
 geom_line(linewidth = 1) +
 labs(
  title = "Average Debt Accumulation by Buyer Income",
  x = "Tick",
  y = "Average Debt",
  color = "Buyer Income"
 ) +
 theme_minimal(base_size = 14)
￼
3. Presupuesto de compradores
Esta probablemente será la gráfica más importante del Experimento C.
budget_curves <- virtEcom_long %>%
 group_by(
  `buyer-income`,
  step
 ) %>%
 summarise(
  budget = mean(`mean-buyer-budget`, na.rm = TRUE),
  .groups = "drop"
 )

ggplot(
 budget_curves,
 aes(
  x = step,
  y = budget,
  color = factor(`buyer-income`)
 )
) +
 geom_line(linewidth = 1) +
 labs(
  title = "Average Buyer Budget by Buyer Income",
  x = "Tick",
  y = "Average Buyer Budget",
  color = "Buyer Income"
 ) +
 theme_minimal(base_size = 14)
￼
4. Cash
cash_curves <- virtEcom_long %>%
 group_by(
  `buyer-income`,
  step
 ) %>%
 summarise(
  cash = mean(`mean-cash`, na.rm = TRUE),
  .groups = "drop"
 )

ggplot(
 cash_curves,
 aes(
  x = step,
  y = cash,
  color = factor(`buyer-income`)
 )
) +
 geom_line(linewidth = 1.2) +
 labs(
  title = "Average Seller Cash by Buyer Income",
  x = "Tick",
  y = "Average Cash",
  color = "Buyer Income"
 ) +
 theme_minimal(base_size = 14)
￼
5. Gráfica que NO teníamos en A ni B (recomendada)
Ahora sí tiene sentido graficar directamente el efecto final de buyer-income.
Supervivencia final
final_survival <- virtEcom_long %>%
 filter(step == max(step)) %>%
 group_by(`buyer-income`) %>%
 summarise(
  sellers_alive = mean(`count sellers`),
  .groups = "drop"
 )

ggplot(
 final_survival,
 aes(
  x = factor(`buyer-income`),
  y = sellers_alive
 )
) +
 geom_col(fill = "steelblue") +
 labs(
  title = "Final Seller Survival",
  x = "Buyer Income",
  y = "Mean Sellers Alive at Tick 1000"
 ) +
 theme_minimal(base_size = 14)