library(tidyverse)

read_behaviorspace_spreadsheet <- function(
  file,
  vars_per_run = 11
) {
 
 raw <- readLines(file)
 
 # Encabezados
 headers <- strsplit(raw[16], ",")[[1]]
 headers <- gsub('"', '', headers)
 
 headers <- headers[-1]
 
 # Variables únicas
 vars <- headers[1:vars_per_run]
 
 # Datos
 data_lines <- raw[17:length(raw)]
 
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
  "Experiments/VirtEcom1.2 Experiment A_OperatingCost-spreadsheet(30).csv"
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
