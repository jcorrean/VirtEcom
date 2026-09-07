library(tidyverse)

read_behaviorspace_spreadsheet <- function(
  file,
  vars_per_run = 10
) {
 
 #----------------------------------------
 # Leer archivo completo
 #----------------------------------------
 
 raw <- readLines(file)
 
 #----------------------------------------
 # Encabezados de variables
 #----------------------------------------
 
 headers <- strsplit(raw[16], ",")[[1]]
 
 headers <- gsub('"', '', headers)
 
 headers <- headers[-1]
 
 #----------------------------------------
 # Datos
 #----------------------------------------
 
 data_lines <- raw[17:length(raw)]
 
 #----------------------------------------
 # Número de corridas
 #----------------------------------------
 
 n_runs <- length(headers) / vars_per_run
 
 message("Runs detectados: ", n_runs)
 
 #----------------------------------------
 # Parsear cada tick
 #----------------------------------------
 
 ticks <- vector("list", length(data_lines))
 
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
  
  names(df) <- headers[1:vars_per_run]
  
  df$run <- seq_len(n_runs)
  
  ticks[[i]] <- df
  
 }
 
 #----------------------------------------
 # Combinar todos los ticks
 #----------------------------------------
 
 out <- bind_rows(ticks)
 
 #----------------------------------------
 # Convertir numéricos
 #----------------------------------------
 
 out <- out %>%
  mutate(
   across(
    everything(),
    ~ suppressWarnings(as.numeric(.))
   )
  )
 
 return(out)
 
}


virtEcom_long <-
 read_behaviorspace_spreadsheet(
  "Experiments/VirtEcom1.2 Experiment A_OperatingCost-spreadsheet(30).csv"
 )

virtEcom_long %>%
 group_by(
  operating_cost,
  step
 ) %>%
 summarise(
  sellers =
   mean(count_sellers)
 )
