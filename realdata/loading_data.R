library(readxl)
library(readr)

# Function for processing each dataset
process_dataset <- function(file_name) {
  dataset <- read_excel(file_name, col_names = FALSE)
  colnames(dataset) <- c("Gap","k","Hustota", "Rychlost")
  dataset <- data.frame(lapply(dataset, as.numeric))
  return(dataset)
}

# Load all datasets
Nacteni_dat <- function(){
  # Process each dataset
  dataset1 <- process_dataset("data/Munich_INTSEC_01.xlsx")
  dataset2 <- process_dataset("data/Munich_INTSEC_02.xlsx")
  dataset3 <- process_dataset("data/Munich_INTSEC_03.xlsx")
  
  # Combine all datasets
  Total_data <- rbind(dataset1, dataset2, dataset3)
  
  # Return a list of all datasets
  output <- list(dataset1, dataset2, dataset3, Total_data)
  return(output)
}

# Helper function to create Gaps list
create_gaps <- function(dataset) {
  Gaps <- vector(mode = "list", length = 9)
  for(j in 1:9){
    l <- j-1
    Gaps[[j]] <- as.numeric(dataset$Gap[dataset$k == l])
  }
  return(Gaps)
}
