library(readxl)
library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(devtools)
library(fitdistrplus)
library(optimx)
library(goftest)
library(goft)
library(ggpubr)
library(rapport)
library(gsubfn)
library(KScorrect)

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
  dataset1 <- process_dataset("realdata/Munich_INTSEC_01.xlsx")
  dataset2 <- process_dataset("realdata/Munich_INTSEC_02.xlsx")
  dataset3 <- process_dataset("realdata/Munich_INTSEC_03.xlsx")
  
  # Combine all datasets
  Total_data <- rbind(dataset1, dataset2, dataset3)
  
  # Return a list of all datasets
  output <- list(dataset1, dataset2, dataset3, Total_data)
  return(output)
}

