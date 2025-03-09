#### Author: Stephen S, Alex M, Nayati S
#### Date today ig

#### load libraries ----

library(tidyverse)
library(readxl)

#### load data ----

local_path <- "C:/Users/sshan/Downloads/cra/"

data_list <- list.files(local_path)

fwf_96 <- c(4,10,1,4,1,1,2,3,4,4,1,1,1,3,3,6,8,6,8,6,8,6,8,6,8)
fwf_04 <- c(5,10,1,4,1,1,2,3,5,4,1,1,1,3,3,10,10,10,10,10,10,10,10,10,10)

col_names <- c("table_id",
              "resp_id",
              "agency",
              "year",
              "loan_type",
              "action_taken",
              "state",
              "county",
              "msa/md",
              "assessment_area_number",
              "partial_county_indicator",
              "split_copunty",
              "pop_class",
              "income_group",
              "report_level",
              "n_sb_loans_100k",
              "dollar_sb_loans_100k",
              "n_sb_loans_100_250k",
              "dollar_sb_loans_100_250k",
              "n_sb_loans_250k_1m",
              "dollar_sb_loans_250k_1m",
              "n_sb_loans_revenue_1m",
              "dollar_sb_loans_revenue_1m",
              "extra",
              "extra2"
              )


cra_prep_function <- function(x) {
  
  file_str <- paste0(local_path,x)
  message(file_str)
  # 1996
  if (str_sub(x, 1, 2) == "96") {
    message("1996")
    cra_data <- read_fwf(paste0(file_str,"/",x,".dat"),
                         col_positions = fwf_widths(fwf_96),
                         col_types = cols(.default = "c"))
    colnames(cra_data) <- col_names
    cra_data <- cra_data %>%
      filter(table_id == "D1-1",
             report_level == "040") %>%
      select(-c(1, 5, 6,11,12,13,14,15,22,23,24,25))
  } 
  # 1997 - 2003
  else if ( str_sub(x, 1, 2) %in% as.character(c("97", "98", "99", "00", "01", "02", "03")) ) {
    message("1997 - 2003")
    alt_lengths <- fwf_96
    alt_lengths[1] <- alt_lengths[1] + 1
    cra_data <- read_fwf(paste0(file_str,"/",x,".dat"),
                         col_positions = fwf_widths(alt_lengths),
                         col_types = cols(.default = "c"))
    colnames(cra_data) <- col_names
    cra_data <- cra_data %>%
      filter(table_id == "D1-1",
             report_level == "040") %>%
      select(-c(1, 5, 6,11,12,13,14,15,22,23,24,25))
    
  } 
  # 2004 - 2006
  else if ( str_sub(x, 1, 2) %in% as.character(c("04", "05", "06")) ) {
    message('2004 - 2007')
    # alt_lengths <- fwf_1996$Length
    # alt_lengths[1] <- alt_lengths[1] + 1
    cra_data <- read_fwf(paste0(file_str,"/",x,"_new.dat"),
                         col_positions = fwf_widths(fwf_04),
                         col_types = cols(.default = "c"))
    colnames(cra_data) <- col_names
    cra_data <- cra_data %>%
      filter(table_id == "D1-1",
             report_level == "040") %>%
      select(-c(1, 5, 6,11,12,13,14,15,22,23,24,25)) 
  }
  # 2007
  else if ( str_sub(x, 1, 2) %in% as.character(c("07")) ) {
    message('2007')
    # alt_lengths <- fwf_1996$Length
    # alt_lengths[1] <- alt_lengths[1] + 1
    cra_data <- read_fwf(paste0(file_str,"/",x,".dat"),
                         col_positions = fwf_widths(fwf_04),
                         col_types = cols(.default = "c"))
    colnames(cra_data) <- col_names
    cra_data <- cra_data %>%
      filter(table_id == "D1-1",
             report_level == "040") %>%
      select(-c(1, 5, 6,11,12,13,14,15,22,23,24,25))
  }
  # 2016 - 2024
  else if ( str_sub(x, 1, 2) %in% as.character(c(16:24)) ) {
    message("2016 - 2024")
    # alt_lengths <- fwf_1996$Length
    # alt_lengths[1] <- alt_lengths[1] + 1
    cra_data <- read_fwf(paste0(file_str,"/cra20",str_sub(x, 1, 2),"_Discl_D11.dat"),
                         col_positions = fwf_widths(fwf_04),
                         col_types = cols(.default = "c"))
    colnames(cra_data) <- col_names
    cra_data <- cra_data %>%
      filter(report_level == "040") %>%
      select(-c(1, 5, 6,11,12,13,14,15,22,23,24,25))
    
  }
  # 2008 - 2015
  else {
    message("2008 - 2015")
    # alt_lengths <- fwf_1996$Length
    # alt_lengths[1] <- alt_lengths[1] + 1
    cra_data <- read_fwf(paste0(file_str,"/exp_discl.dat"),
                         col_positions = fwf_widths(fwf_04),
                         col_types = cols(.default = "c"))
    colnames(cra_data) <- col_names
    cra_data <- cra_data %>%
      filter(table_id == "D1-1",
             report_level == "040") %>%
      select(-c(1, 5, 6,11,12,13,14,15,22,23,24,25))
  }
  
  gc()
  return(cra_data)
}

cra_data <- do.call(rbind, lapply(data_list, cra_prep_function))

cra_data_temp <- cra_data %>%
  mutate(across(.cols = c(8:13), ~ as.numeric(.x))) %>%
  mutate(
         sum_n_loans = rowSums(.[, c(8,10,12)]),
         sum_loan_amt = rowSums(.[, c(9,11,13)]))

library(data.table)

fwrite(cra_data_temp, paste0(local_path, "cra_data.csv"))



