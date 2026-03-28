district_name <- "muzaffarpur" #! manually change district name
library(tidyverse)
library(janitor)
library(tabulapdf)

test_item = NULL # for testing local variables. remove later


# Set directories & files -------------------------------------------------

# Working directory is set to root directory 

input_directory <- paste0("data/input_pdf_files/", district_name)
output_file <- paste0("data/extracted_csv_files/addition", district_name, ".csv")

addition_files <- list.files(path = input_directory, pattern = "_Addition_")

# Initiate scraping -------------------------------------------------------


## If some files from district already scraped -----------------------------


if (file.exists(output_file)) {
  merged_table <- read.csv(output_file) |>
    mutate(serial = as.character(serial))
  files_already_read <- merged_table |>
    distinct(file) |>
    pull(file)
  
  
  ## If district never scraped -----------------------------------------------
  
  
} else {
  merged_table <- tibble(
    date_of_receipt = character(0),
    form_reference_number = character(0),
    name_of_applicant = character(0),
    gender = character(0),
    date_of_birth = character(0),
    name_of_relative = character(0),
    address_of_applicant = character(0),
    status_of_form = character(0),
    file = character(0),
    page = integer(0),
    serial = character(0)
  )
  files_already_read <- character(0)
}



# File wise vectorization -------------------------------------------------


sapply(addition_files[!addition_files %in% files_already_read], function(x){
  
  file_name <- x
  print(file_name)
  file_path <- paste0(input_directory, "/", file_name)
  
  pdf_tables <- file_path |>
    extract_tables(output = "tibble", method = "stream", area = list(c(38.991, 38.99, 1619.446, 1152.155)), guess = F)
  
  
  
  ## Page wise vectorization -------------------------------------------------
  
  
  sapply(1:length(pdf_tables), function(x) {
    page_num <- x
    
    cat(file_name, page_num, "\n")
    
    current_page <- pdf_tables[[x]]
    
    
    if (!is.character(current_page)){ # Runs only if page not empty
      
      current_page |>
        clean_names() |>
        rename(serial = any_of(c("s", "s_no","s_date_of" )),
               form_reference_number = any_of(c("form_reference_number", "form_reference")),
               date_of_receipt = any_of(c("date_of", "date_of_receipt", "date_of_2")),
               date_of_birth = any_of(c("date_of_birth_of", "date_of_birth", "date_of_birth_of_applicant", "date_of_6")),
               name_of_applicant = any_of(c("name_of_applicant", "name_of", "name_of_applicant_gender", "name_of_gender")),
               name_of_relative = any_of(c("name_of_relative", "name_of_relative_relationship", "name_of_relative_relationship_type","name_of_relative_address_of_applicant")),
               address_of_applicant = any_of(c("address_of_applicant", "address_of_applicant_status_of_form", "address_of_applicant_status_of_form_as", "address_of_applicant_status_of", "address_of")),
               status_of_form = any_of(c("status_of_form_as", "status_of_form", "status_of", "status_of_form_as_on_date", "status_of_form_as_on"))
        ) |>
        filter(!serial == "No." | is.na(serial)) -> cleaned_page
      
      cleaned_page |>
        mutate(across(everything(), as.character)) -> cleaned_page
      
      if ("x5" %in% colnames(cleaned_page)) {
        cleaned_page |>
          select(-x5) -> cleaned_page
      }
      
      if ("x8" %in% colnames(cleaned_page)) {
        cleaned_page |>
          select(-x8) -> cleaned_page
      }
      
      if("relationship_type" %in% colnames(cleaned_page)){
        cleaned_page |>
          select(-relationship_type) -> cleaned_page
      }
      
      if("relationship" %in% colnames(cleaned_page)){
        cleaned_page |>
          select(-relationship) -> cleaned_page
      }
      
      
      cleaned_page |>
        mutate(index = 1:nrow(cleaned_page)) -> cleaned_page
      
      cols_na_merge <- colnames(cleaned_page |> select(-serial, -index))
      
      

      ### Merge overflow rows -----------------------------------------------------


      sapply(cols_na_merge, function(x){
        cleaned_page |>
          filter(is.na(serial)) |>
          filter(!is.na(!!sym(x))) |>
          pull(index) -> overflow_index
        
        col_values <- cleaned_page[[x]]
        
        cleaned_page |>
          mutate(!!sym(x) :=  ifelse(index %in% (overflow_index - 1),
                                     paste(!!sym(x), col_values[index + 1]),
                                     !!sym(x))) ->> cleaned_page
      })
      
      cleaned_page |>
        filter(!is.na(serial)) |>
        select(-index) |>
        mutate(file = file_name, page = page_num) -> cleaned_page

      merged_table |>
        add_row(cleaned_page) ->> merged_table
    }
    
  }
  )
  
  
  ## Write to csv once whole file scraped ------------------------------------
  
  
  write_excel_csv(merged_table, output_file)
})
