library(tidyverse)

district_files <- list.files(path = "data/addition_csv/district_wise")

compiled_data <- tibble(
  district = character(0),
  file = character(0),
  page = integer(0),
  serial = character(0),
  date_of_receipt = character(0),
  form_reference_number = character(0),
  name_of_applicant = character(0),
  gender = character(0),
  date_of_birth = character(0),
  name_of_relative = character(0),
  address_of_applicant = character(0),
  status_of_form = character(0)
)

sapply(district_files, function(x) {
  data <- read_csv(paste0("output_csv_files/addition/", x)) |> 
    mutate(serial = as.character(serial)) |> 
    mutate(district = gsub(".csv", "", x))
  
  compiled_data |> 
    add_row(data) ->> compiled_data
  
  rm(data)
})

write_excel_csv(compiled_data, paste0("output_csv_files/addition/compiled.csv"))

