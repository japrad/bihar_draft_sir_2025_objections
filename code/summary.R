library(tidyverse)
compiled_data <- read_csv("data/extracted_csv_files/addition/compiled.csv")


compiled_data |> nrow()
#> 10,69,535


compiled_data |> 
  filter(!(is.na(name_of_applicant) | is.na(form_reference_number))) |> 
  nrow()
#> 906,476 people with possible usable data

compiled_data |> 
  distinct(file) |> 
  nrow()
#> 1030 files

compiled_data |> 
  filter((is.na(name_of_applicant) & is.na(form_reference_number))) |>
  distinct(file) |> 
  nrow()
#> 41 empty files

compiled_data |> 
  distinct(file, page) |> 
  nrow()
#> 18001 pages 



  