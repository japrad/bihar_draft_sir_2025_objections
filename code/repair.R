library(tidyverse)
compiled_data <- read_csv("data/extracted_csv_files/addition/compiled.csv")

# Remove empty files ------------------------------------------------------

compiled_data |> 
  filter(is.na(name_of_applicant) & is.na(form_reference_number)) |> 
  nrow()
#> 122901

# compiled_data |> 
#   filter(is.na(name_of_applicant) & is.na(form_reference_number)) |> 
#   slice_sample(n = 200) |> View()


compiled_data |> 
  filter(!(is.na(name_of_applicant) & is.na(form_reference_number))) -> data

# Store empty file names + district ---------------------------------------

compiled_data |> 
  filter((is.na(name_of_applicant) & is.na(form_reference_number))) |> 
  distinct(file, district) -> empty_files

write_csv(empty_files, "data/repair/empty_files.csv")

rm(compiled_data)


# Add row ID --------------------------------------------------------------

data |> 
  mutate(row_id = 1:nrow(data), .before = district) -> data


# Fix NA Form Reference Number --------------------------------------------

data |> 
  filter(is.na(form_reference_number)) |> 
  nrow()
#> 40158


# data |> 
#   filter(is.na(form_reference_number)) |> 
#   slice_sample(n = 200) |> View()


## Gender in Relative Name -------------------------------------------------


### Exploration -------------------------------------------------------------


data |> 
  filter(is.na(form_reference_number)) |> 
  filter(str_to_lower(name_of_relative) %in% c("m", "f", "t", "male", "female", "third")) |> 
  nrow()
#> 39706

# data |> 
#   filter(is.na(form_reference_number)) |> 
#   filter(str_to_lower(name_of_relative) %in% c("m", "f", "t", "male", "female", "third")) |> 
#   slice_sample(n = 200) |> View()


### Correction --------------------------------------------------------------


data |> 
  filter(is.na(form_reference_number)) |> 
  filter(str_to_lower(name_of_relative) %in% c("m", "f", "t", "male", "female", "third")) |> 
  rowwise() |> 
  mutate(form_reference_number = name_of_applicant,
         
         name_of_applicant = paste(
           c(gender, date_of_birth)[!is.na(c(gender, date_of_birth))], # does not paste NA
           collapse = " "
           ),
         
         gender = name_of_relative,
         date_of_birth = status_of_form,
         name_of_relative = NA
         ) -> repaired_na_reference_relative_gender

data |> 
  rows_update(repaired_na_reference_relative_gender, by = "row_id") -> data

rm(repaired_na_reference_relative_gender)


### Testing -----------------------------------------------------------------

data |> 
  filter(is.na(form_reference_number)) |> 
  filter(str_to_lower(name_of_relative) %in% c("m", "f", "t", "male", "female", "third")) |> 
  nrow()
#> 0

## Remaining  --------------------------------------------------------------


data |> 
  filter(is.na(form_reference_number)) |> 
  nrow()

# data |> 
#   filter(is.na(form_reference_number)) |> 
#   slice_sample(n = 200) |> View()

data |> 
  filter(is.na(form_reference_number)) |> 
  distinct(file)
#> S04A196_Addition_11.pdf

# All these come from a single file. Preserving as gender is available


# Names with Numbers ------------------------------------------------------

data |> 
  filter(grepl("[0-9]", name_of_applicant)) |> 
  nrow()
#> 32692

# data |> 
#   filter(grepl("[0-9]", name_of_applicant)) |> 
#   slice_sample(n = 200) |> View()



## Gender in Relative Name -------------------------------------------------


### Exploration -------------------------------------------------------------

data |> 
  filter(grepl("[0-9]", name_of_applicant)) |> 
  filter(str_to_lower(name_of_relative) %in% c("m", "f", "t", "male", "female", "third")) |> 
  nrow()
#> 32666

# data |> 
#   filter(grepl("[0-9]", name_of_applicant)) |> 
#   filter(str_to_lower(name_of_relative) %in% c("m", "f", "t", "male", "female", "third")) |> 
#   slice_sample(n = 200) |> View()


### Correction --------------------------------------------------------------

data |> 
  filter(grepl("[0-9]", name_of_applicant)) |> 
  filter(str_to_lower(name_of_relative) %in% c("m", "f", "t", "male", "female", "third")) |> 
  rowwise() |> 
  mutate(form_reference_number = name_of_applicant,
         
         name_of_applicant = paste(
           c(gender, date_of_birth)[!is.na(c(gender, date_of_birth))], # does not paste NA
           collapse = " "
         ),
         
         gender = name_of_relative,
         date_of_birth = status_of_form,
         name_of_relative = NA
  ) -> repaired_number_name_relative_gender

data |> 
  rows_update(repaired_number_name_relative_gender, by = "row_id") -> data

rm(repaired_number_name_relative_gender)


### Testing -----------------------------------------------------------------

data |> 
  filter(grepl("[0-9]", name_of_applicant)) |> 
  filter(str_to_lower(name_of_relative) %in% c("m", "f", "t", "male", "female", "third")) |> 
  nrow()
#> 0


## Remaining  --------------------------------------------------------------


### Exploration -------------------------------------------------------------

data |> 
  filter(grepl("[0-9]", name_of_applicant)) |>
  nrow()
#> 28

# data |> 
#   filter(grepl("[0-9]", name_of_applicant)) |>
#   View()


### Correction --------------------------------------------------------------

data |> 
  mutate(name_of_applicant = gsub("[0-9]", "", name_of_applicant)) -> repaired_number_name

data |> 
  rows_update(repaired_number_name, by = "row_id") -> data

rm(repaired_number_name)


### Testing -----------------------------------------------------------------

data |> 
  filter(grepl("[0-9]", name_of_applicant)) |>
  nrow()
#> 0



# Gender not Proper -------------------------------------------------------

data |> 
  filter(!(str_to_lower(gender) %in% c("male", "female", "m", "f", "third", "t"))) |> 
  nrow()
#> 302

# data |> 
#   filter(!(str_to_lower(gender) %in% c("male", "female", "m", "f", "third", "t"))) |> 
#   slice_sample(n = 200) |> View()


## Gender in Name ----------------------------------------------------------


### Exploration -------------------------------------------------------------


data |> 
  filter(!(str_to_lower(gender) %in% c("male", "female", "m", "f", "third", "t"))) |>
  filter(grepl("Male", name_of_applicant) | 
           grepl("Female", name_of_applicant) |
           grepl("Third", name_of_applicant)) |>
  nrow()
#> 287

# data |> 
#   filter(!(str_to_lower(gender) %in% c("male", "female", "m", "f", "third", "t"))) |>
#   filter(grepl("Male", name_of_applicant) | 
#            grepl("Female", name_of_applicant) |
#            grepl("Third", name_of_applicant)) |>
#   slice_sample(n = 200) |> View()


### Correction --------------------------------------------------------------

data |> 
  filter(!(str_to_lower(gender) %in% c("male", "female", "m", "f", "third", "t"))) |>
  mutate(gender = case_when(
    grepl("Male", name_of_applicant) ~ "Male",
    grepl("Female", name_of_applicant) ~ "Female",
    grepl("Third", name_of_applicant) ~ "Third"
  )) |> 
  mutate(name_of_applicant = case_when(
    grepl("Male", name_of_applicant) ~ gsub("Male", "", name_of_applicant),
    grepl("Female", name_of_applicant) ~ gsub("Female", "", name_of_applicant),
    grepl("Third", name_of_applicant) ~ gsub("Third", "", name_of_applicant)
  )) -> repaired_gender_in_name

data |> 
  rows_update(repaired_gender_in_name) -> data

rm(repaired_gender_in_name)
  

### Testing -----------------------------------------------------------------

data |> 
  filter(!(str_to_lower(gender) %in% c("male", "female", "m", "f", "third", "t"))) |>
  filter(grepl("Male", name_of_applicant) | 
           grepl("Female", name_of_applicant) |
           grepl("Third", name_of_applicant)) |>
  nrow()
#> 0

## Remaining ---------------------------------------------------------------

### Exploration -------------------------------------------------------------

data |> 
  filter(!(str_to_lower(gender) %in% c("male", "female", "m", "f", "third", "t"))) |> 
  nrow()
#> 15

# data |> 
#   filter(!(str_to_lower(gender) %in% c("male", "female", "m", "f", "third", "t"))) |> 
#   View()
# All except 1 come from file S04A239_Addition_12.pdf

# data |> 
#   filter(file == "S04A239_Addition_12.pdf") |> 
#   filter(!(str_to_lower(gender) %in% c("male", "female", "m", "f", "third", "t"))) |> 
#   arrange(page, serial) |> 
#   View()

# Two rows have been merged while extraction. No specific pattern 

### Correction --------------------------------------------------------------

data |> 
  filter(file == "S04A239_Addition_12.pdf") |> 
  filter(!(str_to_lower(gender) %in% c("male", "female", "m", "f", "third", "t"))) |> 
  mutate(form_reference_number = paste0("intentional_duplicate_", form_reference_number)) -> intentional_duplicates

data |> 
  add_row(intentional_duplicates) -> data

rm(intentional_duplicates)


### Testing -----------------------------------------------------------------

data |> 
  filter(!(str_to_lower(gender) %in% c("male", "female", "m", "f", "third", "t"))) |> 
  nrow()
#> 29

# data |> 
#   filter(!(str_to_lower(gender) %in% c("male", "female", "m", "f", "third", "t"))) |> 
#   View()
# All from file S04A239_Addition_12.pdf duplicated



# Removing duplicate rows -------------------------------------------------

data |> 
  group_by(district, date_of_receipt, form_reference_number, name_of_applicant, gender, date_of_birth, name_of_relative, address_of_applicant, status_of_form) |> 
  slice(1) |> 
  arrange(row_id) -> unique_data

data |> 
  filter(!(row_id %in% pull(unique_data, row_id))) -> duplicate_data

write_csv(duplicate_data, "data/repair/duplicate_data.csv" )
rm(duplicate_data)

data <- 
  unique_data |> 
  ungroup()
rm(unique_data)


# Making Data Uniform -----------------------------------------------------

data |> 
  mutate(across(c(name_of_applicant, gender, name_of_relative, address_of_applicant, status_of_form), str_to_lower)) |> 
  mutate(gender = case_when(
    gender %in% c("third", "t") ~ "third",
    gender %in% c("female", "f") ~ "female",
    gender %in% c("male", "m") ~ "male"
  )) -> data


write_csv(select(data, -row_id), "data/addition_data.csv" )


