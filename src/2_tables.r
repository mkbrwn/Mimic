#tables for demographics 

#load cleaning script
source("src/1_data_cleaning.r")

library(writexl)

data_table = data |> 
    select(where(~ !inherits(.x, "POSIXct"))) |> 
    select(-c(`Patient Initials`, `Consent Time`))


#Gtsummary table for demographics by 
table_demographics_by_admission = data_table |>
    tbl_summary(
        by = `ICU_admission`)   |> add_p()  

table_demographics_by_death = data_table |>
    tbl_summary(
        by = `Death`)  |> add_p() 

# Save as an excel file
write_xlsx(
    x = list(
        by_ICU_admission = as_tibble(table_demographics_by_admission),
        by_death = as_tibble(table_demographics_by_death)
    ),
    path = "output/tables/demographics_tables.xlsx"
)

print("Demographics tables saved to output/tables/demographics_tables.xlsx")


