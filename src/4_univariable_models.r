#produce models for regression analysis

# source code
source("src/1_data_cleaning.r")

#library
library(MASS)

#univariable logistic regression for ICU admission
univariable_logistic_table = tbl_uvregression(
    data = model_data,
    method = glm,
    y = `ICU_admission`,
    exponentiate = TRUE
)

#univariable slection of variables from univariable_logistic_table
univariable_selection = univariable_logistic_table$table_body |>
    dplyr::filter(!is.na(p.value), p.value < 0.25) |>
    dplyr::pull(variable) |>
    unique()

# multivariable logistic regression for ICU admission
muiltivariable_data = model_data |>
    select(any_of(c("ICU_admission", univariable_selection)))

print("Univariable logistic regression completed. Variables selected for multivariable logistic regression:")