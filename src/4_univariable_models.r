#produce models for regression analysis

# source code
source("src/1_data_cleaning.r")

#library
library(MASS)
library(pak)
library(gtsummary4mice) ## install.packages("pak") pak::pak("jrob95/gtsummary4mice")

#univariable logistic regression for ICU admission
univariable_logistic_table_complete = tbl_uvregression(
    data = model_data |>
    rename(
        MAP = `Mean arterial Pressure`,
        `Microvascular_flox_index` = `Microvascular flox index...53`,
        MRproADM = `MR-proADM`,     
        Baseline_StO2_level = `Baseline StO2 level`,
        rate_of_deoxygenation = `Rate of deoxygenation (%/sec)...48`
    ) |> 
    select(any_of(c("ICU_admission", baseline_terms, biomarkers_terms, tissue_oxygenation_terms, microvascular_function_terms, 
    "Mean arterial Pressure", "Microvascular flox index...53", "MR-proADM",
    "Baseline StO2 level", "Peak StO2 post vascular occlusion (%)"))),
    method = glm,
    y = `ICU_admission`,
    exponentiate = TRUE
)
    
#mice cleanin and imputation of missing data
model_selected_data_mice = model_data |>
    select(any_of(c("ICU_admission", baseline_terms, biomarkers_terms, tissue_oxygenation_terms, microvascular_function_terms, 
    "Mean arterial Pressure", "Microvascular flox index...53", "MR-proADM","Rate of deoxygenation (%/sec)...48",
    "Baseline StO2 level"))) |>
    rename(
        MAP = `Mean arterial Pressure`,
        `Microvascular_flox_index` = `Microvascular flox index...53`,
        rate_of_deoxygenation = `Rate of deoxygenation (%/sec)...48`,
        MRproADM = `MR-proADM`,     
        Baseline_StO2_level = `Baseline StO2 level`,
     )|> 
    mice(m = 50, maxit = 5)


univariable_logistic_table_mice = tbl_uvregression(
    data = model_selected_data_mice,
    method = glm,
    y = `ICU_admission`,
    exponentiate = TRUE
)


#save univariable logistic regression results ready for exporting as excel

univariable_logistic_table_complete = as_tibble(univariable_logistic_table_complete)
univariable_logistic_table_imputed = as_tibble(univariable_logistic_table_mice)


### Across all variables - unrestricted


#univariable logistic regression for ICU admission
univariable_logistic_table = tbl_uvregression(
    data = model_data,
    method = glm,
    y = `ICU_admission`,
    exponentiate = TRUE
)

univariable_logistic_table_unrestricted = as.tibble(univariable_logistic_table)

#univariable selection of variables from univariable_logistic_table
univariable_selection = univariable_logistic_table_complete |>
    dplyr::filter(!is.na(`**p-value**`), `**p-value**` < 0.25) |>
    dplyr::pull(`**Characteristic**`) |>
    unique()

# multivariable logistic regression for ICU admission
multivariable_data = model_data |>
    select(any_of(c("ICU_admission", univariable_selection)))

print("Univariable logistic regression completed. Variables selected for multivariable logistic regression:")

# Save variables for selection 

writexl::write_xlsx(
    list( univariable_logistic_table_complete = univariable_logistic_table_complete,
    univariable_logistic_table_imputed = univariable_logistic_table_imputed,
    univariable_logistic_table_unrestricted = univariable_logistic_table_unrestricted   
    ),
    path = "output/tables/univariable_logistic_regression_results.xlsx"
)

print("Univariable logistic regression results saved to output/tables/univariable_logistic_regression_results.xlsx")