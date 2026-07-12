#produce models for regression analysis

# source code
source("src/1_data_cleaning.r")

#library
library(MASS)
library(pak)
# library(gtsummary4mice) ## install.packages("pak") pak::pak("jrob95/gtsummary4mice")

#univariable logistic regression for ICU admission
univariable_logistic_table_complete = tbl_uvregression(
    data = model_data |>
    select(any_of(c("ICU_admission", baseline_terms, biomarkers_terms, tissue_oxygenation_terms, microvascular_function_terms, 
    "Mean arterial Pressure", "Microvascular flox index...53", "MR-proADM","Total vessel density (mm/mm^-2)", "Perfused vessel density",
    "Baseline StO2 level", "Peak StO2 post vascular occlusion (%)"))) |>
    rename(
        MAP = `Mean arterial Pressure`,
        `Microvascular_flox_index` = `Microvascular flox index...53`,
        MRproADM = `MR-proADM`,     
        Total_vessel_density = `Total vessel density (mm/mm^-2)`,
        Perfused_vessel_density = `Perfused vessel density`,
        Baseline_StO2_level = `Baseline StO2 level`,
        Peak_StO2_post_vascular_occlusion = `Peak StO2 post vascular occlusion (%)`
     ) ,
    method = glm,
    y = `ICU_admission`,
    exponentiate = TRUE
)
    
#mice cleanin and imputation of missing data
model_selected_data_mice = model_data |>
    select(any_of(c("ICU_admission", baseline_terms, biomarkers_terms, tissue_oxygenation_terms, microvascular_function_terms, 
    "Mean arterial Pressure", "Microvascular flox index...53", "MR-proADM","Total vessel density (mm/mm^-2)", "Perfused vessel density",
    "Baseline StO2 level", "Peak StO2 post vascular occlusion (%)"))) |>
    rename(
        MAP = `Mean arterial Pressure`,
        `Microvascular_flox_index` = `Microvascular flox index...53`,
        MRproADM = `MR-proADM`,     
        Total_vessel_density = `Total vessel density (mm/mm^-2)`,
        Perfused_vessel_density = `Perfused vessel density`,
        Baseline_StO2_level = `Baseline StO2 level`,
        Peak_StO2_post_vascular_occlusion = `Peak StO2 post vascular occlusion (%)`
     ) |> 
    mice(m = 50, maxit = 5)


    univariable_logistic_table_mice = tbl_uvregression(
    data = model_selected_data_mice,
    method = glm,
    y = `ICU_admission`,
    exponentiate = TRUE
)


#save univariable logistic regression results to excel

univariable_logistic_table_complete = as_tibble(univariable_logistic_table_complete)
univariable_logistic_table_imputed = as_tibble(univariable_logistic_table_mice)


writexl::write_xlsx(
    list( univariable_logistic_table_complete = univariable_logistic_table_complete,
    univariable_logistic_table_imputed = univariable_logistic_table_imputed),
    path = "output/tables/univariable_logistic_regression_results.xlsx"
)

#univariable selection of variables from univariable_logistic_table
univariable_selection = univariable_logistic_table_complete$table_body |>
    dplyr::filter(!is.na(p.value), p.value < 0.25) |>
    dplyr::pull(variable) |>
    unique()

# multivariable logistic regression for ICU admission
multivariable_data = model_data |>
    select(any_of(c("ICU_admission", univariable_selection)))

print("Univariable logistic regression completed. Variables selected for multivariable logistic regression:")

univariable_logistic_table_mice$table_body