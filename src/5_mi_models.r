#library 
library(mice)
library(visdat)
library(MASS)
library(psfmi)

#source
source("src/4_univariable_models.r")
source("src/0_functions.r") # see functions for mark_mi_selection function

# list of variables for use in models 
    #baseline variables 
    baseline_terms = c("SpO2", "RR", "HR", "MAP", "Temperature", "GCS")
    baseline_formula = paste(sprintf("`%s`", baseline_terms), collapse = " + ")
    
    #biomarkers 
    biomarkers_terms = c("CRP", "Lactate", "HR", "Procalcitonin", "MRproADM")
    biomarkers_formula = paste(sprintf("`%s`", biomarkers_terms), collapse = " + ")
    
    #tissue oxygenation 
    tissue_oxygenation_terms = c("Baseline_StO2_level", "Peak_StO2_post_vascular_occlusion")
    tissue_oxygenation_formula = paste(sprintf("%s", tissue_oxygenation_terms), collapse = " + ")

    #microvascular function
    microvascular_function_terms = c("Microvascular_flox_index", "Total_vessel_density", "Perfused_vessel_density")
    microvascular_function_formula = paste(sprintf("`%s`", microvascular_function_terms), collapse = " + ")

#mice cleanin and imputation of missing data
model_selected_data = model_data |>
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

# Models 
    # basline_model 
    baseline_model = glm(as.formula(paste("ICU_admission ~", baseline_formula)), data = model_data, family = binomial())
    mark_mi_selection(model_selected_data, baseline_formula, "baseline_step_model")

    # biomarkers_model
    biomarkers_model = glm(ICU_admission ~ biomarkers_formula, data = model_data, family = binomial())
    mark_mi_selection(model_selected_data, biomarkers_formula, "biomarkers_step_model")

    # tissue oxygenation 
    tissue_oxygenation_model = glm(ICU_admission ~ tissue_oxygenation_formula, data = model_data, family = binomial())
    mark_mi_selection(model_selected_data, tissue_oxygenation_formula, "tissue_oxygenation_step_model")

    # microvascular function
    microvascular_function_model = glm(ICU_admission ~ microvascular_function_formula, data = model_data, family = binomial())
    mark_mi_selection(model_selected_data, microvascular_function_formula, "microvascular_function_step_model")

    #overall predictive model with all variables
    mark_mi_selection(model_selected_data, 
    paste(baseline_formula, biomarkers_formula, tissue_oxygenation_formula, microvascular_function_formula, sep = " + "),
    "overall_step_model")

#AUC 

overall_step_model$RR_model_final

overall_step_model$predictors_final
#CRP_model in model_selected_data
# with(model_selected_data, glm(ICU_admission ~ CRP, family = binomial)) |> pool() |> summary() |> print() # confirms the output is the same as mark_mi_selection

pool_performance(overall_step_model$RR_model_final)

model_selected_data_performance$ROC_pooled
 model_selected_data_performance$R2_pooled
