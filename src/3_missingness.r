#imputation prior to regression models 

# Run source code 
source("src/1_data_cleaning.r")

# library 
library(mice)
library(visdat)
library(VIM)
library(lattice)



#These variables apear to indicate only for those admitted to ICU?? 
data_for_imputation = model_data |>
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
     ) 

# explore missing data patterns
png("output/figures/imputation/missing_data_pattern.png", width = 1200, height = 800)
md.pattern(data_for_imputation, rotate.names = TRUE)
dev.off()

#imputation 
imp <- mice(data_for_imputation, m = 50, maxit = 5)

#density plots for imputed variables
png("output/figures/imputation/imputed_density_plots.png")
densityplot(imp, layout = c(3, 2))
dev.off()

#figures for impmutation diagnostics
png("output/figures/imputation/imputation_convergence.png")
plot(imp)
dev.off()

#convergence monitoring