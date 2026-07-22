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
    "Mean arterial Pressure", "Microvascular flox index...53", "MR-proADM","Rate of deoxygenation (%/sec)...48",
    "Baseline StO2 level"))) |>
    rename(
        MAP = `Mean arterial Pressure`,
        `Microvascular_flox_index` = `Microvascular flox index...53`,
        rate_of_deoxygenation = `Rate of deoxygenation (%/sec)...48`,
        MRproADM = `MR-proADM`,     
        Baseline_StO2_level = `Baseline StO2 level`,
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

## colinearity check for imputed data
completed_data <- complete(imp, 1)
numeric_data <- completed_data[, vapply(completed_data, is.numeric, logical(1)), drop = FALSE]
corr_mat <- cor(numeric_data, use = "pairwise.complete.obs")

png("output/figures/colinearity/correlogram.png", width = 1400, height = 1200, res = 150)
if (requireNamespace("corrplot", quietly = TRUE)) {
    corrplot::corrplot(
        corr_mat,
        method = "color",
        type = "lower",
        tl.col = "black",
        tl.cex = 0.7
    )
} else {
    heatmap(corr_mat, symm = TRUE, Colv = NA, Rowv = NA, scale = "none", margins = c(10, 10))
}
dev.off()