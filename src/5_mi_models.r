#library 
library(mice)
library(visdat)
library(MASS)
library(psfmi)
library(writexl)
library(car) # for VIF calculation

# list of variables for use in models 
    #baseline variables 
    baseline_terms = c("SpO2", "RR", "HR", "MAP", "Temperature", "GCS")
    baseline_formula = paste(sprintf("`%s`", baseline_terms), collapse = " + ")
    
    #biomarkers 
    biomarkers_terms = c("CRP", "Lactate", "HR", "Procalcitonin", "MRproADM")
    biomarkers_formula = paste(sprintf("`%s`", biomarkers_terms), collapse = " + ")
    
    #tissue oxygenation 
    tissue_oxygenation_terms = c("Baseline_StO2_level", "rate_of_deoxygenation")
    tissue_oxygenation_formula = paste(sprintf("%s", tissue_oxygenation_terms), collapse = " + ")

    #microvascular function
    microvascular_function_terms = c("Microvascular_flox_index")
    microvascular_function_formula = paste(sprintf("`%s`", microvascular_function_terms), collapse = " + ")

# source
source("src/4_univariable_models.r")
source("src/0_functions.r") # see functions for mark_mi_selection function

#mice cleanin and imputation of missing data
model_selected_data = model_data |>
    select(any_of(c("ICU_admission", baseline_terms, biomarkers_terms, tissue_oxygenation_terms, microvascular_function_terms, 
    "Mean arterial Pressure", "Microvascular flox index...53", "MR-proADM","Rate of deoxygenation (%/sec)...48",
    "Baseline StO2 level"))) |>
    rename(
        MAP = `Mean arterial Pressure`,
        `Microvascular_flox_index` = `Microvascular flox index...53`,
        rate_of_deoxygenation = `Rate of deoxygenation (%/sec)...48`,
        MRproADM = `MR-proADM`,     
        Baseline_StO2_level = `Baseline StO2 level`,
     ) |> 
    mice(m = 50, maxit = 5)
print("MICE imputation completed. Imputed data is ready for multivariable logistic regression.")


# Models 
    # basline_model 
    baseline_model = with(model_selected_data, glm(as.formula(paste("ICU_admission ~", baseline_formula)), family = binomial()))
    mark_mi_selection(model_selected_data, baseline_formula, "baseline_step_model")

    # biomarkers_model
    biomarkers_model = with(model_selected_data, glm(as.formula(paste("ICU_admission ~", biomarkers_formula)), family = binomial()))
    mark_mi_selection(model_selected_data, biomarkers_formula, "biomarkers_step_model")

    # tissue oxygenation 
    tissue_oxygenation_model = with(model_selected_data, glm(as.formula(paste("ICU_admission ~", tissue_oxygenation_formula)), family = binomial()))
    mark_mi_selection(model_selected_data, tissue_oxygenation_formula, "tissue_oxygenation_step_model")

    # microvascular function
    microvascular_function_model = with(model_selected_data, glm(as.formula(paste("ICU_admission ~", microvascular_function_formula)), family = binomial()))
    mark_mi_selection(model_selected_data, microvascular_function_formula, "microvascular_function_step_model")

    #overall predictive model with all variables
    overall_model = with(
        model_selected_data,
        glm(
            as.formula(paste("ICU_admission ~", paste(baseline_formula, biomarkers_formula, tissue_oxygenation_formula, microvascular_function_formula, sep = " + "))),
            family = binomial()
        )
    )
    mark_mi_selection(
        model_selected_data,
        paste(baseline_formula, biomarkers_formula, tissue_oxygenation_formula, microvascular_function_formula, sep = " + "),
        "overall_step_model"
    )

#makes table of full models 
    pooled_full_model_sheets = list()
    for (i in c("baseline_model", "biomarkers_model", "tissue_oxygenation_model", "microvascular_function_model", "overall_model")) {
        model_object = get0(i, ifnotfound = NULL)

        if (!is.null(model_object)) {
            pooled_object = tryCatch(pool(model_object), error = function(e) NULL)

            if (!is.null(pooled_object)) {
                pooled_df = summary(pooled_object, conf.int = TRUE, exponentiate = TRUE)
                pooled_df = data.frame(term = rownames(pooled_df), pooled_df, row.names = NULL)
            } else {
                pooled_df = data.frame(term = NA_character_, estimate = NA_real_)
            }
        } else {
            pooled_df = data.frame(term = NA_character_, estimate = NA_real_)
        }

        pooled_full_model_sheets[[i]] = pooled_df
    }

    write_xlsx(pooled_full_model_sheets, "output/tables/mi_full_models_pooled.xlsx")

#AUC 





    #Produce a data frame of the model selection results for each model

    scalar_or_na = function(x, na_value = NA_real_) {
        if (is.null(x) || length(x) == 0) {
            return(na_value)
        }
        x[[1]]
    }

    # Produce a data frame of pooled performance results for each full model
    full_model_specs = list(
        baseline = list(formula = baseline_formula, predictors = baseline_terms),
        biomarkers = list(formula = biomarkers_formula, predictors = biomarkers_terms),
        tissue_oxygenation = list(formula = tissue_oxygenation_formula, predictors = tissue_oxygenation_terms),
        microvascular_function = list(formula = microvascular_function_formula, predictors = microvascular_function_terms),
        overall = list(
            formula = paste(baseline_formula, biomarkers_formula, tissue_oxygenation_formula, microvascular_function_formula, sep = " + "),
            predictors = unique(c(baseline_terms, biomarkers_terms, tissue_oxygenation_terms, microvascular_function_terms))
        )
    )

    all_full_model_results = list()
    for (i in names(full_model_specs)) {
        model_spec = full_model_specs[[i]]

        full_model_performance = tryCatch(
            pool_performance(
                data = complete(model_selected_data, "long", include = TRUE) |>
                    subset(.imp != 0),
                formula = as.formula(paste("ICU_admission ~", model_spec$formula)),
                nimp = 50,
                impvar = ".imp",
                model_type = "binomial",
                cal.plot = FALSE,
                plot.method = "overlay"
            ),
            error = function(e) NULL
        )

        full_model_results = data.frame(
            model = i,
            outcome_variable = "ICU_admission",
            n_predictors_full = as.integer(length(model_spec$predictors)),
            predictors_full = paste(model_spec$predictors, collapse = ", "),
            AUC_pooled = scalar_or_na(if (is.null(full_model_performance)) NULL else full_model_performance$ROC_pooled, NA_real_),
            R2_pooled = scalar_or_na(if (is.null(full_model_performance)) NULL else full_model_performance$R2_pooled, NA_real_),
            Brier_score_pooled = scalar_or_na(if (is.null(full_model_performance)) NULL else full_model_performance$Brier_Scaled_pooled, NA_real_),
            Hosmer_lemeshow_test_pooled = scalar_or_na(if (is.null(full_model_performance)) NULL else full_model_performance$HLtest_pooled, NA_real_)
        )

        assign(paste0(i, "_full_model_results"), full_model_results)
        all_full_model_results[[i]] = full_model_results
    }

    # Save full model pooled performance results
    write_xlsx(do.call(rbind, all_full_model_results), "output/tables/mi_full_model_results.xlsx")

    all_model_results = list()
    for (i in c("baseline", "biomarkers", "tissue_oxygenation", "microvascular_function", "overall")) {
        model_name = paste0(i, "_step_model")
        model_object = get0(model_name, ifnotfound = NULL)
        model_name_performance = paste0(i, "_step_model_performance")
        model_performance_object = get0(model_name_performance, ifnotfound = NULL)

        predictors_final_vec = if (is.null(model_object)) NULL else model_object$predictors_final

        if (is.null(predictors_final_vec) || length(predictors_final_vec) == 0) {
            n_predictors_final = NA_integer_
            predictors_final = NA_character_
        } else {
            n_predictors_final = as.integer(length(predictors_final_vec))
            predictors_final = paste(predictors_final_vec, collapse = ", ")
        }

        model_results = data.frame(
            model = i,
            outcome_variable = "ICU_admission",
            n_predictors_final = n_predictors_final,
            predictors_final = predictors_final,
            AUC_pooled = scalar_or_na(if (is.null(model_performance_object)) NULL else model_performance_object$ROC_pooled, NA_real_),
            R2_pooled = scalar_or_na(if (is.null(model_performance_object)) NULL else model_performance_object$R2_pooled, NA_real_),
            Brier_score_pooled = scalar_or_na(if (is.null(model_performance_object)) NULL else model_performance_object$Brier_Scaled_pooled, NA_real_),
            Hosmer_lemeshow_test_pooled = scalar_or_na(if (is.null(model_performance_object)) NULL else model_performance_object$HLtest_pooled, NA_real_) 
                                    )

        assign(paste0(i, "_model_results"), model_results)
        all_model_results[[i]] = model_results
    }

    #Save the model results to an Excel file
    write_xlsx(do.call(rbind, all_model_results), "output/tables/mi_model_selected_stepwise_results.xlsx")



#Variance inflation factor (VIF) for each model pooled across imputations
    extract_vif_df = function(fit_object) {
        vif_object = tryCatch(car::vif(fit_object), error = function(e) NULL)

        if (is.null(vif_object)) {
            return(data.frame(term = NA_character_, vif = NA_real_, stringsAsFactors = FALSE))
        }

        if (is.matrix(vif_object)) {
            vif_df = data.frame(term = rownames(vif_object), vif_object, row.names = NULL, check.names = FALSE)

            # If GVIF output is returned, use adjusted GVIF for comparability.
            if (all(c("GVIF", "Df") %in% names(vif_df))) {
                vif_df$vif = vif_df$GVIF^(1 / (2 * vif_df$Df))
            } else if ("GVIF^(1/(2*Df))" %in% names(vif_df)) {
                vif_df$vif = vif_df[["GVIF^(1/(2*Df))"]]
            } else {
                vif_df$vif = as.numeric(vif_df[[1]])
            }

            return(vif_df[, c("term", "vif")])
        }

        data.frame(term = names(vif_object), vif = as.numeric(vif_object), row.names = NULL, stringsAsFactors = FALSE)
    }

    pooled_vif_sheets = list()
    pooled_vif_summary_all = list()

    for (i in c("baseline_model", "biomarkers_model", "tissue_oxygenation_model", "microvascular_function_model", "overall_model")) {
        model_object = get0(i, ifnotfound = NULL)

        if (is.null(model_object) || is.null(model_object$analyses) || length(model_object$analyses) == 0) {
            pooled_vif_sheets[[i]] = data.frame(
                imputation = NA_integer_,
                term = NA_character_,
                vif = NA_real_,
                stringsAsFactors = FALSE
            )

            pooled_vif_summary_all[[i]] = data.frame(
                model = i,
                term = NA_character_,
                n_imputations = NA_integer_,
                mean_vif = NA_real_,
                median_vif = NA_real_,
                min_vif = NA_real_,
                max_vif = NA_real_,
                stringsAsFactors = FALSE
            )

            next
        }

        vif_by_imp = lapply(seq_along(model_object$analyses), function(imp_index) {
            fit_object = model_object$analyses[[imp_index]]
            vif_df = extract_vif_df(fit_object)
            vif_df$imputation = imp_index
            vif_df[, c("imputation", "term", "vif")]
        })

        vif_long = do.call(rbind, vif_by_imp)
        pooled_vif_sheets[[i]] = vif_long

        vif_long_valid = vif_long[!is.na(vif_long$term) & !is.na(vif_long$vif), , drop = FALSE]

        if (nrow(vif_long_valid) == 0) {
            pooled_vif_summary = data.frame(
                model = i,
                term = NA_character_,
                n_imputations = NA_integer_,
                mean_vif = NA_real_,
                median_vif = NA_real_,
                min_vif = NA_real_,
                max_vif = NA_real_,
                stringsAsFactors = FALSE
            )
        } else {
            split_by_term = split(vif_long_valid$vif, vif_long_valid$term)
            pooled_vif_summary = data.frame(
                model = i,
                term = names(split_by_term),
                n_imputations = as.integer(vapply(split_by_term, length, integer(1))),
                mean_vif = as.numeric(vapply(split_by_term, mean, numeric(1))),
                median_vif = as.numeric(vapply(split_by_term, median, numeric(1))),
                min_vif = as.numeric(vapply(split_by_term, min, numeric(1))),
                max_vif = as.numeric(vapply(split_by_term, max, numeric(1))),
                row.names = NULL,
                stringsAsFactors = FALSE
            )
        }

        pooled_vif_summary_all[[i]] = pooled_vif_summary
    }
    write_xlsx(do.call(rbind, pooled_vif_summary_all), "output/tables/mi_vif_pooled_summary.xlsx")
 