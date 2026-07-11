
# user written functions for this project to clean the code 

#ensure libraries that are loaded 
library(mice)
library(visdat)
library(MASS)
library(psfmi)

# mark_mi_selection is based upon the psfmi package. It pools data from muiltiple imputation and performs backward  /
# stepwise selection using walds test and rubins rules using the psfmi_lr function. This produces a model object that
# prodices an object called the output_name_formula_name.


# Further this function produces compares the calibration of the model using the psfmi::pool_auc function. This produces / 
#  an object called output_name_performance.

mark_mi_selection = function(data, formula, output_name = NULL) {

    formula_name = deparse(substitute(formula))

    if (is.null(output_name)) {
        output_name = paste0(sub("_formula$", "", formula_name), "_bw_step_model")
    }

    # create long format of mice data for psfmi
        dta = complete(data, "long", include = TRUE) |> subset(.imp != 0)

        print("long format created for psfmi")

        print(as.formula(paste("ICU_admission ~", formula)))

    #perform the backwards step elimination using walds test and rubins rules using the psfmi_lr function
        x = 
        psfmi_lr (
            formula = as.formula(paste("ICU_admission ~", formula)),
            nimp = 50, 
            data = dta,
            impvar = ".imp",
            method = "D1", #D1 is walds test using rubins rules 
            direction = "BW",
            p.crit = 0.05)

        assign(output_name, x, envir = .GlobalEnv)
        message("Created object: ", output_name)
    # compare the clibration of the model 
        output_name_performance = paste0(output_name, "_performance")

        y  = pool_performance(
            data = complete(data, "long", include = TRUE) |>
                subset(.imp != 0),
            formula = as.formula(paste("ICU_admission ~", paste(sprintf("`%s`", x$predictors_final), collapse = " + "))),
            nimp = 50,
            impvar = ".imp",
            model_type = "binomial",
            cal.plot = FALSE,
            plot.method = "overlay")

        assign(output_name_performance, y, envir = .GlobalEnv)
        message("Created object: ", output_name_performance)
        }


