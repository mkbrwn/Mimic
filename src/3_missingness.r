#imputation prior to regression models 

# Run source code 
source("src/1_data_cleaning.r")

# library 
library(mice)
library(visdat)

# explore missing data patterns
png("output/figures/imputation/missing_data_pattern.png", width = 1200, height = 800)
md.pattern(data |> select(where(~ any(is.na(.)))),
rotate.names = TRUE)
dev.off()

#imputation 

#These variables apear to indicate only for those admitted to ICU?? 
    data_for_imputation = data |>
        select(-any_of(c(
            "Respiratory Support",
            "Vasopressor/Inotrope Therapy",
            "Continuous Sedation or a General Anaesthetic",
            "Central Vascular Catheter",
            "Emergency Surgery Initiated <4 Hours of Hospital Admission",
            "Quantity (ml/actual body weight)",
            "Admitted to ICU from Where",
            "APACHE 2 Score",
            "Did myself or someone else think pt need ICU?",
            "Admission_date",
            "Admitted to ICU Y/N",
            "Vascular Occlusion Time...60",

            # colinear 
            "qSOFA.Score",
            "Final.NEWS2.score"

        )))


# remove columns with no usable variation for imputation models
data_for_imputation = data_for_imputation |>
    select(where(~ dplyr::n_distinct(.x, na.rm = TRUE) > 1))

pred <- quickpred(data_for_imputation, mincor = 0.4, minpuc = 0.5)

Imputation_model <- futuremice(
    data_for_imputation,
    m = 5,
    method = "cart",
    predictorMatrix = pred,
    n.core = 5,
    maxit = 5,
    parallelseed = 123
)

