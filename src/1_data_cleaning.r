# load data and clean for regression analysis 

#library 
library(tidyverse)
library(gtsummary)
library(readxl)
library(conflicted)

#conditions for conflict resolution
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

#set random seed for reproducibility
set.seed(123)

#load data - sheet named "Sheet" 
data = read_excel("data/original_data/MMICS data 13-05-26 (for analysis).xlsx", sheet = "Sheet", skip = 1)

# convert selected measurement columns to numeric
numeric_columns = c(
    "qSOFA Score", "APACHE 2 Score",
    "Quantity (ml/actual body weight)",
    "Baseline StO2 level",
    "Nadir of StO2",
    "Rate of deoxygenation (%/sec)...48",
    "Rate of reoxygenation (%/sec)...49",
    "Peak StO2 post vascular occlusion (%)",
    "Time taken to reach baseline StO2 (secs)",
    "Vascular Occlusion Time...52",
    "Microvascular flox index...53",
    "Total vessel density (mm/mm^-2)",
    "Perfused vessel density (mm/mm^-2)",
    "Rate of deoxygenation (%/sec)...56",
    "Rate of reoxygenation (%/sec)...57",
    "Peak StO2 post VOT",
    "Time taken to reach baseline StO2",
    "Vascular Occlusion Time...60",
    "Microvascular flox index...61",
    "Total vessel density",
    "Perfused vessel density",
    "SpO2",
    "Mean arterial Pressure",
    "Temperature", 
    "Frailty Score",
    "GCS", 
    "Highest Number of Organ Support",
    "Length of Time in ICU/HDU (days)"
)

data = data |>
    mutate(across(any_of(numeric_columns), ~ {
        x = .
        if (is.character(x)) {
            x = na_if(trimws(x), "N/A")
        }
        as.numeric(x)
    }))

#  amalgamata Addmission Date and Time together into POSIXct
data = data |> 
    mutate(Admission_date = as.POSIXct(paste(`Admission Date`, `Admission Time`), format = "%d/%m/%Y %H:%M")) |> 
    select(-`Admission Date`, -`Admission Time`)

# change Yes No to 1 and 0 for regression analysis
data = data |>
    mutate(across(c(
    Hypertension,
    Diabetes,
    `Chronic Kidney Disease`,
    `Chronic Respiratory Disease`,
    `Cardiovascular Disease`,
    `Peripheral Vascular Disease`,
    `Cancer Diagnosis`,
    `Chronic Liver Disease`,
    `Chronic Neurological Disease`,
    Immunosuppression
    ), ~ ifelse(. == "Yes" | . == "Y", 1, 0))) |> 
    mutate(Death = ifelse(`Why did the patient exit the study?` == "Death", 1, 0)) 

# Find ICU admission from ICU data sheet

data_ICU = read_excel("data/original_data/MMICS data 13-05-26 (for analysis).xlsx", sheet = "ICU", skip = 1) |> select(`Subject key`)

data = data |> 
    mutate(ICU_admission = ifelse(`Subject key` %in% data_ICU$`Subject key`, 1, 0)) |> 
    select(-`Subject key`)

# data for models 
remove = c(
    "Patient Initials",
    "Consent Time",
    "Admission_date",
    "Admitted to ICU Y/N",
    "Subject key",
    "Other Medical History",

    # ICU missiness variables 
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

    #related to outcome 
        "Why did the patient exit the study?",
            "Death",
            "Has the Patient Died Within 28 Days Y/N",
            "Length of Hospital Stay (days)", 
            "Length of Time in ICU/HDU  (days)", 
             "Highest Number of Organ Support",    
             "Highest Number of Organ Failures",  
             "Critical Care Therapies Y/N",   
             "Blood Gas Type",
    # other predictive scores 
            "qSOFA.Score",
            "Final.NEWS2.score"
)

model_data = data |> select(-any_of(remove)) |>
    select(where(~ n_distinct(.x, na.rm = TRUE) > 1)) # remove columns with no usable variation for regression models

print("Data cleaning complete. Data is ready for analysis.")