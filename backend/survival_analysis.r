library(survival)
library(survminer)
library(ggplot2)
library(dplyr)

# Define the survival analysis function
survival_analysis_fx <- function(control, treatment, time_col, status_col, covariates = NULL) {
    # Combine control and treatment dataframes with a group column
    combined_df <- bind_rows(
        control %>% mutate(Group = 'Control'),
        treatment %>% mutate(Group = 'Treatment')
    )
    
    # Ensure the necessary columns exist
    required_cols <- c(time_col, status_col, covariates)
    if (!all(required_cols %in% names(combined_df))) {
        stop("One or more specified columns do not exist in the dataset.")
    }
    
    combined_df$Group <- factor(combined_df$Group, levels = c("Control", "Treatment"))


    removed_covariates <- c()
    # print which columns were removed and why
    for (col in colnames(combined_df)[colMeans(is.na(combined_df)) > 0.10]) {
        if (col != time_col && col != status_col && col %in% covariates) {
            print(paste0(col, " removed due to >10% missing data"))
            # remove the covariate from the covariates list
            covariates <- covariates[!covariates %in% col]
            # we also need to add this to a list of removed covariates that can be returned
            removed_covariates <- c(removed_covariates, col)
        }
    }

    # Adjusting dataframe filtering, ensuring time_col and status_col are always included
    # required_cols <- c(time_col, status_col)
    # filtered_cols <- colnames(combined_df)[colMeans(is.na(combined_df)) < 0.80 | colnames(combined_df) %in% required_cols]
    # combined_df <- combined_df[, filtered_cols]


    # Let's make a formula for the cox model that includes the covariates
    if (!is.null(covariates) && length(covariates) > 0) {
        covariate_string <- paste(covariates, collapse = " + ")
        cox_formula <- as.formula(paste0("Surv(", time_col, ", ", status_col, ") ~ strata(Group) + ", covariate_string))
    } else {
        cox_formula <- as.formula(paste0("Surv(", time_col, ", ", status_col, ") ~ strata(Group)"))
    }

    # Fit Cox proportional hazards model
    #cox_fit <- coxph(Surv(combined_df[[time_col]], combined_df[[status_col]]) ~ strata(Group), data = combined_df)
    cox_fit <- coxph(cox_formula, data = combined_df)

    cox_summary <- summary(cox_fit)

    # Generate survival curves with stratification by 'Group'
    surv_fit <- survfit(cox_fit)

    #print(str(surv_fit))

    strata <- surv_fit$strata
    
    p <- ggsurvplot(
        surv_fit, 
        data = combined_df,
        #pval = TRUE, #this line breaks the code for some reason
        conf.int = TRUE,
        risk.table = TRUE,
        ggtheme = theme_minimal(),
        legend.title = "Group",
        legend.labs = c("Control", "Treatment"),
        palette = c("blue", "red")
    )

    # Then modify it with ggplot2 functions
    p$plot <- p$plot +
    labs(x = "Time (in Months)", y = "Survival Probability") +
    ylim(min(surv_fit$surv), max(surv_fit$surv))

    return(list(p = p, removed_covariates = removed_covariates, cox_summary = cox_summary, cox_formula = cox_formula, strata = strata))
}
