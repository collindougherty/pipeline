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

    print(unique(combined_df$Group))
    
    # Ensure the necessary columns exist
    required_cols <- c(time_col, status_col, covariates)
    if (!all(required_cols %in% names(combined_df))) {
        stop("One or more specified columns do not exist in the dataset.")
    }
    
    combined_df$Group <- factor(combined_df$Group, levels = c("Control", "Treatment"))

    # Let's make a formula for the cox model that includes the covariates
    if (!is.null(covariates) && length(covariates) > 0) {
        covariate_string <- paste(covariates, collapse = " + ")
        cox_formula <- as.formula(paste0("Surv(", time_col, ", ", status_col, ") ~ strata(Group) + ", covariate_string))
    } else {
        cox_formula <- as.formula(paste0("Surv(", time_col, ", ", status_col, ") ~ strata(Group)"))
    }


    print(cox_formula)

    # Fit Cox proportional hazards model
    #cox_fit <- coxph(Surv(combined_df[[time_col]], combined_df[[status_col]]) ~ strata(Group), data = combined_df)
    cox_fit <- coxph(cox_formula, data = combined_df)
    print("Cox fit success")

    print(summary(cox_fit))

    # Generate survival curves with stratification by 'Group'
    surv_fit <- survfit(cox_fit)
    print("Surv fit success")

    print(str(surv_fit))
    print("print str surv_fit successful")

    print(surv_fit$strata)
    print("prints successful")

    print("on to plotting")
    
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

    print("plotting successful")

    return(p)
}
