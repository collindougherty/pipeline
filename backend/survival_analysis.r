library(survival)
library(survminer)
library(ggplot2)

# Define the survival analysis function
survival_analysis_fx <- function(control, treatment, time_col, status_col) {
  # Ensure that the necessary columns exist in both dataframes
  if (!(time_col %in% names(control)) || !(status_col %in% names(control))) {
    stop("The time or status column does not exist in the control dataset.")
  }
  
  if (!(time_col %in% names(treatment)) || !(status_col %in% names(treatment))) {
    stop("The time or status column does not exist in the treatment dataset.")
  }

#   # Create the survival objects for control and treatment
#   control_surv <- Surv(time = control[[time_col]], event = control[[status_col]])
#   treatment_surv <- Surv(time = treatment[[time_col]], event = treatment[[status_col]])
  
#   # Fit survival curves using Cox proportional hazards model
#   control_fit <- survfit(control_surv ~ 1)
#   treatment_fit <- survfit(treatment_surv ~ 1)
  
#   # Create separate ggsurvplot objects for each group
#   control_plot <- ggsurvplot(
#     control_fit,
#     data = control,
#     conf.int = TRUE,
#     pval = TRUE,
#     risk.table = TRUE,
#     ggtheme = theme_minimal(),
#     palette = "blue"
#   )
  
#   treatment_plot <- ggsurvplot(
#     treatment_fit,
#     data = treatment,
#     conf.int = TRUE,
#     pval = TRUE,
#     risk.table = TRUE,
#     ggtheme = theme_minimal(),
#     palette = "red"
#   )

#   # Combine plots using ggplot2 functions
#   combined_plot <- ggplot() +
#     geom_line(data = control_plot$survplot$data, aes(x = time, y = surv, color = "Control")) +
#     geom_line(data = treatment_plot$survplot$data, aes(x = time, y = surv, color = "Treatment")) +
#     scale_color_manual(values = c("Control" = "blue", "Treatment" = "red")) +
#     labs(color = "Group") +
#     control_plot$theme +
#     theme(legend.title = element_blank())

#   if (control_plot$pval.coord != NULL && treatment_plot$pval.coord != NULL) {
#     combined_plot <- combined_plot + 
#       annotate("text", x = control_plot$pval.coord$x, y = control_plot$pval.coord$y, label = paste0("p = ", signif(control_plot$pval.text, digits = 2)), size = 3.5, color = "blue") +
#       annotate("text", x = treatment_plot$pval.coord$x, y = treatment_plot$pval.coord$y, label = paste0("p = ", signif(treatment_plot$pval.text, digits = 2)), size = 3.5, color = "red")
#   }

#   # Return the combined plot object
#   return(combined_plot)

  # Create the survival objects for control and treatment
  control_surv <- Surv(time = control[[time_col]], event = control[[status_col]])
  treatment_surv <- Surv(time = treatment[[time_col]], event = treatment[[status_col]])
  
  # Fit survival curves using Cox proportional hazards model
  control_fit <- survfit(control_surv ~ 1)
  treatment_fit <- survfit(treatment_surv ~ 1)

  # Combine the survival fits into a data frame for plotting
  surv_fits <- list(control = control_fit, treatment = treatment_fit)
  
  # Create a combined plot
  p <- ggsurvplot_combine(
    surv_fits,
    palette = c("blue", "red"),
    conf.int = TRUE,
    pval = TRUE,
    risk.table = TRUE,
    legend.title = "Groups",
    legend.labs = c("Control", "Treatment")
  )
  
  # Return the plot object
  return(p)
}
