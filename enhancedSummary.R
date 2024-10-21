enhancedSummary <- function(model, correlation = FALSE, symbolic.cor = FALSE, ...) {
  
  if (inherits(model, "lm")) {
    # Handle lm models (as in previous code)
    
  } else if (inherits(model, "merMod")) {
    
    # No need for coercion, just use the model as is
    summary_model <- summary(model)
    
    # Extract fixed and random effects
    fixed_effects <- summary_model$coefficients
    random_effects <- summary_model$varcor
    
    # RMSE and R-squared calculations
    residuals <- resid(model)
    rmse <- sqrt(mean(residuals^2))
    r.squared <- r.squaredGLMM(model)
    marginal_r2 <- r.squared[1, "R2m"]
    conditional_r2 <- r.squared[1, "R2c"]
    
    # Coefficients table
    confint_vals <- as.data.frame(confint(model))
    ci_fixed <- confint_vals[rownames(fixed_effects), ]
    
    coef_table <- cbind(
      Estimate = fixed_effects[, 1],
      Std.Error = fixed_effects[, 2],
      t.value = fixed_effects[, 3],
      'CI(2.5)' = ci_fixed[, 1],
      'CI(97.5)' = ci_fixed[, 2]
    )
    
    # Output
    print(model@call)
    cat("\nRandom Effects:\n")
    print(random_effects)
    cat("\nModel Performance\n")
    cat(sprintf("RMSE: %.3f\n", rmse))
    cat(sprintf("Marginal R-squared: %.3f\n", marginal_r2))
    cat(sprintf("Conditional R-squared: %.3f\n", conditional_r2))
    cat("\nFixed Effects Coefficients\n")
    print(coef_table)
    
  } else {
    stop("The model must be of class 'lm' or 'merMod'")
  }
}
