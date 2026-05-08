run_lesh <- function(data, formula, complexity = 0.02, vars = NULL, location = NULL) {
  # Unified entry point for running GOZH + LESH (SHAP-like contribution)
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.")
  }
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a formula, e.g., y ~ x1 + x2.")
  }
  if (!is.numeric(complexity) || length(complexity) != 1 || is.na(complexity)) {
    stop("`complexity` must be one numeric scalar.")
  }
  if (complexity < 0) {
    stop("`complexity` must be >= 0.")
  }

  y <- all.vars(formula)[1]
  formula_vars <- all.vars(formula)[-1]
  missing_cols <- setdiff(c(y, formula_vars), names(data))
  if (length(missing_cols) > 0) {
    stop(paste0("Columns not found in `data`: ", paste(missing_cols, collapse = ", ")))
  }

  if (is.null(vars)) {
    vars <- formula_vars
  }
  if (!all(vars %in% formula_vars)) {
    stop("`vars` must be a subset of predictors in `formula`.")
  }

  if (!is.null(location) && !all(location %in% names(data))) {
    stop("`location` contains columns not found in `data`.")
  }

  source("Fun_treeall.R")
  source("Fun_shap.R")

  message(sprintf("Running treeall over %d predictors (%d total combinations).",
                  length(formula_vars), 2^length(formula_vars) - 1))
  gozh <- treeall(formula = formula, data = data, location = location, complexity = complexity)

  df <- gozh[["all.q"]]
  rownames(df) <- df$var
  shap_values <- Shap_calcute(vars = vars, pd_data = df)

  list(
    best.vars = gozh[["best.vars"]],
    best.formula = gozh[["best.formula"]],
    all.q = df,
    shap_values = shap_values
  )
}
