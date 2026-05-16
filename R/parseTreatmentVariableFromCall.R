
# Extract per-call metadata (function name, outcome label, treatment-arm
# variable, and any engine-specific argument like km_'s conf_type) from the
# `...` arguments passed to pated(). Uses the call AST rather than string
# surgery on deparse() output so that formulas with nested parens (e.g.,
# `y ~ arm + us(visit | pid)` for mmrm_) are parsed correctly.
parseTreatmentVariableFromCall <- function(...){

  calls <- as.list(substitute(list(...)))[-1L]
  for(i in seq_along(calls)){
    if(!is.call(calls[[i]])){
      stop(str_glue('{calls[[i]]} is not a call. '))
    }
  }

  func <- character(length(calls))
  outcome <- character(length(calls))
  arm <- character(length(calls))
  arg <- rep(NA_character_, length(calls))
  n_terms <- rep(1L, length(calls))

  for (idx in seq_along(calls)) {
    cstr <- calls[[idx]]
    func[idx] <- as.character(cstr[[1]])

    # Locate the formula argument: try `formula = ...` first, otherwise the
    # first positional argument after the function name.
    fml <- cstr$formula
    if (is.null(fml)) {
      arg_names <- names(cstr)
      if (is.null(arg_names)) arg_names <- rep("", length(cstr))
      positional <- which(arg_names == "")
      positional <- positional[positional > 1]
      if (length(positional) == 0) {
        stop(str_glue('No formula argument found in call: {deparse1(cstr)}'))
      }
      fml <- cstr[[positional[1]]]
    }
    if (!is.call(fml) || !identical(fml[[1]], as.name("~"))) {
      stop(str_glue('Argument is not a formula in call: {deparse1(cstr)}'))
    }

    outcome[idx] <- trimws(deparse1(fml[[2]]))

    # The treatment variable is the first plain variable name appearing on the
    # RHS, skipping anything that's only a covariance-structure helper (us,
    # ar1, cs, ...). all.vars() walks the AST and returns variable names in
    # order of appearance, so the first element is the treatment variable in
    # every formula we currently support.
    rhs_vars <- all.vars(fml[[3]])
    if (length(rhs_vars) == 0) {
      stop(str_glue('No treatment variable found on RHS of formula in call: {deparse1(cstr)}'))
    }
    arm[idx] <- rhs_vars[1]

    if (func[idx] == 'km_') {
      if (is.null(cstr$conf_type)) {
        stop('conf_type is missing in km_')
      }
      arg[idx] <- as.character(cstr$conf_type)
    }

    if (func[idx] == 'quantile_') {
      if (is.null(cstr$probs)) {
        stop('probs is missing in quantile_')
      }
      arg[idx] <- deparse(cstr$probs)
      n_terms[idx] <- length(eval(cstr$probs))
    }
  }

  data.frame(func = func, outcome = outcome, arm = arm,
             arg = arg, n_terms = n_terms,
             stringsAsFactors = FALSE)
}
