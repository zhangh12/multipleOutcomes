
parseTreatmentVariableFromFormula <- function(...){
  formulas <- list(...)
  formulas[sapply(formulas, function(x){!inherits(x, 'formula')})] <- NULL
  outcome <- NULL
  arm <- NULL
  for(fstr in formulas){
    outcome <- c(outcome, trimws(sub("\\s*~.*", "", deparse1(fstr))))
    arm <- c(arm, trimws(sub("^.*?~", "", deparse1(fstr))))
  }
  data.frame(func = NA, outcome, arm, arg = NA, n_terms = 1)
}
