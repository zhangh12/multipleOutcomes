#' Prognostic Variables Assisted Treatment Effect Detection
#' 
#' @param ... x
#' @param family x
#' @param data x
#'
#' @return x
#' @export
#'
#' @examples
#' # x
pated <- function(..., family, data){
  fit <- multipleOutcomes(..., family = family, data = data, data_index = NULL)

  parseFormula <- function(...){
    formulas <- list(...)
    formulas[sapply(formulas, function(x){!inherits(x, 'formula')})] <- NULL
    outcome <- NULL
    arm <- NULL
    for(fstr in formulas){
      outcome <- c(outcome, trimws(sub("\\s*~.*", "", deparse1(fstr))))
      arm <- c(arm, trimws(sub("^.*?~", "", deparse1(fstr))))
    }
    data.frame(outcome, arm)
  }

  fml <- parseFormula(...)

  id <- sapply(1:length(fit$id_map), function(i) fit$id_map[[i]][fml$arm[i]])
  id1 <- id[1]
  id2 <- id[-1]
  Delta <- coef(fit)[id1]
  delta <- coef(fit)[id2]
  mcov <- vcov(fit)
  opt_c <- as.vector(- solve(mcov[id2, id2, drop = FALSE]) %*% mcov[id2, id1, drop = FALSE])

  estimate <- Delta + sum(delta * opt_c)
  stderr <- sqrt(
    mcov[id1, id1] +
      2 * as.vector(t(opt_c) %*% mcov[id2, id1, drop = FALSE]) +
      as.vector(t(opt_c) %*% mcov[id2, id2, drop = FALSE] %*% opt_c)
  )

  pvalue <- pchisq((estimate / stderr)^2, df = 1, lower.tail = FALSE)
  
  treatment <-
    data.frame(
      term = fml$outcome[1],
      estimate = estimate,
      stderr = stderr,
      pvalue = pvalue,
      method = 'PATED',
      corr = NA
    )

  nonconfounder <-
    data.frame(
      term = fml$outcome,
      estimate = coef(fit)[id],
      stderr = sqrt(diag(vcov(fit))[id]),
      pvalue = pchisq((coef(fit)[id] / sqrt(diag(vcov(fit)))[id])^2, df = 1, lower.tail = FALSE),
      method = c('Standard', rep('Prognostic', nrow(fml) - 1)),
      corr = cov2cor(mcov)[id, id1]
    )

  ret <- rbind(treatment, nonconfounder)
  ret$method <- paste0(ret$method, ifelse(ret$method == 'Prognostic' & ret$pvalue < .05, '*', ''))
  attr(ret, 'Rel. Eff.') <- mcov[id1, id1] / stderr^2
  ret

}
