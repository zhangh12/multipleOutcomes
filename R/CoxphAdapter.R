
CoxphAdapter <- R6::R6Class(
  "CoxphAdapter",
  inherit = BaseAdapter,
  public = list(
    fit_model = function(){
      
      self$type <- 'coxph'
      
      self$fit <- survival::coxph(formula = self$spec$formula, 
                                  data = self$data, model = TRUE)
      
      ## estimate
      self$estimate <- stats::coef(self$fit)
      
      ## score
      self$score <- -as.matrix(resid(self$fit, type = 'score'))
      
      ## inv_hess
      self$inv_hess <- solve(imatCoxph(self$fit))
      
      invisible(self)
    },
    
    refit = function(bdata) {
      bfit <- survival::coxph(formula = self$spec$formula, 
                              data = bdata)
      stats::coef(bfit)
    }
  )
)

