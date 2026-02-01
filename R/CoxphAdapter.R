
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
      ## unlike in glm, stats::nobs(self$fit) would return the number of 
      ## samples being used in model fitting. For coxph, stats::nobs(self$fit) 
      ## returns number of events, which is not what we need here
      self$n <- nrow(self$score)
      
      kept_ids <- setdiff(seq_len(nrow(self$data)), self$fit$na.action)
      rownames(self$score) <- self$data[[self$spec$id_col]][kept_ids]
      self$sample_id <- rownames(self$score)
      
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

