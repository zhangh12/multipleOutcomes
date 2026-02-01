
GLMAdapter <- R6::R6Class(
  "GLMAdapter",
  inherit = BaseAdapter,
  public = list(
    fit_model = function(){
      
      self$type <- 'glm'
      
      self$fit <- stats::glm(formula = self$spec$formula, 
                             family = self$spec$family, 
                             data = self$data)
      
      ## estimate
      self$estimate <- stats::coef(self$fit)
      
      ## score
      self$score <- sandwich::estfun(self$fit)
      self$n <- stats::nobs(self$fit)
      stopifnot(self$n == nrow(self$score))
      
      kept_ids <- setdiff(seq_len(nrow(self$data)), self$fit$na.action)
      rownames(self$score) <- self$data[[self$spec$id_col]][kept_ids]
      self$sample_id <- rownames(self$score)
      
      ## inv_hess
      self$inv_hess <- -sandwich::bread(self$fit)
      
      invisible(self)
    },
    
    refit = function(bdata){
      bfit <- stats::glm(formula = self$spec$formula, 
                         family = self$spec$family, 
                         data = bdata)
      stats::coef(bfit)
    }
    
  )
)

