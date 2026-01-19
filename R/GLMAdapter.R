
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
      dm <- model.matrix(self$spec$formula, model.frame(self$spec$formula, self$data))
      self$score <- resid(self$fit, type = 'response') * dm
      
      ## inv_hess
      self$inv_hess <- inverseHessianMatrixGlm(self$spec$family, dm, self$fit)
      
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

