
LogRankAdapter <- R6::R6Class(
  "LogRankAdapter",
  inherit = BaseAdapter,
  public = list(
    fit_model = function(){
      
      self$type <- 'logrank'
      
      self$fit <- survival::coxph(formula = self$spec$formula, 
                                  data = self$data, 
                                  ties = self$spec$ties,
                                  model = TRUE, 
                                  iter = 0)
      
      ## estimate, score
      self$score <- -as.matrix(resid(self$fit, type = 'score')) #
      imat <- imatCoxph(self$fit, mean = FALSE)
      self$estimate <- solve(matrixSquareRoot(imat), colSums(-self$score))
      names(self$estimate) <- names(coef(self$fit))
      
      ## inv_hess
      self$inv_hess <- solve(matrixSquareRoot(imat)) * nrow(self$score)
      
      invisible(self)
    },
    
    refit = function(bdata) {
      bfit <- survival::coxph(formula = self$spec$formula, 
                              data = bdata, 
                              ties = self$spec$ties,
                              model = TRUE, 
                              iter = 0)
      score <- as.matrix(resid(bfit, type = 'score')) #
      imat <- imatCoxph(bfit, mean = FALSE)
      
      estimate <- solve(matrixSquareRoot(imat), colSums(score))
      names(estimate) <- names(coef(bfit))
      estimate
      
    }
  )
)

