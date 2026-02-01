
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
      self$score <- -as.matrix(stats::resid(self$fit, type = 'score'))
      ## unlike in glm, stats::nobs(self$fit) would return the number of 
      ## samples being used in model fitting. For coxph, stats::nobs(self$fit) 
      ## returns number of events, which is not what we need here
      self$n <- nrow(self$score)
      
      kept_ids <- setdiff(seq_len(nrow(self$data)), self$fit$na.action)
      rownames(self$score) <- self$data[[self$spec$id_col]][kept_ids]
      self$sample_id <- rownames(self$score)
      
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
      score <- -as.matrix(resid(bfit, type = 'score')) #
      imat <- imatCoxph(bfit, mean = FALSE)
      
      estimate <- solve(matrixSquareRoot(imat), colSums(-score))
      names(estimate) <- names(coef(bfit))
      estimate
      
    }
  )
)

