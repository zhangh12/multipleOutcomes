
MMRMAdapter <- R6::R6Class(
  "MMRMAdapter",
  inherit = BaseAdapter,
  public = list(
    fit_model = function(){
      
      self$type <- 'mmrm'
      
      self$data[[self$spec$id_col]] <- as.factor(self$data[[self$spec$id_col]])
      
      self$fit <- mmrm::mmrm(formula = self$spec$formula, 
                             data = self$data, 
                             covariance = self$spec$covariance,
                             reml = self$spec$reml, 
                             control = self$spec$control)
      
      ## estimate
      self$estimate <- stats::coef(self$fit)
      
      ## score
      tr <- try(self$score <- mmrm::component(self$fit, 'score_per_subject'), silent = TRUE)
      if('try-error' %in% class(tr)){
        stop('score_per_subject is not available. Check mmrm version installed in your session. Contact author for assistance. ')
      }
      
      ## It is very tricky to align rows in score_per_subject and sample ID.
      ## I don't know details of how mmrm is implemented. 
      ## For example, the input data frame does not 
      ## assume samples from a subject are physically contiguous records. 
      ## Also, records with NA may be discarded. As a result, it is unknown 
      ## how to map rows in score with subjects.
      ## I use the following approach as a guardrail: 
      ## 1. extract model matrix from fitted object where only samples being 
      ##    used in fitted the model are retained. This is a record-wise 
      ##    data frame where a subject may have data in more than one row. 
      ##    See ?stats::model.matrix. This matrix does not have the pid column. 
      ## 2. extract full_frame from fitted object. This data frame consists 
      ##    of all columns, including pid. See ?mmrm::component
      ## 3. For any variable that is in both data frames extracted in step 1 and 
      ##    step 2, check whether they are identical. With moderate or large 
      ##    sample size, it is unlikely that all variables are identical in 
      ##    two data frames if they are not well-aligned. 
      ## 4. If the test in step 3 passes, I assume that the orders of pid in 
      ##    score and full_frame are the same. I need to request for 
      ##    confirmation from mmrm authors. 
      
      m1 <- as.data.frame(stats::model.matrix(self$fit))
      m2 <- mmrm::component(self$fit, 'full_frame')
      
      shared_cols <- intersect(names(m1), names(m2))
      
      for(col in shared_cols){
        if(max(abs(m1[, col] - m2[, col])) > 1e-4){
          stop(col, ' does not match. Unable to map score to subject ID. ')
        }
      }
      
      ids <- m2[[self$spec$id_col]]
      rownames(self$score) <- ids[which(!duplicated(ids))]
      self$sample_id <- rownames(self$score)
      
      stopifnot(nrow(self$score) == mmrm::component(self$fit, 'n_subjects'))
      self$n <- mmrm::component(self$fit, 'n_subjects')
      
      ## inv_hess
      self$inv_hess <- -self$fit$beta_vcov * self$n
      
      invisible(self)
    },
    
    refit = function(bdata){
      bdata[[self$spec$id_col]] <- as.factor(bdata[[self$spec$id_col]])
      bfit <- mmrm::mmrm(formula = self$spec$formula, 
                         data = bdata, 
                         covariance = self$spec$covariance,
                         reml = self$spec$reml, 
                         control = self$spec$control)
      stats::coef(bfit)
    }
    
  )
)

