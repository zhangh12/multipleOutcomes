
# Generate two curves of survival probability with pointwise 95% confidence 
# interval: 
# 1. PATED adjusted KM curve
# 2. Conventional KM curve
comparePointwiseConfidenceIntervalWidth <- function(pated_res, km_res, transform = 'identity'){
  
  conf.type <- attr(pated_res, 'conf.type')
  pated_estimate <- pated_res$estimate
  names(pated_estimate) <- rownames(pated_res)
  pated_coordinate <- 
    data.frame(
      Time = extractKaplanMeierTimes(pated_estimate, sort = FALSE),
      Strata = pated_estimate %>% 
        names() %>%
        sapply(function(x) {sub('.*time_\\((.*)\\)_\\(.*\\)', '\\1', x)}),
      pated_estimate = pated_estimate,
      pated_ci_width = pated_res$UCI - pated_res$LCI
    )
  rownames(pated_coordinate) <- NULL
  
  km_estimate <- km_res$estimate
  names(km_estimate) <- rownames(km_res)
  km_coordinate <- 
    data.frame(
      Time = extractKaplanMeierTimes(km_estimate, sort = FALSE),
      Strata = km_estimate %>% 
        names() %>%
        sapply(function(x) {sub('.*time_\\((.*)\\)_\\(.*\\)', '\\1', x)}),
      km_estimate = km_estimate,
      km_ci_width = km_res$UCI - km_res$LCI
    )
  rownames(km_coordinate) <- NULL
  
  m <- merge(pated_coordinate, km_coordinate, by = c('Time', 'Strata'), all = TRUE)
  
  p1 <- 
    ggplot(m, aes(x = .data$Time, y = .data$km_ci_width / .data$pated_ci_width, group = .data$Strata)) + 
      geom_point(aes(color = .data$Strata), size = .5) + 
      scale_x_continuous(transform = transform) + 
      theme_minimal() +
      labs(title = "Widths of Pointwise 95% CI",
           x = str_glue('Time{ifelse(transform == "identity", "", paste(" (", transform, ")"))}'),
           y = 'KM / PATED')
  
  p2 <- 
    ggplot(m, aes(x = .data$km_estimate, y = .data$pated_estimate, group = .data$Strata)) + 
    geom_point(aes(color = .data$Strata), size = .5) + 
    geom_abline(intercept = 0, slope = 1, linetype = 'dashed') + 
    theme_minimal() + 
    labs(title = 'Estimates of S(t)', 
         x = 'KM', 
         y = 'PATED')
  
  list(p1, p2)
}
