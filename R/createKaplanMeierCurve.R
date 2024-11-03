
#' Create curve of survival probability based on conventional KM method, or 
#' PATED adjusted KM estimates.
#' Refer to km_res or pated_res in `pated` about the format of `input`.
#' Transformations are supported when computing confidence intervals.
#' Refer to https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_lifetest_details08.htm 
#' or the LIFETEST proc in SAS for more details of transformation.
createKaplanMeierCurve <- function(input, title = '', transform = 'identity'){
  
  estimate <- input$estimate
  names(estimate) <- rownames(input)
  coordinate <- 
    data.frame(
      Time = extractKaplanMeierTimes(estimate, sort = FALSE),
      Strata = estimate %>% 
        names() %>%
        sapply(function(x) {sub('.*time_\\((.*)\\)_\\(.*\\)', '\\1', x)}),
      St = input$estimate,
      LCI = input$LCI,
      UCI = input$UCI
    )
  rownames(coordinate) <- NULL
  
  p <- ggplot(coordinate, aes(x = Time, group = Strata)) + 
    geom_ribbon(aes(ymin = LCI, ymax = UCI, fill = Strata), alpha = .2) + 
    geom_step(aes(y = St, color = Strata)) + 
    scale_x_continuous(transform = transform) + 
    labs(x = str_glue('Time{ifelse(transform == "identity", "", paste(" (", transform, ")"))}'), y = 'Survival Probability S(t)', title = title) + 
    ylim(c(0, 1))
  
  attr(p, 'coordinate') <- coordinate
  p
  
}