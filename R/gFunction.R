
# g(S(t)), a transformed estimate of survival probability.
# See https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_lifetest_details08.htm
gFunction <- function(conf.type = c('log', 'log-log', 'plain', 'logit', 'arcsin')){

  conf.type <- match.arg(conf.type)

  switch(conf.type,
    'log'     = function(x) log(x),
    'log-log' = function(x) log(-log(x)),
    'plain'   = function(x) x,
    'logit'   = function(x) log(x / (1 - x)),
    'arcsin'  = function(x) asin(sqrt(x))
  )
}

