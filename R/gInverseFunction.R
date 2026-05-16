
# Inversed function of g(S(t)).
# See https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_lifetest_details08.htm
gInverseFunction <- function(conf.type = c('log', 'log-log', 'plain', 'logit', 'arcsin')){

  conf.type <- match.arg(conf.type)

  switch(conf.type,
    'log'     = function(x) exp(x),
    'log-log' = function(x) exp(-exp(x)),
    'plain'   = function(x) x,
    'logit'   = function(x) exp(x) / (1 + exp(x)),
    'arcsin'  = function(x) sin(pmin(pmax(x, 0), pi / 2))^2
  )
}

