if(0){
library(mvtnorm)
library(data.table)
library(sandwich)
library(lmtest)

robust <- function(model){
  coeftest(model, vcov = vcovHC(model, type = "HC1"))
}


power <- function(estimate, se, level = .05){
  mean(pchisq((estimate / se)^2, df = 1, lower.tail = FALSE) < level)
}

n <- 500

adj1 <- NULL
adj2 <- NULL
adj3 <- NULL
se1 <- NULL
se2 <- NULL
se3 <- NULL
pat <- NULL
se0 <- NULL
for(i in 1:2000){
  x1 <- rbinom(n, 1, .5)
  x <- rmvnorm(n, sigma = matrix(c(2, 0, 0, 2), 2))
  x2 <- x[, 1]
  x3 <- x[, 2]
  x4 <- rnorm(n, sd = 2)
  y <- rbinom(n, 1, plogis(x1 * 1 + 1*x2 - 1*x3 + 3*log(abs(x2)) + x4))
  fit3 <- glm(y ~ x1 + x2 + x3 + x2^2, family = 'binomial')
  fit2 <- glm(y ~ x1 + x2, family = 'binomial')
  fit1 <- glm(y ~ x1, family = 'binomial')
  pfit <- pated(y ~ x1, x2 ~ x1, x3 ~ x1, 
                x2^2 ~ x1, #x3^2 ~ x1, 
                family = c('binomial', rep('gaussian', 3)), 
                data = data.frame(y,x1,x2,x3))
  adj1 <- c(adj1, coef(fit1)['x1'])
  adj2 <- c(adj2, coef(fit2)['x1'])
  adj3 <- c(adj3, coef(fit3)['x1'])
  pat <- c(pat, pfit$estimate[1])
  
  se1 <- c(se1, robust(fit1)[,2]['x1'])
  se2 <- c(se2, robust(fit2)[,2]['x1'])
  se3 <- c(se3, robust(fit3)[,2]['x1'])
  se0 <- c(se0, pfit$stderr[1])
}

#print('adj3')
mean(adj3)# %>% print()

#print('adj2')
mean(adj2)# %>% print()

#print('unadj')
mean(adj1)# %>% print()

#print('pated')
mean(pat)# %>% print()

power1 <- power(adj1, se1)
power2 <- power(adj2, se2)
power3 <- power(adj3, se3)
power0 <- power(pat, se0)

data.table(
  method = c('PATED', 'Adj1', 'Adj2', 'Adj3'),
  mean = c(mean(pat), mean(adj1), mean(adj2), mean(adj3)),
  'bias %' = ((c(mean(pat), mean(adj1), mean(adj2), mean(adj3))/mean(adj1) - 1)*100) %>% round(1),
  sd = c(sd(pat), sd(adj1), sd(adj2), sd(adj3)),
  se = c(mean(se0), mean(se1), mean(se2), mean(se3)),
  'power %' = round(100 * c(power0, power1, power2, power3), 1)
) %>% print



}
