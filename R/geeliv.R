
# 
# library(dplyr)
# library(momentfit)
# library(mvtnorm)
# set.seed(NULL)
# res <- NULL
# for(i in 1:1){
# n <- 5000
# z <- rbinom(n, 1, .5) # random assignment
# x <- rmvnorm(n, sigma = matrix(c(1, .5, .5, 1), 2))
# a <- rbinom(n, 1, plogis(0 + 3 * (2*z-1) + .6 * x[,1]))
# y <- 5*x[,1] + 1.5 * a + rnorm(n)
# data <- data.frame(y, z, x=x[,1], a)
# table(a,z)
# mean(a)
# mean(z)
# 
# res <- rbind(res, 
# data.frame(
#   z=t.test(x[,1]~z)$p.value, a=t.test(x[,1]~a)$p.value
# ))
# }
# 
# apply(res, 2, function(pr) {mean(pr < .05)}) %>% print()
# 
# workingModel <- function(b, z, x, a){
#   
#   gam0 <- b[4]
#   gam1 <- b[5]
#   gam2 <- b[6]
#   lin <- gam0 + gam1 * x + gam2 * z
#   exp(a * lin) / (1 + exp(lin))
#   
# }
# 
# conditionalProbabilityOfZ <- function(b, z, x, a){
#   
#   workingModel(b, z = z, x = x, a = a) / (workingModel(b, z = 0, x = x, a = a) + workingModel(b, z = 1, x = x, a = a))
#   
# }
# 
# moment3 <- function(b, resid, z){
#   resid * z
# }
# moment4 <- function(b, z, x, a){
#   a - workingModel(b, z = z, x = x, a = 1)
# }
# 
# moment5 <- function(b, z, x, a){
#   (a - workingModel(b, z = z, x = x, a = 1)) * x
# }
# 
# moment6 <- function(b, z, x, a){
#   (a - workingModel(b, z = z, x = x, a = 1)) * z
# }
# 
# moment7 <- function(z){
#   z - 1/2
# }
# 
# moment8 <- function(z, x){
#   x * (z - 1/2)
# }
# 
# moments <- function(b, data){
#   
#   y <- data$y
#   x <- data$x
#   a <- data$a
#   
#   bet0 <- b[1]
#   bet1 <- b[2]
#   bet2 <- b[3]
#   resid <- y - (bet0 + bet1 * x + bet2 * a)
#   u1 <- resid
#   u2 <- resid * x
#   u3 <- 
#     
#       moment3(b, resid, z = 0) * conditionalProbabilityOfZ(b, z = 0, x = x, a = a) + 
#         moment3(b, resid, z = 1) * conditionalProbabilityOfZ(b, z = 1, x = x, a = a)
#     
#   
#   u4 <- 
#     
#       moment4(b, z = 0, x = x, a = a) * conditionalProbabilityOfZ(b, z = 0, x = x, a = a) + 
#         moment4(b, z = 1, x = x, a = a) * conditionalProbabilityOfZ(b, z = 1, x = x, a = a)
#     
#   
#   u5 <- 
#     
#       moment5(b, z = 0, x = x, a = a) * conditionalProbabilityOfZ(b, z = 0, x = x, a = a) + 
#         moment5(b, z = 1, x = x, a = a) * conditionalProbabilityOfZ(b, z = 1, x = x, a = a)
#     
#   
#   u6 <- 
#     
#       moment6(b, z = 0, x = x, a = a) * conditionalProbabilityOfZ(b, z = 0, x = x, a = a) + 
#         moment6(b, z = 1, x = x, a = a) * conditionalProbabilityOfZ(b, z = 1, x = x, a = a)
#     
#   
#   u7 <- 
#     
#       moment7(z = 0) * conditionalProbabilityOfZ(b, z = 0, x = x, a = a) + 
#         moment7(z = 1) * conditionalProbabilityOfZ(b, z = 1, x = x, a = a)
#     
#   
#   u8 <- 
#     
#       moment8(z = 0, x = x) * conditionalProbabilityOfZ(b, z = 0, x = x, a = a) + 
#         moment8(z = 1, x = x) * conditionalProbabilityOfZ(b, z = 1, x = x, a = a)
#     
#   
#   ret <- cbind(u1, u2, u3, u4, u5, u6, u7, u8)
#   
#   #print(b)
#   
#   sum(apply(ret, 2, mean)^2)
# }
# 
# #set.seed(2)
# #fit <- gmm4(moments, x = data, theta0 = rnorm(6), type = 'twostep')
# 
# #set.seed(1)
# t0 <- rnorm(6)
# t00 <- t0
# 
# fn <- NULL
# while(TRUE){
#   print('eps: ')
#   print(eps)
#   print('t0: ')
#   print(t0)
#   print('fn')
#   fn <- c(fn, moments(t0, data))
#   print(tail(fn, 1))
#   print('================')
#   g <- grad(moments, t0, data = data)
#   t0 <- t0 - .5 * g
#   eps <- max(abs(.5 * g))
#   if(eps < 1e-4){
#     break
#   }
# }
# 
# 
# moments(t1, data)
