library(mice)
library(JointAI)
##1(a)
md_pattern(nhanes, pattern = FALSE, color = c('#34111b', '#e30f41'))
nrow(nhanes)

imp = mice(nhanes, seed = 1, printFlag = FALSE)
imp
fit =  with(imp, lm(bmi ~ age + hyp + chl))

pooled_ests = pool(fit)
pooled_ests


##1(b)
ests_seed2 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 2), lm(bmi ~ age + hyp + chl)))
ests_seed3 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 3), lm(bmi ~ age + hyp + chl)))
ests_seed4 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 4), lm(bmi ~ age + hyp + chl)))
ests_seed5 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 5), lm(bmi ~ age + hyp + chl)))
ests_seed6 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 6), lm(bmi ~ age + hyp + chl)))

seed1 = pooled_ests$pooled$lambda
seed2 = ests_seed2$pooled$lambda
seed3 = ests_seed3$pooled$lambda
seed4 = ests_seed4$pooled$lambda
seed5 = ests_seed5$pooled$lambda
seed6 = ests_seed6$pooled$lambda

lambda = data.frame(seed1,
                    seed2,
                    seed3,
                    seed4,
                    seed5,
                    seed6,
                    row.names = c('beta0', 'beta1', 'beta2', 'beta3'))
lambda


##1(c)
ests_seed1_100 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 1, m = 100), lm(bmi ~ age + hyp + chl)))
ests_seed2_100 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 2, m = 100), lm(bmi ~ age + hyp + chl)))
ests_seed3_100 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 3, m = 100), lm(bmi ~ age + hyp + chl)))
ests_seed4_100 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 4, m = 100), lm(bmi ~ age + hyp + chl)))
ests_seed5_100 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 5, m = 100), lm(bmi ~ age + hyp + chl)))
ests_seed6_100 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 6, m = 100), lm(bmi ~ age + hyp + chl)))

seed1_100 = ests_seed1_100$pooled$lambda
seed2_100 = ests_seed2_100$pooled$lambda
seed3_100 = ests_seed3_100$pooled$lambda
seed4_100 = ests_seed4_100$pooled$lambda
seed5_100 = ests_seed5_100$pooled$lambda
seed6_100 = ests_seed6_100$pooled$lambda

lambda_100 = data.frame(seed1_100,
                        seed2_100,
                        seed3_100,
                        seed4_100,
                        seed5_100,
                        seed6_100,
                        row.names = c('beta0', 'beta1', 'beta2', 'beta3'))
lambda_100


##1(d)
summary(pooled_ests); summary(ests_seed1_100)
summary(ests_seed2); summary(ests_seed2_100)
summary(ests_seed3); summary(ests_seed3_100)




##2
data2 = dataex2

por_ci = function(data, seed){
  nob = 0
  boo = 0
  i = 1
  while (i <= 100) {
    imps.nob = mice(data[,, i], method = 'norm.nob', m = 20, printFlag = FALSE, seed = seed)
    imps.boo = mice(data[,, i], method = 'norm.boot', m = 20, printFlag = FALSE, seed = seed)
    fits.nob = with(imps.nob, lm(Y ~ X))
    fits.boo = with(imps.boo, lm(Y ~ X))
    ests.nob = pool(fits.nob)
    ests.boo = pool(fits.boo)
    a = summary(ests.nob, conf.int = TRUE)[2, c(7)]
    b = summary(ests.nob, conf.int = TRUE)[2, c(8)]
    x = summary(ests.boo, conf.int = TRUE)[2, c(7)]
    y = summary(ests.boo, conf.int = TRUE)[2, c(8)]
    if (a <= 3 & 3 <= b){
      nob = nob + 1
    }
    if (x <= 3 & 3 <= y){
      boo = boo + 1
    }
    i = i + 1
  }
  return(c(nob/100, boo/100))
}

por_ci(data2, seed = 1)


len_ci = function(data){
  i = 1
  len = matrix(0, 100, 2)
  while (i <= 100) {
    imps.nob = mice(data[,, i], method = 'norm.nob', m = 20, printFlag = FALSE, seed = 1)
    imps.boo = mice(data[,, i], method = 'norm.boot', m = 20, printFlag = FALSE, seed = 1)
    fits.nob = with(imps.nob, lm(Y ~ X))
    fits.boo = with(imps.boo, lm(Y ~ X))
    ests.nob = pool(fits.nob)
    ests.boo = pool(fits.boo)
    a = summary(ests.nob, conf.int = TRUE)[2, c(7)]
    b = summary(ests.nob, conf.int = TRUE)[2, c(8)]
    c = abs(a-b)
    x = summary(ests.boo, conf.int = TRUE)[2, c(7)]
    y = summary(ests.boo, conf.int = TRUE)[2, c(8)]
    z = abs(x-y)
    len[i,] = c(c, z)
    i = i + 1
  }
  return(len)
}

len = len_ci(data2)
boxplot(len, names = c("S.Regression", "Bootstrap"))




##4(a)
data4 = dataex4

imps = mice(data4, m = 50, printFlag = FALSE, seed = 1)
fits = with(imps, lm(y ~ x1 + x2 + (x1*x2)))
ests = pool(fits)
plot(imps, layout = c(2,2))
summary(ests, conf.int = TRUE)[, c(7,8)]


##4(b)
x12 = data4$x1*data4$x2
data4.new = cbind(data4, x12)

imp0 = mice(data4.new, maxit = 0)
meth = imp0$method
meth['x12'] = '~I(x1*x2)'


pred = imp0$predictorMatrix
pred[c('x1', 'x2'), 'x12'] = 0

imp = mice(data4.new, method = meth, predictorMatrix = pred, m = 50, seed = 1, printFlag = FALSE)
fits = with(imp, lm(y ~ x1 + x2 + x12))
ests = pool(fits)
plot(imp, layout = c(2,3))
ci95 = summary(ests, conf.int = TRUE)[, c(7,8)]
rownames(ci95) = c('beta0','beta1','beta2','beta3')
ci95


##4(c)
imp = mice(data4.new, m = 50, seed = 1, printFlag = FALSE)
fits = with(imp, lm(y ~ x1 + x2 + x12))
ests = pool(fits)
plot(imp, layout = c(2,3))
ci95 = summary(ests, conf.int = TRUE)[, c(7,8)]
rownames(ci95) = c('beta0','beta1','beta2','beta3')
ci95


##5
data5 = NHANES2
md_pattern(data5, pattern = FALSE, color = c('#34111b', '#e30f41'))

par(mar = c(3, 3, 2, 1), mgp = c(2, 0.6, 0))
plot_all(data5, breaks = 30, ncol = 4)

imp0 = mice(data5, maxit = 0)
meth = imp0$method
meth['hgt'] = 'norm'

post = imp0$post
post['hgt'] = 'imp[[j]][,i] <- squeeze(imp[[j]][,i], c(0, 5))'

imp <- mice(data5, method = meth, maxit = 20, m = 30, seed = 1, printFlag = FALSE)
imp$loggedEvents

plot(imp, layout = c(4, 4))
densityplot(imp)

require(devtools)
require(reshape2)
require(RColorBrewer)
require(ggplot2)
source_url("https://gist.githubusercontent.com/NErler/0d00375da460dd33839b98faeee2fdab/raw/c6f537ecf80eddcefd94992ec7926aa57d454536/propplot.R")

propplot(imp)
xyplot(imp, hgt ~ wgt | gender, pch = c(1, 20))
fit = with(imp, lm(wgt ~ gender + age + hgt + WC))

comp1 <- complete(imp, 1)
plot(fit$analyses[[1]]$fitted.values, residuals(fit$analyses[[1]]), xlab = "Fitted values", ylab = "Residuals")

qqnorm(rstandard(fit$analyses[[1]]), xlim = c(-4, 4), ylim = c(-6, 6))
qqline(rstandard(fit$analyses[[1]]), col = 2)

pooled_ests <- pool(fit)
summary(pooled_ests, conf.int = TRUE)