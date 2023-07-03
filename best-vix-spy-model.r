#code assumes objects from project_spdr_spy.r are loaded!!
par(mar=c(1.5,1.5,1.5,1.5))
par(mfrow=c(1,1))
#see acf
acf(project_data$VIX.Adjusted) #decreasing trend

#see plot
plot(project_data$VIX.Adjusted) #hmmm, nothing really but plot doesn't exhibit stationarity

#adf test
adf.test(project_data$VIX.Adjusted) #says stationary but we can still have non constant variance and non stationarity

#adf test can only check if it has a unit root, not all conditions

#since the acf exhibits a trend, we do a difference
vix_adjusted_diff = na.omit(diff(project_data$VIX.Adjusted))

#we see some jumps in variance like spy, we do a log-diff instead
vix_adjusted_log_diff = na.omit(diff(log(project_data$VIX.Adjusted)))

acf(vix_adjusted_diff) #very good now
adf.test(vix_adjusted_diff) #looks good
plot(vix_adjusted_diff) #much more stabilized
acf(vix_adjusted_log_diff) #great
adf.test(vix_adjusted_log_diff)# looks good
plot(vix_adjusted_log_diff) # much better

#simple ols with diff
vix_spy_lm1 = lm(project_data$LogReturns.Adjusted[-1] ~ vix_adjusted_diff)
summary(vix_spy_lm1)#significant at 0.05, adj r-square is 0.6338
plot(vix_spy_lm1) #scale-location is v shaped.
broom::glance(vix_spy_lm1) #adj-r = 0.634, aic=-51378, bic=-51357

#simple weighted ols with diff
vix_spy_lm1_weighted = lmrob(project_data$LogReturns.Adjusted[-1] ~ vix_adjusted_diff)
summary(vix_spy_lm1_weighted) #significant at 0.05, r-square is 0.7039
plot(vix_spy_lm1_weighted)
broom::glance(vix_spy_lm1_weighted) #r.squared 0.704, no aic/bic.

#simple ols with logs/diff
vix_spy_lm1_log = lm(project_data$LogReturns.Adjusted[-1] ~ vix_adjusted_log_diff)
summary(vix_spy_lm1_log)#significant at 0.05, adj r-square is 0.5206
plot(vix_spy_lm1_log) #scale-location is v shaped. Problem with outliers
broom::glance(vix_spy_lm1_log) #aic=-49403 and bic=-49382

#simple weighted ols with logs/diff
vix_spy_lm1_weighted_log = lmrob(project_data$LogReturns.Adjusted[-1] ~ vix_adjusted_log_diff)
summary(vix_spy_lm1_weighted_log) #significant at 0.05, r-square is 0.5918
plot(vix_spy_lm1_weighted_log) 
broom::glance(vix_spy_lm1_weighted_log) #r.squared 0.592, no aic/bic.


#check residuals for all. I think acf here is really pacf
checkresiduals(vix_spy_lm1_weighted_log) #correlated, don't look normal
checkresiduals(vix_spy_lm1_weighted) # pretty much similar as previous one
checkresiduals(vix_spy_lm1) # not good
checkresiduals(vix_spy_lm1_log) #not good


#let's focus on acf/pacf for the residuals of weighted log
pacf(resid(vix_spy_lm1_weighted_log)) #big spike at lag 1, p=1
acf(resid(vix_spy_lm1_weighted_log)) #small spikes not too big

#we should actually check pacf/acf of simple ols because gls doesn't seem to connect to lmrob
pacf(resid(vix_spy_lm1_log)) #big spikes at lag 1, p=1
acf(resid(vix_spy_lm1_log)) #small spikes not too big


#errors look like aR(1)
#try gls for correlated errors
library(nlme)
LogReturns.Adjusted = as.numeric(project_data$LogReturns.Adjusted[-1])
vix_spy_lm1_gls_log = gls(LogReturns.Adjusted ~ as.numeric(vix_adjusted_log_diff), 
                          correlation = corARMA(p=1)) #aic=-49422 and bic=-49394
summary(vix_spy_lm1_gls_log)
plot(vix_spy_lm1_gls_log)
hist(resid(vix_spy_lm1_gls_log))
qqnorm(resid(vix_spy_lm1_gls_log))
qqline(resid(vix_spy_lm1_gls_log)) 
shapiro.test(resid(vix_spy_lm1_gls_log)[1:5000]) #not really normal

#ccf of vix and spy
ccf(as.numeric(LogReturns.Adjusted), as.numeric(vix_adjusted_log_diff))#  really just one lag
acf(LogReturns.Adjusted) #we have a small spike at q=1
pacf(LogReturns.Adjusted) # large spike at p=1

#lets try spy~ with lagged spy and vi (no gls this time)
vix_spy_lm1_gls_log_lagspy1 = lm(LogReturns.Adjusted ~ as.numeric(vix_adjusted_log_diff) 
                          + lag(LogReturns.Adjusted), 
                          ) 
summary(vix_spy_lm1_gls_log_lagspy1) #wow!
plot(vix_spy_lm1_gls_log_lagspy1) # wow!
acf(resid(vix_spy_lm1_gls_log_lagspy1)) #almost white noise!
#is this really this good?
vif(vix_spy_lm1_gls_log_lagspy1) # good! All < 5. So no multicollinearity.
broom::glance(vix_spy_lm1_gls_log_lagspy1)#VERY low aic and bic
shapiro.test(rstandard(vix_spy_lm1_gls_log_lagspy1)[1:5000])#hmm, not normal but can't be relied upon!
#with such a large sample size, we can assume the mean is normally distributed.
