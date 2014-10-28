
###################################
#Statistical Inference - Coursera
###################################

#########
#Week 1  
#########

#plot of a beta probability density with a = 2, b = 1
par(mfrow = c(1,1))
x <- c(-0.5, 0,1,1,1.5)
y <- c(0,0,2,0,0)
plot(x,y, lwd = 3, frame = F, type = "l")

pbeta(.75,2,1) #.5625
pbeta(c(.4, .5, .6),2,1)
qbeta(.5,2,1) #median

x <- 1:4
p <- x/sum(x)
temp <- rbind(x, p)
rownames(temp) <- c("X", "Prob")
temp

#Lec 4
library(manipulate)
galton <- read.csv("Galton.csv")
data(galton)
myHist <- function(mu){
  g <- ggplot(galton, aes(x = child))
  g <- g + geom_histogram(fill = "salmon",
    binwidth=1, aes(y = ..density..), colour = "black")
  g <- g + geom_density(size = 2)
  g <- g + geom_vline(xintercept = mu, size = 2)
  mse <- round(mean((galton$child - mu)^2), 3)
  g <- g + labs(title = paste('mu = ', mu, ' MSE = ', mse))
  g
}

manipulate(myHist(mu), mu = slider(62, 74, step = 0.5)) #??? produces an error


########
#Week 2
########

###
#simulation example 1: variance of sample means of std normal variates (n = 10)
###
nosim <- 1000 #trials
n <- 10 #samples per trial
sd(apply(matrix(rnorm(nosim * n), nosim), 1, mean))

#exact variance
1/sqrt(n)

###
#simulation example 2: variance of sample means of std uniform variates (n = 10)
###
nosim <- 1000 #trials
n <- 10 #samples per trial
sd(apply(matrix(runif(nosim * n), nosim), 1, mean))

#exact variance:
1/sqrt(12*n)

###
#simulation example 3: variance of sample means of poisson(4) variates (n = 10)
###
nosim <- 1000 #trials
n <- 10 #samples per trial
sd(apply(matrix(rpois(nosim * n, 4), nosim), 1, mean))

#exact variance:
2/sqrt(n)

###
#simulation example 3: the sd of the mean of random sample of n fair coin flips is 1/(2*sqrt(n))
###
nosim <- 1000 #trials
n <- 10 #samples per trial
sd(apply(matrix(sample(0:1, nosim * n, replace = T), nosim), 1, mean))

#exact value
1/(2*sqrt(n))


#Data example

library(UsingR); data(father.son);
x <- father.son$sheight
n<-length(x)
round(c(var(x), var(x) / n, sd(x), sd(x) / sqrt(n)),2)


#LLN
n <- 10000
means <- cumsum(rnorm(n))/(1:n)
library(ggplot2)
g <- ggplot(data.frame(x = 1:n, y = means), aes(x = x, y = y))
g <- g + geom_hline(yintercept = 0) + geom_line(size = 2)
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
g

###LLN: coin flip

means <- cumsum(sample(0:1, n, replace = TRUE))/(1:n)
g <- ggplot(data.frame(x = 1:n, y = means), aes(x = x, y = y))
g <- g + geom_hline(yintercept = 0.5) + geom_line(size = 2)
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
g


###
#95% CI for Galtons father/son data
###
library(UsingR)
data(father.son)
x <- father.son$sheight
(mean(x) + c(-1, 1) * qnorm(0.975) * sd(x)/sqrt(length(x)))/12


###
#Lec 8 - T intervals
###
library(ggplot2)
library(manipulate)

k <- 1000
xvals <- seq(-5, 5, length = k)
myplot <- function(df){
d <- data.frame(y = c(dnorm(xvals), dt(xvals, df)),
x = xvals,
dist = factor(rep(c("Normal", "T"), c(k,k))))
g <- ggplot(d, aes(x = x, y = y))
g <- g + geom_line(size = 2, aes(colour = dist))
g
}
manipulate(myplot(mu), mu = slider(min = 1, max = 20, step = 1))


pvals <- seq(.5, .99, by = .01)
myplot2 <- function(df){
d <- data.frame(n= qnorm(pvals),t=qt(pvals, df),
p = pvals)
g <- ggplot(d, aes(x= n, y = t))
g <- g + geom_abline(size = 2, col = "lightblue")
g <- g + geom_line(size = 2, col = "black")
g <- g + geom_vline(xintercept = qnorm(0.975))
g <- g + geom_hline(yintercept = qt(0.975, df))
g
}
manipulate(myplot2(df), df = slider(1, 20, step = 1))


###sleep data: matched pairs - mean diff
g1 <- sleep$extra[1 : 10]; g2 <- sleep$extra[11 : 20]
difference <- g2 - g1
mn <- mean(difference); s <- sd(difference); n <- 10

mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n)
t.test(difference)
t.test(g2, g1, paired = TRUE)
t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)


###independent samples - diff of means
#from Rosner text on Biostats
sp <- sqrt((7 * 15.34^2 + 20 * 18.23^2) / (8 + 21 - 2))
132.86 - 127.44 + c(-1, 1) * qt(.975, 27) * sp * (1 / 8 + 1 / 21)^.5

###Chick weight data from R
library(datasets); data(ChickWeight); library(reshape2)
##define weight gain or loss
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
names(wideCW)[-(1 : 2)] <- paste("time", names(wideCW)[-(1 : 2)], sep = "")
library(dplyr)
wideCW <- mutate(wideCW,
gain = time21 - time0
)

###########################
#Lec 9 - Hypothesis Testing
###########################
library(UsingR); data(father.son)
t.test(father.son$sheight - father.son$fheight)

#using chick data from above
wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
t.test(gain ~ Diet, paired = FALSE,
var.equal = TRUE, data = wideCW14)
#try var.equal = F as well




library(UsingR); data(father.son)
t.test(father.son$sheight - father.son$fheight)

######################################################################
#Project Part I - Simulated Dist of means of 40 Exp(.2) random variates
######################################################################

#http://stackoverflow.com/questions/1395105/getting-latex-into-r-plots 

#colors to try: 8, 11, 12, 17, 19, 20, 21, 22, 23, 31, 32, 33, 36, 41, 42, 51, 52, 55, 56, 57, 61, 62, 66, 67, 72, 74, 75, 76, 80, 82

lambda = 0.2 
nosim <- 10000 #trials
n <- 40 #samples per trial

data <- matrix(rexp(nosim * n, lambda), nosim)
sample_mean <- apply(data, 1, mean)
true_mean = 1/lambda
sampling_dist_est_mean = mean(sample_mean)
rel_mean_err = abs(true_mean - sampling_dist_est_mean) / true_mean #off by roughly a few hundredths of 1% for n = 40

###Part I- Q1 (and Q3): histogram plot code; showing sample dist approx normal###

#plot of sample means; ablines at true mean and mean of sample means; limiting dist N(5, .625)
hist(sample_mean, freq = F, breaks = 25, col = colors()[82], xlab = "sample means", xlim = c(2,9),
main = "Distribution of sample means\n n = 40 random Exp(0.2) variates (10k simulations)")
abline(v = true_mean, col = "blue", lwd = 2)
abline(v = sampling_dist_est_mean, col = "green", lwd = 2)
str1 <- paste(c("Mean of sample means:\n", as.character(round(sampling_dist_est_mean,2)), "(rounded to .01)"), collapse = " ")
legend(6,.5, bty = "n", lty = "solid", col = c("blue", "green", "red"), legend = c("True mean: 5", str1, "N(5, .625)" ))
xvals <- seq(true_mean - 3.5 * true_sd , true_mean + 3.5 * true_sd , length = 250)
yvals <- dnorm(xvals, mean = true_mean, sd = true_sd)
lines(xvals, yvals, lwd = 2, col = "red")
###end Part I Q1###

##############################

### Part I, Q2: Comparison of sample mean variability to theoretical variability ###
true_sd = 1/(lambda * sqrt(n)) #about 0.791 when n = 40
sampling_dist_est_sd = sd(sample_mean)
rel_sd_err = abs(true_sd - sampling_dist_est_sd)/true_sd #off by about .5% for n = 40
### end Part I, Q2 ###

##############################

###Part I, Q3: show dist of sample means is approx normal. Using qqplot code###
quant_probs <- seq(.01, .99, length = 99)
sample_quants <- quantile(sample_mean, quant_probs)
norm_quants <- qnorm(quant_probs, mean = true_mean, sd = true_sd)
qqplot(norm_quants, sample_quants, col = "red", pch = 19, main = "Quantiles (1% - 99%): N(5. .625) ~ sample data\nplus normal line data would fit if exactly normal")
qqline(norm_quants, distribution = function(x) qnorm(x,true_mean, true_sd), col = "blue", lwd = 2)

#plot(sample_quants, norm_quants, main = "Quantiles (1% - 99%): N(5. .625) ~ sample data\nplus normal line data would fit if exactly normal", 
#xlab = "sample mean quantiles", ylab = "N(5, .625)", col = "red", pch = 19)
#lines(xvals, xvals, col = "blue", lwd = 2, lty = "solid")
#legend("topleft", pch = "-", col = c("red","blue"), legend = c("Sample data","Normal line"))

###end Part I, Q3###

##############################

### Part I, Q4: 95% CI coverage for sample mean data ###

CI_95_sample_means <- matrix(c(sample_mean - qnorm(.975) * sampling_dist_est_sd / sqrt(n), sample_mean + qnorm(.975) * sampling_dist_est_sd / sqrt(n)), nrow = nosim) 
head(CI_95_sample_means)
res <- apply(CI_95_sample_means, 1, function(x) 5 >= x[1] & 5 <= x[2] )
length(res[res == T])/nosim #percent coverage for all trials

### end Part I, Q4 ###


###########
#end Part I
###########


#############################################
#Part II - ToothGrowth data inference
#############################################

data(ToothGrowth)
summary(ToothGrowth)
names(ToothGrowth) #len, supp, dose


##########
##########
##Week 4##
##########
##########

###############
#Lec 11 - Power
###############

#Ex 1
alpha <- 0.05
mu_0 <- 30
mu_a <- 32
sdev <- 4
n <- 16
zcrit <- qnorm(.95)
z_delta <- (mu_a - mu_0)/(sdev / sqrt(n))
pow <- pnorm(zcrit - z_delta, lower.tail = F) #0.64

#Ex 2
sigma <- 10
mu_0 <- 0
mu_a <- 2
n <- 100
alpha <- 0.05
plot(c(-3,6), c(0, dnorm(0)), type = "n", frame = F, xlab = "Z value", ylab = "")
xvals <- seq(-3, 6, length = 1000)
lines(xvals, dnorm(xvals), type = "l",lwd = 3)
lines(xvals, dnorm(xvals, mean = sqrt(n)*(mu_a - mu_0)/sigma), lwd = 3)
abline(v = qnorm(1 - alpha))

#Ex3 t-test power

#all are .604
power.t.test(n = 16, delta = 2/4, sd = 1, type = "one.sample", alt = "one.sided")$power
power.t.test(n = 16, delta = 2, sd = 4, type = "one.sample", alt = "one.sided")$power
power.t.test(n = 16, delta = 100, sd = 200, type = "one.sample", alt = "one.sided")$power 

#estimate n for given power
power.t.test(power = .8, delta = .5, sd = 1, type = "one.sample", alt = "one.sided")$n #26.1


########################
#Lec 12 - Multiple Tests
########################

#Ex 1 - No true positives
set.seed(1010093)
pValues <- rep(NA, 1000)
for(i in 1:1000){
    y <- rnorm(20)
	x <- rnorm(20)
	pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}

#controls false positive rate
sum(pValues < 0.05) #51

#controls FWER
sum(p.adjust(pValues, method = "bonferroni") < 0.05) #0

#controls FDR
sum(p.adjust(pValues, method = "BH") < 0.05) #0


#Ex 2 - 50% true positives
set.seed(1010093)
pValues <- rep(NA, 1000)
for(i in 1:1000){
	x <- rnorm(20)
	#first 500 beta  = 0, last 500 beta = 2
	if(i <= 500){
	    y <- rnorm(20)
	}
	else{
	    y <- rnorm(20, mean = 2 * x)
	}
	
	pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}

trueStatus <- rep(c("zero","not zero"), each = 500)
table(pValues < .05, trueStatus)

#controls FWER
table(p.adjust(pValues, method = "bonferroni") < .05, trueStatus)

#controls FDR
table(p.adjust(pValues, method = "BH") < .05, trueStatus)

par(mfrow = c(1,2))
plot(pValues, p.adjust(pValues, method = "bonferroni"), pch = 19)
plot(pValues, p.adjust(pValues, method = "BH"), pch = 19)

#############################################
#Lec 13 - Resampling: Jackknife and Bootstrap
#############################################

par(mfrow = c(1,1))

#Ex 1 - Estimate bias and std error of median
library(UsingR)
data(father.son)
x <- father.son$sheight
n <- length(x)
theta <- median(x)
jk <- sapply(1:n, function(i) median(x[-i]))
thetaBar <- mean(jk)
biasEst <- (n-1)*(thetaBar - theta)
seEst <- sqrt((n-1) * mean((jk - thetaBar)^2))
c(biasEst,seEst)

library(bootstrap)
temp <- jackknife(x, median)
c(temp$jack.bias, temp$jack.se)

#Ex 2 - nonparametric bootstrap algo for median of above data set
B <- 1000
resamples <- matrix(sample(x, n*B, replace = T), B, n)
medians <- apply(resamples, 1, median)
sd(medians)
quantile(medians, c(.025, .975))
hist(medians)


#Ex 3 - Group comparisons: Permutation test on sprays B and C
data(InsectSprays)
boxplot(count ~ spray, data = InsectSprays)
subdata <- InsectSprays[InsectSprays$spray %in% c("B", "C"),]
y <- subdata$count
group <- as.character(subdata$spray)
testStat <- function(w,g) mean(w[g == "B"]) - mean(w[g == "C"])
observedStat <- testStat(y, group)
permutations <- sapply(1:10000, function(i) testStat(y, sample(group)))
observedStat
mean(permutations > observedStat)
hist(permutations)

#######
#Quiz 4
#######

#Question 1
g1 <- c(140, 138, 150, 148, 135)
g2 <- c(132, 135, 151, 146, 130)
diffs <- g2 - g1
t.test(diffs)


#Question 2
n <- 9 
xbar <- 1100
sdev <- 30
xbar + c(-1,1) * qt(.975, n - 1) * sdev / sqrt(n)


#Question 4 poisson example
#lambda = 0.01 is benchmark rate
lambda_0 <- 0.01
events <- 10
interval <- 1787
poisson.test(events, interval, lambda_0 , alternative = "less")

#Question 5 assume normal data and common variance
n1 <- 9
n2 <- 9
m1 <- -3
m2 <- 1
s1 <- 1.5
s2 <- 1.8

sp <- sqrt(.5*(s1^2 + s2^2))
se <- sp * sqrt(1/n1 + 1/n2)
tstat <- (m1 - m2)/ se #-5.12
pval <- pt(tstat, n1 + n2 - 2) * 2 #.0001


#Question 7
n <- 100
d <- .01
sdev <- .04
zdelta <- sqrt(n) * d/sdev
alpha <- .05
zcrit <- qnorm(1 - alpha)
pow <- pnorm(zcrit - zdelta, lower.tail = F)

z10 <- qnorm(.1)
n <- ((zcrit - z10) * sdev / d)^2 #about 137



#Question 8
pow <- .9
d <- .01
sdev <- .04
alpha <- .05
zcrit <- qnorm(1 - alpha)
z10 <- qnorm(.1)
n <- ((zcrit - z10) * sdev / d)^2 #about 137



#Question 10

#sample from Metropolis
n1 <- 288
xbar1 <- 44
sdev1 <- 12

#sample from Gotham City
n2 <- 288
xbar2 <- 42.04
sdev2 <- 12

sp <- sqrt(.5 * (sdev1 ^ 2 + sdev2 ^ 2))
se <- sp * sqrt(1 / n1 + 1 / n2)

zstat <- (xbar1 - xbar2)/(se)
pval <- 2 * pnorm(zstat, lower.tail = F) #.05
