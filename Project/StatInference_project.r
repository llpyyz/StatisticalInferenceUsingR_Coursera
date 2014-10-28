################################################
#Introduction to Statistical Inference - Project
#Author : David Schonberger
#Created: 8/22/2014
################################################

########################################################################
#Part I:
#Illustrate via simulation and associated explantory text the properties
#of the distribution of the mean of 40 exponential(0.2) random variables.
########################################################################

nosim <- 100000 #trials
n <- 40 #samples per trial
lambda <- 0.2
data <- matrix(rexp(nosim * n, lambda), nosim)
sample_means <- apply(data, 1, mean)
true_mean <- 1/lambda
estimated_mean <- mean(sample_means)
true_sd <- 1/(lambda * sqrt(n)) #about 0.791 when n = 40
estimated_sd <- sd(sample_means)

hist(sample_means, freq = F, col = colors()[82], breaks = 30, xlab = "sample means", main = "Estimated Sampling Distribution of Sample Means\n10K Simulations of 40 Exp(0.2) rv's")
abline(v = true_mean, col = "blue", lwd = 2)
abline(v = estimated_mean, col = "green", lwd = 2)
str1 <- paste(c("Mean of sample means:\n", as.character(round(estimated_mean,2)), "(rounded to .01)"), collapse = " ")
legend(6,.5, bty = "n", lty = "solid", col = c("blue", "green", "red"), legend = c("True mean: 5", str1, "N(5, .625)" ))
xvals <- seq(true_mean - 3.5 * true_sd , true_mean + 3.5 * true_sd , length = 250)
yvals <- dnorm(xvals, mean = true_mean, sd = true_sd)
lines(xvals, yvals, lwd = 2, col = "red")

#95% CI coverage
sample_sds <- apply(data, 1, sd)
CIs_sample_means <- matrix(c(sample_means - qnorm(.975) * sample_sds / sqrt(n), sample_means + qnorm(.975) * sample_sds / sqrt(n)), nrow = nosim) 
head(CIs_sample_means)
res <- apply(CIs_sample_means, 1, function(x) 5 >= x[1] & 5 <= x[2] )
length(res[res == T])/nosim #percent coverage for all trials


###############################
#Part II:
#Analyzing ToothGrowth data set
###############################

#http://stat.ethz.ch/R-manual/R-devel/library/datasets/html/ToothGrowth.html
#Content of data set: 
#The Effect of Vitamin C on tooth growth in guinea pigs. 
#60 obs of 3 vars: 10 guinea pigs at each of three dose levels delivered in
#two ways: orange juice and ascorbic acid
#
#1. len - numeric tooth length
#2. supp - supplement type VC or OJ
#3. dose - numeric does in mg
data(ToothGrowth)
summary(ToothGrowth)
names(ToothGrowth)

vc_halfmg <- ToothGrowth[1:10,1]
oj_halfmg <- ToothGrowth[31:40,1]
diff_halfmg <- vc_halfmg - oj_halfmg

vc_onemg <- ToothGrowth[11:20,1]
oj_onemg <- ToothGrowth[41:50,1]
diff_onemg <- vc_onemg - oj_onemg

vc_twomg <- ToothGrowth[21:30,1]
oj_twomg <- ToothGrowth[51:60,1]
diff_twomg <- vc_twomg - oj_twomg

#From the plot, it appears there may be a difference in tooth growth vs supp type for dose = 0.5 and dose = 1
#but probably no difference when dose = 2
ggplot(ToothGrowth, aes(supp, len)) + geom_boxplot() + facet_grid(. ~ dose) + labs(y = "length") +
labs(x = "supplement type") + labs(title = "Tooth growth in guinea pigs\nLength v. type of supplement; broken out by dosage amount")

#1. Assuming data ordered, i.e. the same 10 guinea pigs in same order in each of the six combinations of supp type and dose

#2. Also assuming a washout period between treatments.

#3. Since the samples are small, we use a t-distribution rather than a z-dist for our model.

#Thus we do a paired data hypothesis test/CI for mean diff in length for the two treatments.
#With three distinct doses we do three such paired tests/CIs.


CI_halfmg <- t.test(diff_halfmg)$conf
pval_halfmg <- t.test(diff_halfmg)$p.value

CI_onemg <- t.test(diff_onemg)$conf
pval_onemg <- t.test(diff_onemg)$p.value

CI_twomg <- t.test(diff_twomg)$conf
pval_twomg <- t.test(diff_twomg)$p.value


CI_halfmg <- t.test(diff_halfmg, alternative = "less")$conf
pval_halfmg <- t.test(diff_halfmg,alternative = "less")$p.value
