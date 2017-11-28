# Load data
click_package_exit_rate <- read.csv("Click_Package_Exit_Rate.csv")
click_package_exit_rate_data <- data.frame(click_package_exit_rate)
cperd_clean <- subset(click_package_exit_rate_data, click_package_exit_rate_data$visitors_package >= 10)

# Assign names to columns
# names(click_package_exit_rate_data) <- c("nugget_id", "quick_exit_nugget", "visitors_nugget", "clickability_test_id", "click_package_id", "quick_exit_package", "visitors_package")

# Compute quick exit rates
# click_package_exit_rate_data$quick_exit_rate_nugget <- round(((click_package_exit_rate_data$quick_exit_nugget/click_package_exit_rate_data$visitors_nugget)*100), digits = 2)
# click_package_exit_rate_data$quick_exit_rate_package <- round(((click_package_exit_rate_data$quick_exit_package/click_package_exit_rate_data$visitors_package)*100), digits = 2)
# click_package_exit_rate_data$quick_exit_rate_diff <- click_package_exit_rate_data$quick_exit_rate_package - click_package_exit_rate_data$quick_exit_rate_nugget

# Descriptive Stats
desc_stats <- function(x) {c(mean(x), sd(x), var(x))}
desc_stats(click_package_exit_rate_data$quick_exit_rate_nugget)
desc_stats(click_package_exit_rate_data$quick_exit_rate_package)
desc_stats(click_package_exit_rate_data$quick_exit_rate_diff)

# Plot histgrams
hist(click_package_exit_rate_data$quick_exit_rate_nugget)
hist(click_package_exit_rate_data$quick_exit_rate_package)
hist(click_package_exit_rate_data$quick_exit_rate_diff)


# Get mean quick_exit_rate, var, and num click packages per nugget
library("SDMTools")
nugget_id_unique <- (as.vector(levels(cperd_clean$nugget_id)))
clickability_id_unique <- (as.vector(levels(cperd_clean$clickability_test_id)))
cp_mean <- as.vector(round(tapply(cperd_clean$quick_exit_rate_package, cperd_clean$nugget_id, mean), digits = 3))
# cp_mean_weighted <- as.vector(tapply(click_package_exit_rate_data$quick_exit_rate_package[click_package_exit_rate_data$visitors_package >= 10], click_package_exit_rate_data$nugget_id[click_package_exit_rate_data$visitors_package >= 10], wt.mean(wt = click_package_exit_rate_data$visitors_package), digits = 3))
cp_var <- as.vector(round(tapply(cperd_clean$quick_exit_rate_package, cperd_clean$nugget_id, var), digits = 3))
# cp_var_weighted <- as.vector(tapply(click_package_exit_rate_data$quick_exit_rate_package[click_package_exit_rate_data$visitors_package >= 10], click_package_exit_rate_data$nugget_id[click_package_exit_rate_data$visitors_package >= 10], var))
cp_len <- as.vector(tapply(cperd_clean$quick_exit_rate_package, cperd_clean$nugget_id, length))
cp_len_1 <- as.vector(tapply(cperd_clean$quick_exit_rate_package, cperd_clean$clickability_test_id, length))
true_value <- as.vector(tapply(click_package_exit_rate_data$quick_exit_rate_nugget, click_package_exit_rate_data$nugget_id, mean))
quick_exit_tests <- data.frame(nugget_id_unique, cp_mean, cp_var, cp_len, true_value)
quick_exit_tests_1 <- data.frame(clickability_id_unique, cp_len_1)


# Calculate t-values and p-values per nugget
quick_exit_tests$t_value <- round(((cp_mean - true_value)/sqrt(cp_var/cp_len)), digits = 3)
quick_exit_tests$p_value_t <- round(2*pt(-abs(quick_exit_tests$t_value), df=cp_len-1), digits = 3)
# quick_exit_tests$t_value_weighted <- round(((cp_mean_weighted - true_value)/sqrt(cp_var_weighted/cp_len)), digits = 3)
# quick_exit_tests$p_value_weighted <- round(2*pt(-abs(quick_exit_tests$t_value_weighted), df=cp_len-1), digits = 3)

# Plot histograms
hist(quick_exit_tests$cp_mean)
hist(quick_exit_tests$true_value)
hist(quick_exit_tests$t_value)
hist(quick_exit_tests$p_value_t)

# One-sample t-tests comparing quick_exit rate across click packages to "true" quick_exit rate

# By nugget 
library(plyr)

colnames(quick_exit_tests)[1] <- "nugget_id"
cperd_clean_1 <- join(cperd_clean, quick_exit_tests, by = "nugget_id", type = "left")
cperd_clean_1 <- filter(cperd_clean_1, cp_len > 1)

t.test.plyr <- function(x, var, mu) 
  {y <- rep(NA, 3) 
  if(nrow(x) < 2) {return(y)} 
  res <- t.test(x[var], mu = [1,mu]) 
  y[1] = round(res$statistic, digits = 3) 
  y[2] = round(res$p.value, digits = 3) 
  y[3] = res$p.value 
  names(y) = c("t-value", "p-value", "df") y}

result_t <- ddply(cperd_clean, .(nugget_id), t.test.plyr, "quick_exit_rate_package", "quick_exit_rate_nugget")

# By clickability test
library(plyr)

colnames(quick_exit_tests_1)[1] <- "clickability_test_id"
cperd_clean_2 <- join(cperd_clean, quick_exit_tests_1, by = "clickability_test_id", type = "left")
cperd_clean_2 <- filter(cperd_clean_2, cp_len_1 > 1)

t.test.plyr <- function(x, var, mu) 
{y <- rep(NA, 3) 
if(nrow(x) < 2) {return(y)} 
res <- t.test(x[,var], mu = [1,mu]) 
y[1] = round(res$statistic, digits = 3) 
y[2] = round(res$p.value, digits = 3) 
y[3] = res$p.value 
names(y) = c("t-value", "p-value", "df") y}

result_t2 <- ddply(cperd_clean, .(clickability_test_id), t.test.plyr, "quick_exit_rate_package", "quick_exit_rate_nugget")


# tapply(click_package_exit_rate_data$quick_exit_rate_package, click_package_exit_rate_data$nugget_id, FUN = function(x) t.test(click_package_exit_rate_data$quick_exit_rate_package[click_package_exit_rate_data$visitors_package >= 10], mu=mean(click_package_exit_rate_data$quick_exit_rate_nugget, na.rm = TRUE)))
# by(click_package_exit_rate_data, click_package_exit_rate_data$nugget_id, FUN = function(x) t.test(click_package_exit_rate_data$quick_exit_rate_package[click_package_exit_rate_data$visitors_package >= 10], mu=mean(click_package_exit_rate_data$quick_exit_rate_nugget, na.rm = TRUE)))
# for(i in 1:length(click_package_exit_rate_data$nugget_id)) {t.test(click_package_exit_rate_data$quick_exit_package[click_package_exit_rate$nugget_id[i]], mu = mean(click_package_exit_rate_data$quick_exit_rate_nugget[click_package_exit_rate_data$nugget_id[i]], na.rm = TRUE))}
# for(i in 1:length(levels(click_package_exit_rate_data$nugget_id))) {t.test(click_package_exit_rate_data$quick_exit_package[levels(click_package_exit_rate_data$nugget_id)[i]], mu = mean(click_package_exit_rate_data$quick_exit_rate_nugget[levels(click_package_exit_rate_data$nugget_id)[i]], na.rm = TRUE))}

# Weighted t-tests (by # visitors per click package)
# library("weights")
# tapply(click_package_exit_rate_data$quick_exit_rate_package, click_package_exit_rate_data$nugget_id, FUN = function(x) wtd.t.test(click_package_exit_rate_data$quick_exit_rate_package[click_package_exit_rate_data$visitors_package >= 10], y=mean(click_package_exit_rate_data$quick_exit_rate_nugget, na.rm = TRUE), weight=click_package_exit_rate_data$visitors_package[click_package_exit_rate_data$visitors_package >= 10]))
# by(click_package_exit_rate_data, click_package_exit_rate_data$nugget_id, FUN = function(x) wtd.t.test(click_package_exit_rate_data$quick_exit_rate_package[click_package_exit_rate_data$visitors_package >= 10], y=mean(click_package_exit_rate_data$quick_exit_rate_nugget, na.rm = TRUE), weight=click_package_exit_rate_data$visitors_package[click_package_exit_rate_data$visitors_package >= 10]))
# for(i in 1:length(click_package_exit_rate_data$nugget_id)) {wtd.t.test(click_package_exit_rate_data$quick_exit_package[click_package_exit_rate$nugget_id[i]], y = mean(click_package_exit_rate_data$quick_exit_rate_nugget[click_package_exit_rate_data$nugget_id[i]], na.rm = TRUE), weight=click_package_exit_rate_data$visitors_package[click_package_exit_rate_data$visitors_package >= 10])}

# Get p-values and df from t-tests
# pvalues_unweighted <- c(tapply(click_package_exit_rate_data$quick_exit_rate_package, click_package_exit_rate_data$nugget_id, FUN = function(x) t.test(click_package_exit_rate_data$quick_exit_rate_package[click_package_exit_rate_data$visitors_package >= 10], mu=mean(click_package_exit_rate_data$quick_exit_rate_nugget, na.rm = TRUE))$p.value))
# df_unweighted <- c(tapply(click_package_exit_rate_data$quick_exit_rate_package, click_package_exit_rate_data$nugget_id, FUN = function(x) t.test(click_package_exit_rate_data$quick_exit_rate_package[click_package_exit_rate_data$visitors_package >= 10], mu=mean(click_package_exit_rate_data$quick_exit_rate_nugget, na.rm = TRUE))$parameter))
# pvalues_weighted <- tapply(click_package_exit_rate_data$quick_exit_rate_package, click_package_exit_rate_data$nugget_id, FUN = function(x) wtd.t.test(click_package_exit_rate_data$quick_exit_rate_package[click_package_exit_rate_data$visitors_package >= 10], y=mean(click_package_exit_rate_data$quick_exit_rate_nugget, na.rm = TRUE), weight=click_package_exit_rate_data$visitors_package[click_package_exit_rate_data$visitors_package >= 10])$p.value)
# df_weighted <- tapply(click_package_exit_rate_data$quick_exit_rate_package, click_package_exit_rate_data$nugget_id, FUN = function(x) wtd.t.test(click_package_exit_rate_data$quick_exit_rate_package[click_package_exit_rate_data$visitors_package >= 10], y=mean(click_package_exit_rate_data$quick_exit_rate_nugget, na.rm = TRUE), weight=click_package_exit_rate_data$visitors_package[click_package_exit_rate_data$visitors_package >= 10])$parameter)
# fisher_unweighted <- data.frame(pvalues_unweighted,df_unweighted)
# fisher_weighted <- data.frame(pvalues_weighted,df_weighted)


# Chi-squared tests


library(plyr)

prop.test.plyr <- function(x, successes, trials){
  
  res1_chisq <- prop.test(x[,successes], x[,trials])$statistic
  res2_chisq <- prop.test(x[,successes], x[,trials])$p.value  
  res3_chisq <- prop.test(x[,successes], x[,trials])$parameter
  res1_chisq <- round(res1_chisq, digits = 3)
  res2_chisq <- round(res2_chisq, digits = 3)
  res_all_chisq <- c(res1_chisq,res2_chisq,res3_chisq)
  names(res_all_chisq) <- c("chi_square", "p_value", "df")
  res_all_chisq
}

# By nugget

result_chisq <- ddply(.data = cperd_clean, 
                .variables = .(nugget_id), 
                .fun = prop.test.plyr, 
                "quick_exit_package", 
                "visitors_package")

# By clickability test
 
result_chisq2 <- ddply(.data = cperd_clean, 
                       .variables = .(clickability_test_id), 
                       .fun = prop.test.plyr, 
                       "quick_exit_package", 
                       "visitors_package")

# by(cperd_clean, cperd_clean$nugget_id, FUN = function(x) {prop.test(cperd_clean$quick_exit_package, cperd_clean$visitors_package, alternative = "two.sided")})
# click_package_exit_rate_data$chi_sq_contr <- round(((click_package_exit_rate_data$quick_exit_rate_diff/100)^2/(click_package_exit_rate_data$quick_exit_rate_nugget/100)), digits = 3)
# quick_exit_tests$chisq <- by(click_package_exit_rate_data$chi_sq_contr[click_package_exit_rate_data$visitors_package >= 10], click_package_exit_rate_data$nugget_id[click_package_exit_rate_data$visitors_package >= 10], sum)
# chisq.test(click_package_exit_rate_data$quick_exit_rate_package[click_package_exit_rate_data$visitors_package >=10], click_package_exit_rate_data$quick_exit_rate_nugget[click_package_exit_rate_data$visitors_package >=10])

# Get p-values from chi-squared tests
# quick_exit_tests$df <- quick_exit_tests$cp_len - 1
# quick_exit_tests$p_value_chisq <- round(pchisq(quick_exit_tests$chisq, df = quick_exit_tests$df, lower.tail = FALSE), digits = 3)


# Histograms of chi-squared values, p-values, and sample size
library("ggplot2")

ggplot(data=result_chisq, aes(result_chisq$p_value)) + geom_histogram(fill = "blue", col = "red") + labs(title = "Distribution of p-values for Chi-squared tests (by nugget)") + labs(x = "p-values", y = "Count")
ggplot(data=result_chisq, aes(result_chisq$chi_square)) + geom_histogram(fill = "blue", col = "red") + labs(title = "Distribution of Chi-squared values (by nugget)") + labs(x = "Chi-squared values", y = "Count")

ggplot(data=result_chisq2, aes(result_chisq2$p_value)) + geom_histogram(fill = "blue", col = "red") + labs(title = "Distribution of p-values for Chi-squared tests (by clickability test)") + labs(x = "p-values", y = "Count")
ggplot(data=result_chisq2, aes(result_chisq2$chi_square)) + geom_histogram(fill = "blue", col = "red") + labs(title = "Distribution of Chi-squared values (by clickability test)") + labs(x = "Chi-squared values", y = "Count")

ggplot(data=result_chisq, aes(result_chisq$df)) + geom_histogram(fill = "blue", col = "red") + labs(title = "Distribution of degrees of freedom (by nugget)") + labs(x = "Degrees of freedom", y = "Count")
ggplot(data=result_chisq2, aes(result_chisq2$df)) + geom_histogram(fill = "blue", col = "red") + labs(title = "Distribution of degrees of freedom (by clickability test)") + labs(x = "Degrees of freedom", y = "Count")


# Use Fisher's and Stouffer's methods to obtain pooled p-values for t-tests
library("metap")
fisher_t <- sumlog(p = result_t$p_value[!is.na(result_t$p_value)])
fisher_t
stouffer_t <- sumz(p = result_t$p_value[!is.na(result_t$p_value)], weights = result_t$df[!is.na(result_t$p_value)])
stouffer_t

fisher_t2 <- sumlog(p = result_t2$p_value[!is.na(result_t2$p_value)])
fisher_t2
stouffer_t2 <- sumz(p = result_t2$p_value[!is.na(result_t2$p_value)], weights = result_t2$df[!is.na(result_t2$p_value)])
stouffer_t2


# Use Fisher's and Stouffer's methods to obtain pooled p-values for chi-squared tests
library("metap")

# By nugget
fisher_chisq <- sumlog(p = result_chisq$p_value[!is.na(result_chisq$p_value)])
fisher_chisq
stouffer_chisq <- sumz(p = result_chisq$p_value[!is.na(result_chisq$p_value)], weights = result_chisq$df[!is.na(result_chisq$p_value)])
stouffer_chisq

# By clickability test
fisher_chisq2 <- sumlog(p = result_chisq2$p_value[!is.na(result_chisq2$p_value)])
fisher_chisq2
stouffer_chisq2 <- sumz(p = result_chisq2$p_value[!is.na(result_chisq2$p_value)], weights = result_chisq2$df[!is.na(result_chisq2$p_value)])
stouffer_chisq2

# Count and % of tests that are significant at alpha = .05 and .10

# By nugget
sum(result_chisq$p_value < .05, na.rm = TRUE)
sum(result_chisq$p_value < .10, na.rm = TRUE)
sum(result_chisq$p_value < .05, na.rm = TRUE)/sum(!is.na(result_chisq$p_value))
sum(result_chisq$p_value < .10, na.rm = TRUE)/sum(!is.na(result_chisq$p_value))

# By clickability test

sum(result_chisq2$p_value <= .05, na.rm = TRUE)
sum(result_chisq2$p_value <= .10, na.rm = TRUE)
sum(result_chisq2$p_value <= .05, na.rm = TRUE)/sum(!is.na(result_chisq2$p_value))
sum(result_chisq2$p_value <= .10, na.rm = TRUE)/sum(!is.na(result_chisq2$p_value))

# Count and % of tests with small sample size
sum(quick_exit_tests$cp_len < 5, na.rm = TRUE)/sum(!is.na(quick_exit_tests$cp_len))
sum(quick_exit_tests$cp_len < 10, na.rm = TRUE)/sum(!is.na(quick_exit_tests$cp_len))
sum(quick_exit_tests_1$cp_len_1 < 5, na.rm = TRUE)/sum(!is.na(quick_exit_tests_1$cp_len_1))
sum(quick_exit_tests_1$cp_len_1 < 10, na.rm = TRUE)/sum(!is.na(quick_exit_tests_1$cp_len_1))


# Calculate mean and SD QER by CT
library("plyr")
cperd_clean_2a <- read.csv("cperd_clean_2a.csv")
cperd_clean_2a <- data.frame(cperd_clean_2a)
cperd_clean_2a <- filter(cperd_clean_2a, p_value <= .10)

summary_qer_ct <- ddply(cperd_clean_2a, .(clickability_test_id), summarize, mean=mean(quick_exit_rate_package), median=median(quick_exit_rate_package), sd=sd(quick_exit_rate_package), var=var(quick_exit_rate_package), length=length(quick_exit_rate_package))
cperd_clean_2b <- join(cperd_clean_2a, summary_qer_ct, by = "clickability_test_id", type = "left")

qe_test <- ddply(cperd_clean_2b, .(clickability_test_id), summarize, quick_exit_test=sum(quick_exit_package), visitors_test=sum(visitors_package), qer_test=(sum(quick_exit_package)/sum(visitors_package))*100)
cperd_clean_2b <- join(cperd_clean_2b, qe_test, by = "clickability_test_id", type = "left")


# Calculate chi-squared values using CT QER as expected values

cperd_clean_2b$ch_sq_critical <- ifelse(cperd_clean_2b$length == 7, 10.645, ifelse(cperd_clean_2b$length == 6, 9.236, ifelse(cperd_clean_2b$length == 5, 7.779, ifelse(cperd_clean_2b$length == 4, 6.251, ifelse(cperd_clean_2b$length == 3, 4.605, 2.706)))))

cperd_clean_2b$chi_sq_cont <- ((cperd_clean_2b$quick_exit_rate_package - cperd_clean_2b$qer_test)^2)/(cperd_clean_2b$qer_test)


ch_sq <- ddply(cperd_clean_2b, .(clickability_test_id), summarize, chi_sq=sum(chi_sq_cont))

cperd_clean_2b <- join(cperd_clean_2b, ch_sq, by = "clickability_test_id", type = "left")

cperd_clean_2b$ch_sq_sig <- ifelse(cperd_clean_2b$chi_sq >= cperd_clean_2b$ch_sq_critical, 1, 0)
cperd_clean_2b$ch_sq_sig <- factor(cperd_clean_2b$ch_sq_sig, levels = c(0,1), labels = c("Not Significant", "Significant"))

cperd_clean_2e <- subset(cperd_clean_2b, !duplicated(cperd_clean_2b$clickability_test_id))

cperd_clean_2e$sig_p <- ifelse(cperd_clean_2e$p_value <= .10,1,0)
cperd_clean_2e$sig_p <- factor(cperd_clean_2e$sig_p, levels = c(0,1), labels = c("Not Significant", "Significant"))

# Determine outliers by SD (3 sigma)

cperd_clean_2b$outlier_2sd <- ifelse(cperd_clean_2b$quick_exit_rate_package >= cperd_clean_2b$mean+(2*cperd_clean_2b$sd),1,0) 
cperd_clean_2b$outlier_3sd <- ifelse(cperd_clean_2b$quick_exit_rate_package >= cperd_clean_2b$mean+(3*cperd_clean_2b$sd),1,0) 

cperd_clean_2b$outlier_2sd <- factor(cperd_clean_2b$outlier_2sd, levels = c(0,1), labels = c("Not Outlier", "Outlier"))
cperd_clean_2b$outlier_3sd <- factor(cperd_clean_2b$outlier_3sd, levels = c(0,1), labels = c("Not Outlier", "Outlier"))

# Determine outliers by IQD (boxplot rule)


# fun2 <- function(x) {quantile(x, c(.75))+(1.5*(quantile(x, c(.75)) - quantile(x, c(.25))))
# }
# outlier_IQD <- ddply(cperd_clean_2b, .(clickability_test_id), summarize, fun2(cperd_clean_2b$quick_exit_rate_package))


outlier_IQD <- (aggregate(cperd_clean_2b$quick_exit_rate_package, by=list(as.factor(cperd_clean_2b$clickability_test_id)), FUN=summary, na.rm=TRUE))
x <- as.data.frame(outlier_IQD$x)
outlier_IQD <- as.data.frame(cbind(outlier_IQD$Group.1, x))

names(outlier_IQD) <- c("clickability_test_id", "Min", "25_%tile", "Median", "Mean", "75_%tile", "Max")

cperd_clean_2b <- join(cperd_clean_2b, outlier_IQD, by = "clickability_test_id", type = "left")

cperd_clean_2b$IQR_outlier_threshold <- cperd_clean_2b$`75_%tile` + (1.5*(cperd_clean_2b$`75_%tile` - cperd_clean_2b$`25_%tile`))

cperd_clean_2b$outlier_IQR <- ifelse(cperd_clean_2b$quick_exit_rate_package >= cperd_clean_2b$IQR_outlier_threshold,1,0)
cperd_clean_2b$outlier_IQR <- factor(cperd_clean_2b$outlier_IQR, levels = c(0,1), labels = c("Not Outlier", "Outlier"))


# Determine outliers by Hampel identifier
library(stats)

mad_x <- ddply(cperd_clean_2b, .(clickability_test_id), function(x){
  limits = mad(x$quick_exit_rate_package)
})

hampel_outlier <- ddply(cperd_clean_2b, .(clickability_test_id), function(x){
  limits = median(x$quick_exit_rate_package) + 3*mad(x$quick_exit_rate_package)
})

cperd_clean_2c <- join(cperd_clean_2b, mad_x, by = "clickability_test_id", type = "left")
cperd_clean_2c <- join(cperd_clean_2b, hampel_outlier, by = "clickability_test_id", type = "left")

colnames(cperd_clean_2c)[29:30] <- c("mad", "hampel_outlier_threshold")

cperd_clean_2c$hampel_outlier <- ifelse(cperd_clean_2c$quick_exit_rate_package >= cperd_clean_2c$hampel_outlier_threshold,1,0)
cperd_clean_2c$hampel_outlier <- factor(cperd_clean_2c$hampel_outlier, levels = c(0,1), labels = c("Not Outlier", "Outlier"))

# Determine outliers by adjusted boxplot rule
library(robustbase)

mc <- ddply(cperd_clean_2c, .(clickability_test_id), function(x){
  limits = mc(x$quick_exit_rate_package, na.rm = TRUE)
})

cperd_clean_2d <- join(cperd_clean_2c, mc, by = "clickability_test_id", type = "left")

colnames(cperd_clean_2d)[32] <- c("medcouple")

cperd_clean_2d$abr_outlier_threshold <- ifelse(cperd_clean_2d$medcouple < 0, (cperd_clean_2d$`75_%tile` + (1.5*exp(4*cperd_clean_2d$medcouple)*(cperd_clean_2d$`75_%tile` - cperd_clean_2d$`25_%tile`))), (cperd_clean_2d$`75_%tile` + (1.5*exp(3*cperd_clean_2d$medcouple)*(cperd_clean_2d$`75_%tile` - cperd_clean_2d$`25_%tile`))))

cperd_clean_2d$abr_outlier <- ifelse(cperd_clean_2d$quick_exit_rate_package >= cperd_clean_2d$abr_outlier_threshold,1,0)
cperd_clean_2d$abr_outlier <- factor(cperd_clean_2d$abr_outlier, levels = c(0,1), labels = c("Not Outlier", "Outlier"))

# Get quick exits and visits per clickability test


# qe_ct <- ddply(cperd_clean_2d, .(clickability_test_id), function(x){
#  limits = sum(x$quick_exit_package, na.rm = TRUE)
# })

# visitors_ct <- ddply(cperd_clean_2d, .(clickability_test_id), function(x){
#  limits = sum(x$visitors_package, na.rm = TRUE)
# })

# cperd_clean_2d <- join(cperd_clean_2d, qe_ct, by = "clickability_test_id", type = "left")
# cperd_clean_2d <- join(cperd_clean_2d, visitors_ct, by = "clickability_test_id", type = "left")

# colnames(cperd_clean_2d)[35:36] <- c("quick_exit_test", "visitors_test")