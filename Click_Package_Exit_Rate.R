# Load data
click_package_exit_rate <- read.csv("Click_Package_Exit_Rate.csv")
click_package_exit_rate_data <- data.frame(click_package_exit_rate)

# Assign names to columns
names(click_package_exit_rate_data) <- c("nugget_id", "quick_exit_nugget", "visitors_nugget", "clickability_test_id", "click_package_id", "quick_exit_package", "visitors_package")

# Compute quick exit rates
click_package_exit_rate_data$quick_exit_rate_nugget <- round(((click_package_exit_rate_data$quick_exit_nugget/click_package_exit_rate_data$visitors_nugget)*100), digits = 2)
click_package_exit_rate_data$quick_exit_rate_package <- round(((click_package_exit_rate_data$quick_exit_package/click_package_exit_rate_data$visitors_package)*100), digits = 2)
click_package_exit_rate_data$quick_exit_rate_diff <- click_package_exit_rate_data$quick_exit_rate_package - click_package_exit_rate_data$quick_exit_rate_nugget

# Descriptive Stats
desc_stats <- function(x) {c(mean(x), sd(x), var(x))}
desc_stats(click_package_exit_rate_data$quick_exit_rate_nugget)
desc_stats(click_package_exit_rate_data$quick_exit_rate_package)
desc_stats(click_package_exit_rate_data$quick_exit_rate_diff)

# Plot histgrams
hist(click_package_exit_rate_data$quick_exit_rate_nugget)
hist(click_package_exit_rate_data$quick_exit_rate_package)
hist(click_package_exit_rate_data$quick_exit_rate_diff)

# Paired t-test comparing nugget quick exit rates to click package quick exit rates
# t.test(click_package_exit_rate_data$quick_exit_rate_package[click_package_exit_rate_data$visitors_package >= 10], click_package_exit_rate_data$quick_exit_rate_nugget[click_package_exit_rate_data$visitors_package >= 10], paired = TRUE)

# Get mean quick_exit_rate, var, and num click packages per nugget
library("SDMTools")
nugget_id_unique <- (as.vector(levels(click_package_exit_rate_data$nugget_id)))
cp_mean <- as.vector(round(tapply(click_package_exit_rate_data$quick_exit_rate_package[click_package_exit_rate_data$visitors_package >= 10], click_package_exit_rate_data$nugget_id[click_package_exit_rate_data$visitors_package >= 10], mean), digits = 3))
# cp_mean_weighted <- as.vector(tapply(click_package_exit_rate_data$quick_exit_rate_package[click_package_exit_rate_data$visitors_package >= 10], click_package_exit_rate_data$nugget_id[click_package_exit_rate_data$visitors_package >= 10], wt.mean(wt = click_package_exit_rate_data$visitors_package), digits = 3))
cp_var <- as.vector(round(tapply(click_package_exit_rate_data$quick_exit_rate_package[click_package_exit_rate_data$visitors_package >= 10], click_package_exit_rate_data$nugget_id[click_package_exit_rate_data$visitors_package >= 10], var), digits = 3))
# cp_var_weighted <- as.vector(tapply(click_package_exit_rate_data$quick_exit_rate_package[click_package_exit_rate_data$visitors_package >= 10], click_package_exit_rate_data$nugget_id[click_package_exit_rate_data$visitors_package >= 10], var))
cp_len <- cp_len <- as.vector(tapply(click_package_exit_rate_data$quick_exit_rate_package[click_package_exit_rate_data$visitors_package >= 10], click_package_exit_rate_data$nugget_id[click_package_exit_rate_data$visitors_package >= 10], length))
true_value <- as.vector(tapply(click_package_exit_rate_data$quick_exit_rate_nugget, click_package_exit_rate_data$nugget_id, mean))
quick_exit_tests <- data.frame(nugget_id_unique, cp_mean, cp_var, cp_len, true_value)


# Calculate t-values and p-values per nugget
quick_exit_tests$t_value <- round(((cp_mean - true_value)/sqrt(cp_var/cp_len)), digits = 3)
quick_exit_tests$p_value <- round(2*pt(-abs(quick_exit_tests$t_value), df=cp_len-1), digits = 3)
# quick_exit_tests$t_value_weighted <- round(((cp_mean_weighted - true_value)/sqrt(cp_var_weighted/cp_len)), digits = 3)
# quick_exit_tests$p_value_weighted <- round(2*pt(-abs(quick_exit_tests$t_value_weighted), df=cp_len-1), digits = 3)

# Plot histograms
hist(quick_exit_tests$cp_mean)
hist(quick_exit_tests$true_value)
hist(quick_exit_tests$t_value)
hist(quick_exit_tests$p_value)

# One-sample t-tests comparing quick_exit rate across click packages to "true" quick_exit rate (by nugget)

  # Unweighted tests
# tapply(click_package_exit_rate_data$quick_exit_rate_package, click_package_exit_rate_data$nugget_id, FUN = function(x) t.test(click_package_exit_rate_data$quick_exit_rate_package[click_package_exit_rate_data$visitors_package >= 10], mu=mean(click_package_exit_rate_data$quick_exit_rate_nugget, na.rm = TRUE)))
# by(click_package_exit_rate_data, click_package_exit_rate_data$nugget_id, FUN = function(x) t.test(click_package_exit_rate_data$quick_exit_rate_package[click_package_exit_rate_data$visitors_package >= 10], mu=mean(click_package_exit_rate_data$quick_exit_rate_nugget, na.rm = TRUE)))
# for(i in 1:length(click_package_exit_rate_data$nugget_id)) {t.test(click_package_exit_rate_data$quick_exit_package[click_package_exit_rate$nugget_id[i]], mu = mean(click_package_exit_rate_data$quick_exit_rate_nugget[click_package_exit_rate_data$nugget_id[i]], na.rm = TRUE))}
# for(i in 1:length(levels(click_package_exit_rate_data$nugget_id))) {t.test(click_package_exit_rate_data$quick_exit_package[levels(click_package_exit_rate_data$nugget_id)[i]], mu = mean(click_package_exit_rate_data$quick_exit_rate_nugget[levels(click_package_exit_rate_data$nugget_id)[i]], na.rm = TRUE))}

  # Weighted tests (by # visitors per click package)
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

# Use Fisher's and Stouffer's methods to obtain pooled p-value
library("metap")
fisher <- sumlog(p = quick_exit_tests$p_value[!is.na(quick_exit_tests$p_value)])
fisher
stouffer <- sumz(p = quick_exit_tests$p_value[!is.na(quick_exit_tests$p_value)], weights = quick_exit_tests$cp_len[!is.na(quick_exit_tests$p_value)])
stouffer