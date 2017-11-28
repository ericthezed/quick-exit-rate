# Calculate summary of QER by CT
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