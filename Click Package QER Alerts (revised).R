# Load data
cperd_2a <- read.csv("qer_alerts_sanity_check.csv")
cperd_2a <- data.frame(cperd_2a)

# Name variables
names(cperd_2a) <- c("clickability_test_id", "click_package_id", "left_quickly_30", "pct_left_quickly_30", "total_visitors")

# Remove rows with NULL values
cperd_2a <- cperd_2a[complete.cases(cperd_2a),]

# Remove click packages with < 10 visitors
cperd_2a <- filter(cperd_2a, total_visitors >= 10)

# Get summary stats
summary_qer_ct <- ddply(cperd_2a, .(clickability_test_id), summarize, mean=mean(pct_left_quickly_30), median=median(pct_left_quickly_30), sd=sd(pct_left_quickly_30), var=var(pct_left_quickly_30), length=length(pct_left_quickly_30))
cperd_2b <- join(cperd_2a, summary_qer_ct, by = "clickability_test_id", type = "left")

# Remove single-package tests 
cperd_2b <- filter(cperd_2b, length != 1)

# Get expected QER
qe_test <- ddply(cperd_2b, .(clickability_test_id), summarize, quick_exit_test=sum(left_quickly_30), visitors_test=sum(total_visitors), qer_test=(sum(left_quickly_30)/sum(total_visitors))*100)
cperd_2b <- join(cperd_2b, qe_test, by = "clickability_test_id", type = "left")

# Compute contribution of each click package to chi-squared
cperd_2b$chi_sq_cont <- ((cperd_2b$pct_left_quickly_30 - cperd_2b$qer_test)^2)/(cperd_2b$qer_test)

# Compute ch-squared for each click test
ch_sq <- ddply(cperd_2b, .(clickability_test_id), summarize, chi_sq=sum(chi_sq_cont))
cperd_2b <- join(cperd_2b, ch_sq, by = "clickability_test_id", type = "left")

# Compute quartiles
outlier_IQD_sc <- (aggregate(cperd_2b$pct_left_quickly_30, by=list(as.factor(cperd_2b$clickability_test_id)), FUN=summary, na.rm=TRUE))
x <- as.data.frame(outlier_IQD_sc$x)
outlier_IQD_sc <- as.data.frame(cbind(outlier_IQD_sc$Group.1, x))
names(outlier_IQD_sc) <- c("clickability_test_id", "Min", "25_%tile", "Median", "Mean", "75_%tile", "Max")
cperd_2b <- join(cperd_2b, outlier_IQD_sc, by = "clickability_test_id", type = "left")

# Compute thresholds for QER outliers
cperd_2b$IQR_outlier_threshold_high <- cperd_2b$`75_%tile` + (1.5*(cperd_2b$`75_%tile` - cperd_2b$`25_%tile`))
cperd_2b$IQR_outlier_threshold_low <- cperd_2b$`25_%tile` - (1.5*(cperd_2b$`75_%tile` - cperd_2b$`25_%tile`))

# Determine outlying click packages
cperd_2b$outlier <- ifelse(cperd_2b$pct_left_quickly_30 >= cperd_2b$IQR_outlier_threshold_high,"High QER",ifelse(cperd_2b$pct_left_quickly_30 <= cperd_2b$IQR_outlier_threshold_low, "Low QER", "Okay"))
cperd_2b$outlier <- factor(cperd_2b$outlier, levels = c("Okay", "Low QER", "High QER"), labels = c("Okay", "Low QER", "High QER"))

# Determine alerts
cperd_2b$outlier2 <- ifelse(cperd_2b$chi_sq >= 10 & cperd_2b$pct_left_quickly_30 >= cperd_2b$IQR_outlier_threshold_high,"High QER", ifelse(cperd_2b$chi_sq >= 10 & cperd_2b$pct_left_quickly_30 <= cperd_2b$IQR_outlier_threshold_low,"Low QER", "Okay"))
cperd_2b$outlier2 <- factor(cperd_2b$outlier2, levels = c("Okay", "Low QER", "High QER"), labels = c("Okay", "Low QER", "High QER"))

# Remove duplicate rows
cperd_2e <- subset(cperd_2b, !duplicated(cperd_2b$clickability_test_id))
