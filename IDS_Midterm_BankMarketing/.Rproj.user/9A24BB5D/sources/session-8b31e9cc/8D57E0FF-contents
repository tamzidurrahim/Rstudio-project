bank <- read.csv("bank-full.csv", sep = ";", header = TRUE)

dim(bank)
names(bank)
str(bank)
summary(bank)

factor_cols <- c("job", "marital", "education", "default",
                 "housing", "loan", "contact", "month",
                 "poutcome", "y")

bank[factor_cols] <- lapply(bank[factor_cols], factor)

str(bank)

table(bank$y)
prop.table(table(bank$y))

na_counts <- colSums(is.na(bank))
na_counts

factor_cols <- c("job", "marital", "education", "default",
                 "housing", "loan", "contact", "month",
                 "poutcome", "y")

unknown_counts <- sapply(bank[factor_cols], function(x) sum(x == "unknown"))
unknown_counts

bank_clean <- bank

bank_clean <- subset(bank_clean, education != "unknown")

job_mode <- names(sort(table(bank_clean$job), decreasing = TRUE))[1]
job_mode

bank_clean$job[bank_clean$job == "unknown"] <- job_mode

sapply(bank_clean[factor_cols], function(x) sum(x == "unknown"))

dim(bank_clean)

numeric_cols <- c("age", "balance", "duration", "campaign", "pdays", "previous")

summary(bank_clean[, numeric_cols])

balance_q1 <- quantile(bank_clean$balance, 0.25)
balance_q3 <- quantile(bank_clean$balance, 0.75)
balance_iqr <- balance_q3 - balance_q1
balance_lower <- balance_q1 - 1.5 * balance_iqr
balance_upper <- balance_q3 + 1.5 * balance_iqr

duration_q1 <- quantile(bank_clean$duration, 0.25)
duration_q3 <- quantile(bank_clean$duration, 0.75)
duration_iqr <- duration_q3 - duration_q1
duration_lower <- duration_q1 - 1.5 * duration_iqr
duration_upper <- duration_q3 + 1.5 * duration_iqr

balance_outliers_before <- sum(bank_clean$balance < balance_lower | bank_clean$balance > balance_upper)
duration_outliers_before <- sum(bank_clean$duration < duration_lower | bank_clean$duration > duration_upper)

bank_clean2 <- bank_clean
bank_clean2$balance[bank_clean2$balance < balance_lower] <- balance_lower
bank_clean2$balance[bank_clean2$balance > balance_upper] <- balance_upper

bank_clean2$duration[bank_clean2$duration < duration_lower] <- duration_lower
bank_clean2$duration[bank_clean2$duration > duration_upper] <- duration_upper

balance_outliers_after <- sum(bank_clean2$balance < balance_lower | bank_clean2$balance > balance_upper)
duration_outliers_after <- sum(bank_clean2$duration < duration_lower | bank_clean2$duration > duration_upper)

summary(bank_clean2[, c("balance", "duration")])

bank_work <- bank_clean2

bank_work$age_group <- cut(
  bank_work$age,
  breaks = c(0, 30, 45, 60, Inf),
  labels = c("young", "middle", "mature", "senior"),
  right = TRUE
)

housing_num <- ifelse(bank_work$housing == "yes", 1, 0)
bank_work$housing_num <- housing_num

duration_min <- min(bank_work$duration)
duration_max <- max(bank_work$duration)
bank_work$duration_norm <- (bank_work$duration - duration_min) / (duration_max - duration_min)

str(bank_work[, c("age", "age_group", "housing", "housing_num", "duration", "duration_norm")])
summary(bank_work[, c("age_group", "housing_num", "duration_norm")])

bank_nodup <- bank_work


dup_flags <- duplicated(bank_nodup)
dup_count <- sum(dup_flags)

dim_before_dup <- dim(bank_nodup)

bank_nodup <- bank_nodup[!dup_flags, ]

dim_after_dup <- dim(bank_nodup)

dup_count
dim_before_dup
dim_after_dup


bank_final <- bank_nodup

invalid_age <- sum(bank_final$age < 18 | bank_final$age > 95)
invalid_duration <- sum(bank_final$duration <= 0)
invalid_campaign <- sum(bank_final$campaign < 1)

invalid_age
invalid_duration
invalid_campaign

bank_final <- subset(
  bank_final,
  age >= 18 & age <= 95 &
    duration > 0 &
    campaign >= 1
)

dim_bank_final <- dim(bank_final)
summary(bank_final[, c("age", "duration", "campaign")])


table(bank_final$y)

set.seed(123)

yes_index <- which(bank_final$y == "yes")
no_index <- which(bank_final$y == "no")

n_yes <- length(yes_index)
n_no <- length(no_index)

n_yes
n_no

sampled_no_index <- sample(no_index, n_yes)

balanced_index <- c(yes_index, sampled_no_index)
bank_balanced <- bank_final[balanced_index, ]

bank_balanced$y <- droplevels(bank_balanced$y)

table(bank_balanced$y)

set.seed(123)

n_bal <- nrow(bank_balanced)
train_size <- floor(0.7 * n_bal)

train_indices <- sample(seq_len(n_bal), size = train_size)

train_data <- bank_balanced[train_indices, ]
test_data <- bank_balanced[-train_indices, ]

dim_train <- dim(train_data)
dim_test <- dim(test_data)

dim_train
dim_test
table(train_data$y)
table(test_data$y)


numeric_cols <- c("age", "balance", "duration", "campaign", "pdays", "previous")

stats_mean_by_y <- aggregate(
  bank_final[, numeric_cols],
  by = list(y = bank_final$y),
  FUN = mean
)

stats_median_by_y <- aggregate(
  bank_final[, numeric_cols],
  by = list(y = bank_final$y),
  FUN = median
)

stats_sd_by_y <- aggregate(
  bank_final[, numeric_cols],
  by = list(y = bank_final$y),
  FUN = sd
)

stats_mean_by_y
stats_median_by_y
stats_sd_by_y


mean_duration_by_housing <- aggregate(
  duration ~ housing,
  data = bank_final,
  FUN = mean
)

mean_duration_by_housing

duration_ttest <- t.test(duration ~ housing, data = bank_final)
duration_ttest



balance_IQR_by_agegroup <- aggregate(
  balance ~ age_group,
  data = bank_final,
  FUN = IQR
)

balance_sd_by_agegroup <- aggregate(
  balance ~ age_group,
  data = bank_final,
  FUN = sd
)

balance_var_by_agegroup <- aggregate(
  balance ~ age_group,
  data = bank_final,
  FUN = var
)

balance_range_by_agegroup <- aggregate(
  balance ~ age_group,
  data = bank_final,
  FUN = function(x) max(x) - min(x)
)

balance_IQR_by_agegroup
balance_sd_by_agegroup
balance_var_by_agegroup
balance_range_by_agegroup



overall_summary_numeric <- summary(bank_final[, c("age", "balance", "duration",
                                                  "campaign", "pdays", "previous")])
overall_summary_numeric

overall_summary_categorical <- summary(bank_final[, c("job", "marital", "education",
                                                      "default", "housing", "loan",
                                                      "contact", "month", "poutcome", "y")])
overall_summary_categorical