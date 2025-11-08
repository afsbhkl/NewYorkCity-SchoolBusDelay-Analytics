## ==============================
## 1) Setup and Data Import 设置与数据导入
## ==============================
library(dplyr)
library(stringr)

# Read raw bus_original dataset
df <- read.csv("dataset/bus_original.csv")


## ==============================
## 2) Drop unnecessary columns & handle missing values 删除不重要的数据+处理缺值
## ==============================
# Remove columns not needed for cleaning
df <- df %>%
  select(-Created_On, -Incident_Number, -Number_Of_Students_On_The_Bus, -Last_Updated_On)

# Convert empty strings "" to NA and remove rows with NA (first pass)
df[df == ""] <- NA
df <- na.omit(df)

# Repeat to ensure consistency (second pass)
df[df == ""] <- NA
df_clean <- na.omit(df)

## ==============================
## 3) Preprocess How_Long_Delayed 清理How_Long_Delayed
## ==============================
# Standardize: convert all values to lowercase
df_clean$How_Long_Delayed <- tolower(df_clean$How_Long_Delayed)

# Keep only rows containing time-related markers (h, m, -, /)
df_clean <- df_clean[grepl("h|m|-|/", df_clean$How_Long_Delayed), ]

# Initialize numeric output column for parsed delays (in minutes)
df_clean$Delay_clean <- NA_real_

## 2.1) Minutes only (contains "m", but not h, -, /, or \)
mask_m_only <- grepl("m", df_clean$How_Long_Delayed) &
  !grepl("h|-|/|\\\\", df_clean$How_Long_Delayed)

df_clean$Delay_clean[mask_m_only] <- as.numeric(
  str_extract(df_clean$How_Long_Delayed[mask_m_only], "\\d+")
)

## 2.2) Minute ranges with "-" (e.g., "10-15m"; no :, /, h)
mask_m_dash <- grepl("m", df_clean$How_Long_Delayed) &
  grepl("-", df_clean$How_Long_Delayed) &
  !grepl(":|/|h", df_clean$How_Long_Delayed)

nums_m_dash <- regmatches(df_clean$How_Long_Delayed[mask_m_dash],
                          gregexpr("\\d+", df_clean$How_Long_Delayed[mask_m_dash]))

num1_m_dash <- as.numeric(sapply(nums_m_dash, `[`, 1))
num2_m_dash <- as.numeric(sapply(nums_m_dash, `[`, 2))

# Fix invalid ranges (if first >= second, strip the first digit of num1)
fix_idx_m_dash <- which(num1_m_dash >= num2_m_dash & !is.na(num1_m_dash) & !is.na(num2_m_dash))
if (length(fix_idx_m_dash) > 0) {
  num1_m_dash[fix_idx_m_dash] <- as.numeric(sub("^\\d", "", sapply(nums_m_dash[fix_idx_m_dash], `[`, 1)))
}

avg_m_dash <- rowMeans(cbind(num1_m_dash, num2_m_dash), na.rm = TRUE)
df_clean$Delay_clean[mask_m_dash] <- avg_m_dash

## 2.3) Minute “fractions” with "/" (e.g., "10/20m")
mask_m_slash <- grepl("m", df_clean$How_Long_Delayed) &
  grepl("/", df_clean$How_Long_Delayed)

nums_m_slash <- regmatches(df_clean$How_Long_Delayed[mask_m_slash],
                           gregexpr("\\d+", df_clean$How_Long_Delayed[mask_m_slash]))

num1_m_slash <- as.numeric(sapply(nums_m_slash, `[`, 1))
num2_m_slash <- as.numeric(sapply(nums_m_slash, `[`, 2))

# Fix invalid pairs (first >= second → drop the first digit of the first)
fix_idx_m_slash <- which(num1_m_slash >= num2_m_slash & !is.na(num1_m_slash) & !is.na(num2_m_slash))
if (length(fix_idx_m_slash) > 0) {
  num1_fixed <- sub("^\\d", "", sapply(nums_m_slash[fix_idx_m_slash], `[`, 1))
  num1_m_slash[fix_idx_m_slash] <- as.numeric(num1_fixed)
}

avg_m_slash <- rowMeans(cbind(num1_m_slash, num2_m_slash), na.rm = TRUE)
df_clean$Delay_clean[mask_m_slash] <- avg_m_slash

## 2.4) Pure numeric ranges (e.g., "10-15")
mask_num_dash_only <- grepl("^[0-9]+-[0-9]+$", df_clean$How_Long_Delayed)

nums_num_dash <- regmatches(df_clean$How_Long_Delayed[mask_num_dash_only],
                            gregexpr("\\d+", df_clean$How_Long_Delayed[mask_num_dash_only]))

num1_num_dash <- as.numeric(sapply(nums_num_dash, `[`, 1))
num2_num_dash <- as.numeric(sapply(nums_num_dash, `[`, 2))

avg_num_dash <- rowMeans(cbind(num1_num_dash, num2_num_dash), na.rm = TRUE)
df_clean$Delay_clean[mask_num_dash_only] <- avg_num_dash

## 2.5) Hours only (e.g., "2h"; contains digits + h, no -, /, :, m)
mask_h_only <- grepl("\\d", df_clean$How_Long_Delayed) &
  grepl("h", df_clean$How_Long_Delayed) &
  !grepl("-|/|:|m", df_clean$How_Long_Delayed)

num_hours_only <- as.numeric(regmatches(df_clean$How_Long_Delayed[mask_h_only],
                                        regexpr("\\d+", df_clean$How_Long_Delayed[mask_h_only])))

df_clean$Delay_clean[mask_h_only] <- num_hours_only * 60

## 2.6) Fractional hours with "/" (e.g., "1/2h"; no -, :, m)
mask_h_slash <- grepl("\\d", df_clean$How_Long_Delayed) &
  grepl("h", df_clean$How_Long_Delayed) &
  grepl("/", df_clean$How_Long_Delayed) &
  !grepl("-|:|m", df_clean$How_Long_Delayed)

nums_h_slash <- regmatches(df_clean$How_Long_Delayed[mask_h_slash],
                           gregexpr("\\d+", df_clean$How_Long_Delayed[mask_h_slash]))

num1_h_slash <- as.numeric(sapply(nums_h_slash, `[`, 1))
num2_h_slash <- as.numeric(sapply(nums_h_slash, `[`, 2))

avg_hours_h_slash <- rowMeans(cbind(num1_h_slash, num2_h_slash), na.rm = TRUE)
df_clean$Delay_clean[mask_h_slash] <- avg_hours_h_slash * 60

## 2.7) Colon time format (e.g., "1:30h"; hh:mmh; no -, /, m)
mask_h_colon <- grepl("\\d", df_clean$How_Long_Delayed) &
  grepl("h", df_clean$How_Long_Delayed) &
  grepl(":", df_clean$How_Long_Delayed) &
  !grepl("-|/|m", df_clean$How_Long_Delayed)

matches_h_colon <- regmatches(df_clean$How_Long_Delayed[mask_h_colon],
                              gregexpr("\\d+", df_clean$How_Long_Delayed[mask_h_colon]))

hours_h_colon <- as.numeric(sapply(matches_h_colon, `[`, 1))
mins_h_colon  <- as.numeric(sapply(matches_h_colon, `[`, 2))
mins_h_colon[is.na(mins_h_colon)] <- 0

df_clean$Delay_clean[mask_h_colon] <- hours_h_colon * 60 + mins_h_colon

## 2.8) Combined hours + minutes (e.g., "1h30m"; no -, /, :)
mask_hm_combo <- grepl("\\d", df_clean$How_Long_Delayed) &
  grepl("h", df_clean$How_Long_Delayed) &
  grepl("m", df_clean$How_Long_Delayed) &
  !grepl("-|/|:|：", df_clean$How_Long_Delayed)

x_hm <- df_clean$How_Long_Delayed[mask_hm_combo]
hours_hm <- as.numeric(sub(".*?(\\d+)\\s*h.*", "\\1", x_hm))
mins_hm  <- as.numeric(sub(".*?(\\d+)\\s*m.*", "\\1", x_hm))
hours_hm[is.na(hours_hm)] <- 0
mins_hm[is.na(mins_hm)]   <- 0

df_clean$Delay_clean[mask_hm_combo] <- hours_hm * 60 + mins_hm

## ==============================
## 3) Export intermediate cleaned data
## ==============================
# Rows where Delay_clean is still NA (for later review)
df_na <- df_clean[is.na(df_clean$Delay_clean), ]

# Export cleaned dataset after delay parsing (to be used by later steps) 下载检查How_Long_Delayed的清理情况
write.csv(df_clean, "dataset/bus_clean_process.csv", row.names = FALSE)


## ==============================
## 4) Reload intermediate file for further cleaning steps
## ==============================
df_clean <- read.csv("dataset/bus_clean_process.csv", stringsAsFactors = FALSE)

# Remove remaining rows with NA
df_clean <- na.omit(df_clean)

# Keep a copy of rows where Delay_clean is NA (inspection/debugging)
df_na <- df_clean[is.na(df_clean$Delay_clean), ]

## ==============================
## 5) Column adjustments (structure-level cleaning)
## ==============================
# Drop How_Long_Delayed (now redundant after parsing)
df_clean <- df_clean %>%
  select(-How_Long_Delayed)

# Normalize School_Year to the first 4 digits (e.g., "2015-2016" → "2015")
df_clean$School_Year <- sub("^(\\d{4}).*", "\\1", df_clean$School_Year)

## ==============================
## 6) Clean Bus_No (keep pure integers only)
## ==============================
df_clean <- df_clean[grepl("^\\d+$", df_clean$Bus_No), ]

## ==============================
## 7) Clean Route_Number (valid alphanumerics only, not "0")
## ==============================
# Exclude Route_Number == "0"
df_clean <- df_clean[df_clean$Route_Number != "0", ]

# Exclude non-alphanumeric Route_Number values
df_clean <- df_clean[grepl("^[A-Za-z0-9]+$", df_clean$Route_Number), ]

## ==============================
## 8) Normalize Bus Company Names (standardization for downstream grouping)
## ==============================
# Copy original to a working column
df_clean$Bus_company_name2 <- df_clean$Bus_Company_Name

# Uppercase normalization
df_clean$Bus_company_name2 <- toupper(df_clean$Bus_company_name2)

# Use first 5 characters as a prefix key and map to the first seen canonical name
prefix <- substr(df_clean$Bus_company_name2, 1, 5)
mapping <- tapply(df_clean$Bus_company_name2, prefix, function(x) x[1])
df_clean$Bus_company_name2 <- mapping[prefix]

## ==============================
## 9) Export fully cleaned dataset (hand-off to next stages)
## ==============================
write.csv(df_clean, "dataset/bus_clean_result.csv", row.names = FALSE)

