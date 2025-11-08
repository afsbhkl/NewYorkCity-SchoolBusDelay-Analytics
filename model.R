## ==============================
## 1) CART
## finding the main factors decide the delay time of school bus throgh CART
## 通过cart模型来寻找影响how_long_delay的主要因素
## ==============================

if (!requireNamespace("reticulate", quietly = TRUE)) install.packages("reticulate")
library(reticulate)

required_py_packages <- c("pandas", "openpyxl", "scikit-learn", "numpy")
for (pkg in required_py_packages) if (!py_module_available(pkg)) py_install(pkg)

invisible(py_run_string(r"(
import pandas as pd
from sklearn.tree import DecisionTreeRegressor
from sklearn.preprocessing import LabelEncoder
import numpy as np
from sklearn.metrics import mean_absolute_error

file_path = r'dataset/bus_clean.xlsx'
df = pd.read_excel(file_path, sheet_name='Bus_new', engine='openpyxl')
required_columns = ['Route_Number', 'Day', 'Hours', 'Boro', 'Bus_company_name', 'Delay_clean', 'Reason']
missing_cols = [col for col in required_columns if col not in df.columns]
if missing_cols: exit()
natural_factors = ['Weather Conditions', 'Heavy Traffic', 'Natural Disaster']
df['Natural_Factor'] = df['Reason'].apply(lambda x: 1 if str(x) in natural_factors else 0)
for col in ['Route_Number', 'Day', 'Boro', 'Bus_company_name']:
    le = LabelEncoder()
    df[col] = le.fit_transform(df[col].astype(str))
features = ['Route_Number', 'Day', 'Hours', 'Boro', 'Bus_company_name', 'Natural_Factor']
X = df[features]
y = df['Delay_clean']
model = DecisionTreeRegressor(max_depth=6, min_samples_split=12, random_state=42)
model.fit(X, y)
importance = model.feature_importances_ * 100
result = pd.DataFrame({'Feature': features, 'Importance': np.round(importance, 2)})
result = result.sort_values('Importance', ascending=False)
result['Significance'] = ''
result.iloc[0, 2] = '**'
result.iloc[1, 2] = '*'
y_pred = model.predict(X)
mae = mean_absolute_error(y, y_pred)
natural_cases = df['Natural_Factor'].sum()
total_cases = len(df)
percentage = round(df['Natural_Factor'].mean() * 100, 1)
reason_counts = df['Reason'].value_counts()
bus_results = {
    'feature_importance': result,
    'mae': mae,
    'natural_distribution': {'cases': natural_cases, 'total': total_cases, 'percentage': percentage},
    'reason_counts': reason_counts
}
)"))

results <- py$bus_results

output_lines <- c(
  "Feature Importance Ranking (Scale 0-100):",
  "=================================================="
)

max_feature_length <- max(nchar(ifelse(results$feature_importance$Feature == "Natural_Factor", 
                                       "Reason (Natural factors)", 
                                       as.character(results$feature_importance$Feature))))

for(i in 1:nrow(results$feature_importance)) {
  feature <- as.character(results$feature_importance$Feature[i])
  display_name <- ifelse(feature == "Natural_Factor", "Reason (Natural factors)", feature)
  spaces_needed <- max_feature_length - nchar(display_name) + 2
  output_lines <- c(output_lines,
                    sprintf("%s%s%.2f%s", 
                            display_name,
                            paste(rep(" ", spaces_needed), collapse = ""),
                            results$feature_importance$Importance[i],
                            results$feature_importance$Significance[i]))
}

output_lines <- c(output_lines,
                  "==================================================",
                  "",
                  sprintf("Model Mean Absolute Error (MAE): %.2f minutes", results$mae),
                  "",
                  "Natural factors distribution:",
                  sprintf("Natural factor cases: %d/%d (%.1f%%)", 
                          results$natural_distribution$cases,
                          results$natural_distribution$total,
                          results$natural_distribution$percentage),
                  "",
                  "Original reason categories:",
                  "Reason"
)

reason_df <- as.data.frame(results$reason_counts)
max_reason_length <- max(nchar(rownames(reason_df)))
reason_lines <- sapply(rownames(reason_df), function(reason) {
  spaces_needed <- max_reason_length - nchar(reason) + 2
  sprintf("%s%s%d", reason, paste(rep(" ", spaces_needed), collapse = ""), reason_df[reason, 1])
})

writeLines(c(output_lines, reason_lines))

## ==============================
## 2) Randomforest
## finding the main factors decide the delay time of school bus throgh RandomForest
## 通过随机森林模型来寻找影响how_long_delay的主要因素
## ==============================
if (!requireNamespace("reticulate", quietly = TRUE)) install.packages("reticulate")
library(reticulate)

required_py_packages <- c("pandas", "openpyxl", "scikit-learn")
for (pkg in required_py_packages) if (!py_module_available(pkg)) py_install(pkg)

invisible(py_run_string(r"(
import pandas as pd
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
import numpy as np

file_path = r'dataset/bus_clean.xlsx'
df = pd.read_excel(file_path, sheet_name='Bus_new', engine='openpyxl')
df = df[['Route_Number', 'Boro', 'Delay_clean', 'Bus_company_name', 'Day', 'Hours']]
df['Day'] = df['Day'].map({
    'Monday':0, 'Tuesday':1, 'Wednesday':2, 'Thursday':3,
    'Friday':4, 'Saturday':5, 'Sunday':6
}).fillna(-1)
for col in ['Route_Number', 'Boro', 'Bus_company_name']:
    df[col] = LabelEncoder().fit_transform(df[col].astype(str))
df['Is_weekend'] = df['Day'].isin([5,6]).astype(int)
df = df.dropna()
X = df[['Route_Number', 'Boro', 'Bus_company_name', 'Day', 'Hours', 'Is_weekend']]
y = df['Delay_clean']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
model = RandomForestRegressor(n_estimators=200, max_depth=12, min_samples_leaf=5, random_state=42, n_jobs=-1).fit(X_train, y_train)
feature_importance = pd.DataFrame({
    'Feature': X.columns,
    'Importance': model.feature_importances_
}).sort_values('Importance', ascending=False)
feature_importance['Significance'] = ''
feature_importance.iloc[0, 2] = '**'
feature_importance.iloc[1, 2] = '*'
bus_data_results = {
    'split_records': {'train':len(X_train), 'test':len(X_test)},
    'feature_importance': feature_importance,
    'model_performance': {
        'train_r2': model.score(X_train, y_train),
        'test_r2': model.score(X_test, y_test)
    }
}
)"))

results <- py$bus_data_results

writeLines(c(
  "Data split results:",
  sprintf("Training set: %d records", results$split_records$train),
  sprintf("Test set: %d records", results$split_records$test),
  "",
  "=== Feature Importance with Time Features ===",
  paste(rep("=", 50), collapse = ""),
  sprintf("%-20s %.4f%s", results$feature_importance$Feature, results$feature_importance$Importance, results$feature_importance$Significance),
  paste(rep("=", 50), collapse = ""),
  "",
  "=== Model Performance ===",
  sprintf("Training R²: %.4f", results$model_performance$train_r2),
  sprintf("Test R²: %.4f", results$model_performance$test_r2)
))

#Companies with weak operational management
if (!requireNamespace("reticulate", quietly = TRUE)) install.packages("reticulate")
library(reticulate)

required_py_packages <- c("pandas", "openpyxl")
for (pkg in required_py_packages) {
  if (!py_module_available(pkg)) py_install(pkg)
}

invisible(py_run_string('
import pandas as pd

file_path = r"dataset/bus_clean.xlsx"

df = pd.read_excel(file_path, engine="openpyxl")

operational_issues = [
    "Flat Tire",
    "Late return from Field Trip",
    "Mechanical Problem",
    "Problem Run",
    "Won`t Start"
]

operational_df = df[df["Reason"].isin(operational_issues)]

issue_counts = operational_df.groupby(["Bus_company_name", "Reason"]).size().unstack().fillna(0)

issue_counts["Total_Operational_Issues"] = issue_counts.sum(axis=1)

total_delays = df["Bus_company_name"].value_counts()

issue_counts["Operational_Issue_Percentage"] = (issue_counts["Total_Operational_Issues"] / total_delays) * 100

issue_counts = issue_counts[issue_counts["Total_Operational_Issues"] >= 100]

top_companies = issue_counts.sort_values("Operational_Issue_Percentage", ascending=False).head(6)

def get_top_issues(row):
    issues = row[operational_issues]
    top2 = issues.nlargest(2)
    return pd.Series({
        "Top_Issue_1": top2.index[0] if len(top2) > 0 else "",
        "Top_Issue_1_Count": top2.values[0] if len(top2) > 0 else 0,
        "Top_Issue_2": top2.index[1] if len(top2) > 1 else "",
        "Top_Issue_2_Count": top2.values[1] if len(top2) > 1 else 0
    })

top_issues = top_companies.apply(get_top_issues, axis=1)

result = pd.concat([
    top_issues,
    top_companies[["Total_Operational_Issues", "Operational_Issue_Percentage"]]
], axis=1)

result = result.reset_index().rename(columns={"Bus_company_name": "Company_Name"})

result = result[[
    "Company_Name",
    "Top_Issue_1", "Top_Issue_1_Count",
    "Top_Issue_2", "Top_Issue_2_Count",
    "Total_Operational_Issues",
    "Operational_Issue_Percentage"
]]

result.columns = [
    "Bus Company",
    "Primary Issue", "Primary Count",
    "Secondary Issue", "Secondary Count",
    "Total Operational Issues",
    "Operational Issue %"
]

result["Operational Issue %"] = result["Operational Issue %"].map("{:.2f}%".format)

bus_operational_results = {
    "title": "Top Bus Companies by Operational Issues (Minimum 100 Issues)",
    "divider": "=" * 160,
    "result": result.to_string(index=False),
    "note": "Note: Only companies with at least 100 operational issues are included."
}
'))

output_lines <- c(
  "",
  py$bus_operational_results$title,
  py$bus_operational_results$divider,
  py$bus_operational_results$result,
  py$bus_operational_results$divider,
  "",
  py$bus_operational_results$note,
  ""
)

writeLines(output_lines)

## ==============================
## 3) Bootstrap
## The Bootstrap analysis identified 6 problematic operators.
## 通过Bootstrap分析识别出6家问题运营商
## ==============================

library(reticulate)
library(magrittr)

pd <- import("pandas")
np <- import("numpy")

py_run_string("
import numpy as np
import pandas as pd
def process_data(file_path):
    df = pd.read_excel(file_path, sheet_name='Bootstrap')
    required_cols = ['Bus_company_name', 'Route_Number', 'Reason', 'Delay_clean']
    df = df[required_cols].dropna()
    df['Bus_company_name'] = df['Bus_company_name'].str.strip()
    df['Route_Number'] = df['Route_Number'].astype(str).str.strip()
    df['Reason'] = df['Reason'].str.strip()
    df['Delay_clean'] = pd.to_numeric(df['Delay_clean'], errors='coerce')
    return df.dropna(subset=['Delay_clean'])

def bootstrap_analysis_py(data, n_iter=1000):
    results = {}
    top_companies = data['Bus_company_name'].value_counts().head(6).index
    
    for company in top_companies:
        company_data = data[data['Bus_company_name'] == company]
        delay_stats = [np.mean(company_data.sample(frac=1, replace=True)['Delay_clean']) for _ in range(n_iter)]
        delay_ci = np.percentile(delay_stats, [2.5, 97.5])
        
        failure_types = {}
        top_reasons = company_data['Reason'].value_counts().index[:3]
        for reason in top_reasons:
            stats = [np.mean(company_data.sample(frac=1, replace=True)['Reason'] == reason) for _ in range(n_iter)]
            ci = np.percentile(stats, [2.5, 97.5])
            failure_types[reason] = f\"{ci[0]*100:.1f}% ~ {ci[1]*100:.1f}%\"
        
        route_delays = []
        top_routes = company_data['Route_Number'].value_counts().index[:20]
        for route in top_routes:
            route_data = company_data[company_data['Route_Number'] == route]['Delay_clean']
            if len(route_data) < 5: continue
            stats = [np.mean(np.random.choice(route_data, size=len(route_data), replace=True)) for _ in range(n_iter)]
            ci = np.percentile(stats, [2.5, 97.5])
            route_delays.append((route, ci[0], ci[1], delay_ci[0]))
        
        top_routes_sorted = sorted(route_delays, key=lambda x: -x[1])[:3]
        
        results[company] = {
            'Failure Types': failure_types,
            'Critical Routes': [(route, f\"{lower:.1f} ~ {upper:.1f}\", f\"{company_ref:.1f}\") for route, lower, upper, company_ref in top_routes_sorted],
            'Company Mean Delay': f\"{delay_ci[0]:.1f} ~ {delay_ci[1]:.1f}\"
        }
    return results
")

load_data <- function(file_path) {
  py$process_data(file_path) %>% py_to_r()
}

bootstrap_analysis <- function(data, n_iter=1000L) {
  if (!inherits(data, "python.builtin.object")) {
    data <- r_to_py(data)
  }
  py$bootstrap_analysis_py(data, n_iter = as.integer(n_iter)) %>% py_to_r()
}

print_results <- function(results) {
  for (company in names(results)) {
    res <- results[[company]]
    writeLines(sprintf("\n\033[1m%s\033[0m", toupper(company)))
    writeLines(strrep("=", 60))
    writeLines(sprintf("\nCompany Mean Delay: %s mins (95%% CI)", res[['Company Mean Delay']]))
    writeLines("\n[Top Failure Types]")
    for (reason in names(res[['Failure Types']])) {
      writeLines(sprintf("- %-25s %s", reason, res[['Failure Types']][[reason]]))
    }
    writeLines("\n[Critical Problem Routes]")
    writeLines(sprintf("%-15s%-25s%-20s", "Route", "Route Delay (Mean)", "Company Baseline"))
    writeLines(strrep("-", 60))
    for (route in res[['Critical Routes']]) {
      writeLines(sprintf("%-15s%-25s%-20s", route[[1]], route[[2]], route[[3]]))
    }
  }
}

if (interactive()) {
  df <- load_data('dataset/bus_clean.xlsx')
  writeLines(sprintf("Data Overview:\n- Total Records: %d\n- Companies Count: %d", nrow(df), length(unique(df$Bus_company_name))))
  results <- bootstrap_analysis(df)
  writeLines(paste0("\n", strrep("=", 60)))
  writeLines(paste0(strrep(" ", 20), "Bus Breakdown Analysis", strrep(" ", 20)))
  writeLines(strrep("=", 60))
  print_results(results)
}

## ==============================
## 3) regression and OPI Ranking
## An operational performance index (OPI) was constructed through regression models for use in contract renewal evaluations.
## 通过回归模型构建运营绩效指数（OPI），用于合同续签评估。
## ==============================
library(tidyverse)
library(knitr)
library(broom)
library(readxl)  

# 1. Define OPI Weights
WEIGHTS <- list(
  Eff = 0.40,      # Efficiency (Delay_clean, negative coefficient/lower is better)
  Resp = 0.20,     # Responsiveness (Time_Gap, negative coefficient/lower is better)
  Rel = 0.30,      # Reliability (Severe_Delay OR, OR < 1/lower is better)
  Comp = 0.10      # Compliance (Notification_Success OR, OR > 1/higher is better)
)

# 2. Read Data and Dependent Variable Construction 
df <- read_excel("dataset/bus_clean.xlsx", sheet = "Bootstrap_with_month")

df <- df %>%
  mutate(
    Severe_Delay = ifelse(Delay_clean > 30, 1, 0),
    Notification_Success = ifelse(Has_Contractor_Notified_Schools == "Yes" & 
                                    Has_Contractor_Notified_Parents == "Yes", 1, 0),
    across(c(Bus_company_name, Boro, Reason, Run_Type), as.factor)
  ) %>%
  na.omit()

# 3. Define Global Model Formula
formula_reg <- ~ Bus_company_name + Boro + Reason + Run_Type

# 4. Run the Four Global Regression Models
model_A <- lm(Delay_clean ~ Bus_company_name + Boro + Reason + Run_Type, data = df)
model_B <- lm(Time_Gap ~ Bus_company_name + Boro + Reason + Run_Type, data = df)
model_C <- glm(Severe_Delay ~ Bus_company_name + Boro + Reason + Run_Type, 
               data = df, family = binomial(link = "logit"))
model_D <- glm(Notification_Success ~ Bus_company_name + Boro + Reason + Run_Type, 
               data = df, family = binomial(link = "logit"))

# 5. Extract Adjusted Metrics
baseline_company <- sort(unique(df$Bus_company_name))[1] 

extract_adjusted_metrics <- function(model, metric_type) {
  results <- tidy(model, exponentiate = (metric_type %in% c("Rel", "Comp"))) %>%
    filter(str_detect(term, "^Bus_company_name"))
  
  baseline_row <- tibble(
    term = paste0("Bus_company_name", baseline_company),
    estimate = ifelse(metric_type %in% c("Rel", "Comp"), 1, 0) 
  )
  
  bind_rows(results, baseline_row) %>%
    mutate(
      Bus_company_name = gsub("Bus_company_name", "", term),
      Bus_company_name = gsub("Bus_company_name[T.]", "", Bus_company_name, fixed = TRUE),
      Bus_company_name = gsub("`", "", Bus_company_name, fixed = TRUE)
    ) %>%
    select(Bus_company_name, estimate) %>%
    rename(!!paste0("Adj_", metric_type) := estimate)
}

metrics_Eff <- extract_adjusted_metrics(model_A, "Eff")
metrics_Resp <- extract_adjusted_metrics(model_B, "Resp")
metrics_Rel <- extract_adjusted_metrics(model_C, "Rel")
metrics_Comp <- extract_adjusted_metrics(model_D, "Comp")

adjusted_df <- metrics_Eff %>%
  full_join(metrics_Resp, by = "Bus_company_name") %>%
  full_join(metrics_Rel, by = "Bus_company_name") %>%
  full_join(metrics_Comp, by = "Bus_company_name")

# 6. Standardization
z_score <- function(x) {
  as.numeric(scale(x, center = TRUE, scale = TRUE))
}

performance_scores <- adjusted_df %>%
  mutate(
    S_Eff = z_score(Adj_Eff) * -1,
    S_Resp = z_score(Adj_Resp) * -1,
    S_Rel = z_score(Adj_Rel) * -1,
    S_Comp = z_score(Adj_Comp) * 1
  )

# 7. Calculate OPI
final_ranking <- performance_scores %>%
  mutate(
    OPI = (S_Eff * WEIGHTS$Eff) +
      (S_Resp * WEIGHTS$Resp) +
      (S_Rel * WEIGHTS$Rel) +
      (S_Comp * WEIGHTS$Comp),
    OPI_Score_100 = 70 + (OPI * 10),
    OPI_Score_100 = pmax(0, pmin(100, OPI_Score_100))
  ) %>%
  arrange(desc(OPI_Score_100)) %>%
  mutate(Rank = as.integer(row_number())) %>%
  select(Rank, 
         `Company_Name` = Bus_company_name, 
         `OPI_Score_100`, 
         OPI,
         S_Eff, S_Resp, S_Rel, S_Comp,
         `Adj_Coeff_Delay (min)` = Adj_Eff,
         `Adj_Coeff_Notif (min)` = Adj_Resp,
         `Adj_OR_Severe_Delay` = Adj_Rel,
         `Adj_OR_Notif_Success` = Adj_Comp
  ) %>%
  # Ensure all numeric columns are numeric
  mutate(across(where(is.numeric), as.numeric))

# 8. Output Results
writeLines(c(
  "================================================================================================================================",
  "--- Overall Operational Performance Ranking (OPI) Based on Regression Conclusions ---",
  "================================================================================================================================",
  "",
  " Rank |         Company_Name         | OPI_Score_100 |  OPI  | S_Eff | S_Resp | S_Rel | S_Comp | Adj_Delay(min) | Adj_Notif(min) | Adj_OR_Delay | Adj_OR_Notif",
  "-----:|:----------------------------:|-------------:|:-----:|:-----:|:------:|:-----:|:------:|:--------------:|:--------------:|:------------:|:------------:",
  sapply(1:nrow(final_ranking), function(i) {
    row <- final_ranking[i, ]
    
    
    sprintf("%4d | %-27s | %12.2f | %5.2f | %5.2f | %5.2f | %5.2f | %5.2f | %13.2f | %13.2f | %11.2f | %11.2f",
            row$Rank,
            substr(row$Company_Name, 1, 27),  
            row$OPI_Score_100,
            row$OPI,
            row$S_Eff,
            row$S_Resp,
            row$S_Rel,
            row$S_Comp,
            row$`Adj_Coeff_Delay (min)`,
            row$`Adj_Coeff_Notif (min)`,
            row$Adj_OR_Severe_Delay,
            row$Adj_OR_Notif_Success)
  }),
  "",
  "Explanation:",
  "1. The ranking is based on metrics adjusted by the regression models, controlling for factors like Boro, Reason, and Run_Type.",
  sprintf("2. The baseline company is the first alphabetically (%s), whose adjusted coefficients are 0 and ORs are 1.", baseline_company),
  "3. OPI Weights: Efficiency 40%, Reliability 30%, Responsiveness 20%, Compliance 10%.",
  "4. OPI_Score_100 is a score scaled from 0-100 using Adjusted Z-Score method, where 70 represents average performance."
))

#4.2
# ============== Analysis of Congestion Routes for Each Company ==============
library(tidyverse)
library(knitr)
library(broom)
library(readxl)  

df <- read_excel("dataset/bus_clean.xlsx", sheet = "Bootstrap_with_month")

df_clean <- df %>%
  mutate(
    across(c(Bus_company_name, Route_Number, Month, Hours, Day), as.factor)
  ) %>%
  select(Delay_clean, Bus_company_name, Route_Number, Month, Hours, Day) %>%
  na.omit()

company_names <- unique(df_clean$Bus_company_name)

analyze_congested_routes_regression <- function(company_name, data) {
  df_comp <- data %>% filter(Bus_company_name == company_name)
  model <- lm(Delay_clean ~ Route_Number + Month + Hours + Day, data = df_comp)
  
  tidy(model) %>%
    filter(str_detect(term, "^Route_Number"), estimate > 0, p.value < 0.05) %>%
    mutate(
      `Route Number` = gsub("Route_Number", "", term),
      `Route Number` = gsub("`", "", `Route Number`, fixed = TRUE)
    ) %>%
    select(`Route Number`, `Materiality(coef_Route_Number)` = estimate, `P-value` = p.value) %>%
    arrange(desc(`Materiality(coef_Route_Number)`)) %>%
    head(5)
}

all_results <- list()
for (name in company_names) {
  result <- analyze_congested_routes_regression(name, df_clean)
  if (nrow(result) > 0) {
    result$`Company Name` <- name
    all_results[[name]] <- result
  }
}

final_congested_routes_df <- bind_rows(all_results)

# Prepare the output with writeLines
output_lines <- c(
  "======================================================================================",
  "--- Top 5 Most Congested Routes (Regression Adjusted, with Day Control) per Company ---",
  "======================================================================================",
  "",
  "Interpretation: 'Materiality(coef_Route_Number)' is the increase in average delay (in minutes)",
  "caused by this route relative to the baseline route, adjusted for Month, Hour, and Day effects.",
  "",
  "----------------------------------------------------------------------------------------",
  " Company Name            | Route Number | Delay Increase (min) | P-value ",
  "------------------------|--------------|----------------------|---------"
)

if (nrow(final_congested_routes_df) > 0) {
  # Format each row of the results
  result_lines <- sapply(1:nrow(final_congested_routes_df), function(i) {
    row <- final_congested_routes_df[i, ]
    sprintf(" %-22s | %-12s | %19.2f | %7.4f",
            substr(row$`Company Name`, 1, 22),
            row$`Route Number`,
            row$`Materiality(coef_Route_Number)`,
            row$`P-value`)
  })
  
  output_lines <- c(output_lines, result_lines, 
                    "----------------------------------------------------------------------------------------",
                    "",
                    "Note: Only shows routes with statistically significant delays (p < 0.05)")
} else {
  output_lines <- c(output_lines, 
                    "No statistically significant congested routes found.",
                    "")
}

# Write all lines
writeLines(output_lines)


## ==============================
## 4) visualization
## ==============================
# Create data frame
op_issue <- data.frame(
  Company = c(
    "LEESEL TRANSP CORP.",
    "SNT BUS INC.",
    "BORO TRANSIT, INC.",
    "QUALITY TRANSPORTATION CO.",
    "FIRST STEPS TRANSP INC.",
    "JOFAZ TRANSPORTATION INC."
  ),
  Value = c(26.53, 30.77, 25.14, 24.91, 22.89, 36.78)
)

# Arrange data and create factor levels
op_issue <- op_issue %>%
  arrange(Value) %>%
  mutate(Company = factor(Company, levels = Company))

# Create plot with blue color gradient
ggplot(op_issue, aes(x = Value, y = Company, fill = Value)) +
  geom_col(color = "white",  linewidth = 1) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_text(
    aes(label = Company),
    hjust = 1,
    x = -1,
    size = 10,
    fontface = "bold"
  ) +
  geom_text(
    aes(label = paste0(Value, "%"), x = Value),
    hjust = -0.1,
    size = 8
  ) +
  scale_x_continuous(expand = expansion(add = c(40, 8))) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Percentage of Operational Issues by Operator"
  )
