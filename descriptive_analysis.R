## ==============================
## 1) Setup and Data Import 设置与数据导入
## ==============================
library(ggplot2)
library(data.table)
# Setting colour scheme
# Colours for unordered categorical variables
cust_colours <- c("#069CC6", "#07AEDA", "#07BCED", "#12C6F8", "#25CBF8", "#39CFF9", "#4DD4F9", "#61D9FA", "#74DEFB")

# Colours for 3 to 4 ordered categorical variables
cust_colours2 <- c("#74DEFB", "#4DD4F9", "#25CBF8", "#07BCED")

# Colours for 2 ordered categorical variables
cust_colours3 <- c("#61D9FA", "#12C6F8")

df2 <- fread("dataset/bus_clean_result.csv")
df3 <- df2[Delay_clean < 180]

# Graphs
# 2a Delays ADD MEAN, OUTLIERS, ETC. 校车延误时间的箱线图
ggplot(data=df3, aes(x = "", y = Delay_clean)) +
  geom_boxplot(size = 0.8, width = 0.9, fill=cust_colours[9]) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none",
      axis.text.x = element_text(
      hjust = 0.5,
      size =30,
      face = "bold")
  )+
  scale_y_continuous(labels = NULL) +
  labs(x = NULL, y = NULL, title = "School Bus Delays")



## ==============================
## 2) Data_process 计算time_gap
## ==============================
library(dplyr)   
library(lubridate)  
library(ggplot2)
library(tidyr)
library(tidyverse)


#Data Processing
#install.packages("readxl")
library(readxl)
df <- read_excel("dataset/bus_clean.xlsx")

df <- df %>%
  mutate(
    Occurred_On = ymd_hms(Occurred_On),  
    Informed_On = ymd_hms(Informed_On)  
  )

# calculate time gap (minutes)
df <- df %>%
  mutate(
    Time_Gap = as.numeric(difftime(Informed_On, Occurred_On, units = "mins"))
  )

# count how many Time_Gap values are negative
num_less_than_zero <- sum(df$Time_Gap < 0, na.rm = TRUE)

# print the count
print(num_less_than_zero)

# remove rows with negative Time_Gap
df <- df[!(df$Time_Gap < 0), ]

# inspect missing values per column
print(colSums(is.na(df)))


## ==============================
# 3)descriptive statistics for numeric variables 对数值型变量进行描述性统计
## ==============================
df %>%
  select(Delay_clean, Time_Gap) %>%  # select numeric columns
  summary()

## Delay_clean visualization 校车延误时间的风琴图
q <- quantile(df$Delay_clean,na.rm = TRUE)
df_filtered <- df %>% filter(Delay_clean <= q)

# violin plot for Delay_clean (fix x-axis mapping)
ggplot(df_filtered, aes(x = factor(1), y = Delay_clean)) +  # add x=factor(1) as placeholder to avoid error
  geom_violin(fill = "#4285F4", alpha = 0.7, linewidth = 1) +
  geom_boxplot(width = 0.1, fill = "white", linewidth = 0.8) +
  labs(
    title = "Distribution of Delay_clean",
    y = "Delay_clean Value",
    x = ""  # hide x-axis label (placeholder)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_blank(),  # hide x-axis tick text
    axis.ticks.x = element_blank()  # hide x-axis tick marks
  )

## Time_Gap visualization 通知时间的风琴图
# compute 99th percentile of Delay_clean to filter extremes
q99 <- quantile(df$Time_Gap, 0.99, na.rm = TRUE)
df_filtered <- df %>% filter(Time_Gap <= q99)

# violin plot for Time_Gap (same x-axis placeholder)
ggplot(df_filtered, aes(x = factor(1), y = Time_Gap)) +  # add x-axis placeholder
  geom_violin(fill = "#34A853", alpha = 0.7, linewidth = 1) +
  geom_boxplot(width = 0.1, fill = "white", linewidth = 0.8) +
  labs(
    title = "Distribution of Time_Gap",
    y = "Time_Gap Value",
    x = ""  # hide x-axis label
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_blank(),  # hide x-axis tick text
    axis.ticks.x = element_blank(),  # hide x-axis tick marks
    axis.text.y = element_text(size = 12),
    panel.grid.major.y = element_line(color = "gray90", size = 0.5),
    panel.grid.minor.y = element_blank()
  ) +
  scale_y_continuous(
    limits = c(0, max(df_filtered$Time_Gap, na.rm = TRUE)),  # set y-axis range
    breaks = seq(0, max(df_filtered$Time_Gap, na.rm = TRUE), by = 2)  # custom y-axis breaks
  )

## ==============================
## 4）frequency table for categorical variables 对分类变量进行描述性统计
## ==============================
# View the basic data of school bus operation types, delay reasons, regions and school bus companies
# 查看校车运行类型、延误原因、地区和校车公司的基本数据
df %>%
  select(Run_Type, Reason, Boro, Bus_company_name) %>%
  lapply(table)  # compute frequency for each categorical column

# 绘制延误原因、地区和校车公司的单变量热力图
# Draw a single-variable heat map of the causes of delays, the regions, and the school bus companies.

# define a function to plot frequency heatmap for a single categorical variable
plot_single_heatmap <- function(data, var_name) {
  # compute frequency
  freq_df <- data %>%
    select(all_of(var_name)) %>%
    count(!!sym(var_name)) %>%  # count frequency per category
    rename(Category = !!sym(var_name), Frequency = n) %>%
    # add dummy column for x-axis (2D heatmap requirement)
    mutate(Dummy = "Frequency")
  
  # plot heatmap
  ggplot(freq_df, aes(x = Dummy, y = Category, fill = Frequency)) +
    geom_tile(color = "white", linewidth = 0.5) +  # tiles
    geom_text(aes(label = Frequency), size = 4, color = "black") +  # show counts
    scale_fill_gradient(low = "#f1eef6", high = "#980043") +  # color gradient
    labs(
      title = paste("Frequency Heatmap of", var_name),
      x = "", y = var_name, fill = "Frequency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.y = element_text(size = 11),
      axis.text.x = element_blank(),  # hide dummy x ticks
      axis.ticks.x = element_blank()
    )
}
# plot single-variable heatmaps
vars <- c("Reason", "Boro", "Bus_company_name")
lapply(vars, function(var) {
  print(plot_single_heatmap(df, var))  # display heatmaps one by one
})

# 绘制延误原因、地区和校车公司的两变量交叉频数热图
# Draw a two-variable cross-frequency heatmap of the causes of delays, regions, and school bus companies

# define function to plot cross-frequency heatmap for two categorical variables
plot_pair_heatmap <- function(data, var1, var2) {
  # compute cross frequency
  cross_freq <- data %>%
    select(all_of(c(var1, var2))) %>%
    count(!!sym(var1), !!sym(var2)) %>%  # cross counts
    rename(Var1 = !!sym(var1), Var2 = !!sym(var2), Frequency = n)
  
  # plot heatmap
  ggplot(cross_freq, aes(x = Var2, y = Var1, fill = Frequency)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = Frequency), size = 2, color = "black") +
    scale_fill_gradientn(colors = c("#ffffcc", "#c2e699", "#78c679", "#238443")) +
    labs(
      title = paste("Heatmap:", var1, "vs", var2),
      x = var2, y = var1, fill = "Frequency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10)
    )
}

# define variables for pairwise combinations (all combinations here)
vars <- c("Run_Type", "Reason", "Boro","Bus_company_name")
pairs <- expand.grid(var1 = vars, var2 = vars, stringsAsFactors = FALSE) %>%
  filter(var1 < var2)  # avoid duplicates (e.g. A-B and B-A)

# plot for each pair
lapply(1:nrow(pairs), function(i) {
  print(plot_pair_heatmap(df, pairs$var1[i], pairs$var2[i]))
})

## ==============================
## 4）How_long_delay analysis 单独对延误时间进行分析
## ==============================
all_boro_delay_stats <- df %>%
  filter(!is.na(Boro), !is.na(Bus_company_name))

# Only filter out the delays caused by operational errors. 只筛选出由于操作失误导致的延误
result <- all_boro_delay_stats %>%
  group_by(Bus_company_name) %>%
  mutate(total_reasons = n()) %>%
  group_by(Bus_company_name, total_reasons) %>%
  summarise(
    target_reasons_count = sum(
      Reason %in% c(
        "Flat Tire", 
        "Late return from Field Trip", 
        "Mechanical Problem", 
        "Problem Run",
        "Won't Start"
      ),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    proportion = round(target_reasons_count / total_reasons, 2),
    proportion_percent = paste0(proportion * 100, "%")
  ) %>%
  select(Bus_company_name, total_reasons, target_reasons_count, proportion, proportion_percent)

print(result)

# export to CSV (default to working directory)
write.csv(
  result, 
  file = "bus_company_reason_proportion.csv",  # filename (customizable)
  row.names = FALSE  # exclude row names
)

#Visualization
# proportion visualization 查看每个公司的延误中，操作性延误占的比例
result %>%
  arrange(proportion) %>%
  mutate(Bus_company_name = fct_inorder(Bus_company_name)) %>%
  ggplot(aes(x = proportion, y = Bus_company_name)) +
  geom_col(aes(fill = proportion)) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)),
            hjust = -0.2, size = 3.5, color = "black") +
  scale_fill_gradient(low = "green", high = "red", name = "Proportion") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Bus Companies Ranked by Operation Error Index",
       x = "Proportion", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),  # center and bold
    panel.grid.major.y = element_blank()
  )


# bubble chart
ggplot(result,
       aes(x = total_reasons,
           y = proportion,
           size = target_reasons_count,
           colour = proportion)) +
  geom_point(alpha = .7) +
  scale_size_area(max_size = 15) +
  scale_colour_gradient(low = "orange", high = "darkred") +
  labs(title = "Total vs Operation Error Index vs Count of Target Reasons",
       x = "Total",
       y = "Operation Error Index",
       size = "Target Count",
       colour = "Operation Error Index") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)   # center title
  )

## ==============================
## 5）Route_Number 单独对校车线路进行分析
## ==============================
# Identify the 10 school bus routes where delays occur most frequently 
# 寻找延误发生最频繁的10个校车线路
route_counts <- df_filtered %>%
  count(Route_Number, sort = TRUE)   # descending order

route_counts %>%
  slice_head(n = 10) %>%                # top 10
  ggplot(aes(x = reorder(Route_Number, n), y = n)) +
  geom_col(aes(fill = n), width = 0.7) +
  scale_fill_gradient("Count", low = "#a8dadc", high = "#1d3557") +
  labs(title = "Top 10 Route Frequency",
       x = NULL, y = "Frequency") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


## ==============================
## 6）Present the important descriptive analysis results in an attractive way 
## 对重要的描述性分析结果进行美化输出
## ==============================

# Visualisation
library(ggplot2)
library(data.table)
# Setting colour scheme
cust_colours <- c("#069CC6", "#07AEDA", "#07BCED", "#12C6F8", "#25CBF8", "#39CFF9", "#4DD4F9", "#61D9FA", "#74DEFB", "#88E2FB", "#B0ECFC", "#C4F1FD", "#EBFAFE")

# Graphs
# 1a School Bus Ridership
non_dis <- 150000 - 66000
dis <- 66000

rs_df <- data.frame(
  Category = factor(c("Dis", "Non-Dis"), levels = c("Dis", "Non-Dis")),
  Count = c(dis, non_dis)
)

# Convert to percentages
rs_df$Percent <- rs_df$Count / sum(rs_df$Count) * 100
rs_df


ggplot(rs_df, aes(x = 1, y = Count, fill = Category)) +
  geom_col(width = 0.3, position = "stack") +
  coord_flip() +  # make it horizontal
  geom_text(aes(label = paste0(rs_df$Percent, "%")),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 15,
            fontface = "bold") +
  scale_fill_manual(values = c(cust_colours[4], cust_colours[11])) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none"
  )

# 2a Delays ADD MEAN, OUTLIERS, ETC.
ggplot(data=df_filtered, aes(x = "", y = Delay_clean)) +
  geom_boxplot(size = 0.8, width = 0.9, fill=cust_colours[10]) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(
      hjust = 0.5,
      size =30,
      face = "bold")
  )+
  scale_y_continuous(labels = NULL) +
  labs(x = NULL, y = NULL, title = "School Bus Delays")

min(df_filtered$Delay_clean)
quantile(df_filtered$Delay_clean, probs = 0.25)
mean(df_filtered$Delay_clean)
median(df_filtered$Delay_clean)
quantile(df_filtered$Delay_clean, probs = 0.75)
max(df_filtered$Delay_clean)




# 2b Delay Causes
cause_table <- df_filtered %>%
  group_by(Reason) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Count) %>%
  mutate(Reason = factor(Reason, levels = Reason))

ggplot(cause_table, aes(x = Count, y = forcats::fct_reorder(Reason, Count), fill = Count)) +
  geom_col(color = "White", size = 1) +
  scale_fill_gradient(low = cust_colours[11], high = cust_colours[1]) +
  geom_text(aes(label = Reason),
            hjust = 1,
            x = -6000,
            size = 10,
            fontface = "bold") +
  geom_text(aes(label = Count, x = Count),
            hjust = -0.1,
            size = 8) +
  scale_x_continuous(expand = expansion(add = c(90000, 30000))) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL, title = "Frequency of Delay Causes")


# 2c Operation Error Index
result %>%
  filter(proportion > 0) %>%
  arrange(proportion) %>%
  mutate(Bus_company_name = fct_inorder(Bus_company_name)) %>%
  ggplot(aes(x = proportion, y = Bus_company_name)) +
  geom_col(aes(fill = proportion)) +
  geom_text(aes(label = Bus_company_name, x=-0.01),
            hjust = 1, size = 4.5, color = "black") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)),
            hjust = -0.2, size = 4, color = "black") +
  scale_fill_gradient(low = cust_colours[12], high = cust_colours[1])  +
  scale_x_continuous(expand = expansion(mult = c(0.5, 0.1))) +
  labs(title = "Bus Companies Ranked by Operation Error Index",
       x = "Proportion", y = NULL) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL, title = "Operators by Operation Error Index")



