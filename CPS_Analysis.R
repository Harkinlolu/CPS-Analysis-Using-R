## 1. INSTALL AND LOAD REQUIRED LIBRARIES
install_and_load_all <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(sprintf("Installing missing package: %s", pkg))
      install.packages(pkg, dependencies = TRUE)
    }
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}

required_packages <- c(
  # Core Data Handling
  "tidyverse", "fs", "lubridate", "janitor", "glue", "reshape2",
  
  # Tidymodels Core + Sub-packages
  "tidymodels", "recipes", "parsnip", "workflows", "rsample", "yardstick", "tune", "dials", "broom",
  
  # Modeling Engines
  "glmnet", "ranger", "xgboost", "rpart",
  
  # Tuning & Interpretation
  "vip", "doParallel",
  
  # Traditional ML & Evaluation
  "caret", "Metrics",
  
  # Clustering & Distance
  "cluster", "dbscan", "dendextend", "pracma",
  
  # Visualization Enhancements
  "ggrepel", "viridis", "scales", "GGally",
  
  # Statistical Analysis
  "rstatix", "FSA", "e1071", "inflection",
  
  # Plot Layouts and Tables
  "knitr", "gridExtra",
  
  # String Interpolation
  "glue",
  
  # Time Series Utilities
  "zoo"
)

install_and_load_all(required_packages)


# Core Data Handling
library(tidyverse)    # dplyr, ggplot2, readr, tidyr, stringr, purrr
library(fs)           # File/path manipulation
library(lubridate)    # Date/time parsing
library(janitor)      # clean_names(), tabyl()
library(glue)         # String interpolation for dynamic labels and messages
library(reshape2)     # melt(), dcast()

# Modeling & Tidymodels
library(tidymodels)   # Tidymodels meta-package

# Explicit tidymodels sub-packages (for clarity and direct use)
library(recipes)       # Data preprocessing (step_* functions)
library(parsnip)       # Model specification (e.g. rand_forest, logistic_reg)
library(workflows)     # Combine model + recipe into a workflow
library(rsample)       # Train-test split, cross-validation
library(yardstick)     # Model evaluation metrics (AUC, accuracy, etc.)
library(tune)          # Tuning (grid search, Bayesian)
library(dials)         # Hyperparameter ranges
library(broom)         # Tidy model outputs

# Engines for tidymodels
library(glmnet)        # Lasso, Ridge regression
library(ranger)        # Random Forest engine
library(xgboost)       # XGBoost engine
library(rpart)         # Decision Tree engine

# Tuning & Interpretation
library(vip)           # Variable importance plots
library(doParallel)    # Parallel tuning support

# Traditional ML & Evaluation
library(caret)         # Classic ML training and evaluation
library(Metrics)       # rmse(), mae(), etc.

# Clustering & Distance
library(cluster)       # silhouette(), pam()
library(dbscan)        # DBSCAN clustering
library(dendextend)    # Dendrogram enhancements
library(pracma)        # Elbow detection utilities

# Visualization Enhancements
library(ggrepel)       # Non-overlapping text labels
library(viridis)       # Colorblind-safe palettes
library(scales)        # Format axes, percentages, rescaling
library(GGally)        # Provides ggpairs() for scatterplot matrix (pair plot)

# Statistical Analysis
library(rstatix)       # ANOVA, t-tests, post-hoc
library(FSA)           # Dunn’s test, fisheries stats
library(e1071)         # skewness(), kurtosis()
library(inflection)    # Detect inflection points in trend analysis

# Plot Layouts and Tables
library(knitr)         # kable()
library(gridExtra)     # Arrange multiple ggplots

# Time Series Utilities
library(zoo)           # rollmean(), time-series smoothing


## 2. DATA PREPROCESSING
# ==== Define Base Directory and Years of Interest ====
base_dir <- "CPS_Data"
target_years <- c("2014", "2015")

# ==== Identify Target Year Folders ====
year_folders <- dir_ls(base_dir, type = "directory") %>%
  keep(~ basename(.) %in% target_years)

# ==== Load Data from 2014 and 2015 ====
all_data <- list()

for (year_folder in year_folders) {
  year <- basename(year_folder)
  csv_files <- dir_ls(year_folder, regexp = "\\.csv$", recurse = FALSE)
  
  for (csv_file in csv_files) {
    filename <- tolower(path_file(csv_file))
    parts <- str_split(filename, "_", simplify = TRUE)
    
    # Attempt to extract month name from filename
    month <- NA
    for (part in parts) {
      try_month <- try(parse_date_time(part, orders = "B"), silent = TRUE)
      if (!inherits(try_month, "try-error") && !is.na(try_month)) {
        month <- format(try_month, "%B")
        break
      }
    }
    
    # Read the CSV and process
    df <- read_csv(csv_file, show_col_types = FALSE)
    
    if ("Date" %in% colnames(df)) {
      df <- df %>% mutate(Date = suppressWarnings(as.Date(Date)))
    }
    
    df <- df %>%
      mutate(Year = as.integer(year),
             Month = ifelse(!is.na(month), month, "Unknown"))
    
    all_data[[length(all_data) + 1]] <- df
  }
}

# ==== Fill Missing November 2015 Using 2016 Data ====
november_2016_path <- NULL
year_2016_folder <- path(base_dir, "2016")

if (dir_exists(year_2016_folder)) {
  november_files <- dir_ls(year_2016_folder, regexp = "november.*\\.csv$", recurse = FALSE)
  if (length(november_files) > 0) {
    november_2016_path <- november_files[1]
  }
}

if (!is.null(november_2016_path)) {
  df_nov16 <- read_csv(november_2016_path, show_col_types = FALSE)
  if ("Date" %in% colnames(df_nov16)) {
    df_nov16 <- df_nov16 %>% mutate(Date = suppressWarnings(as.Date(Date)))
  }
  df_nov16 <- df_nov16 %>%
    mutate(Year = 2015,
           Month = "November")
  all_data[[length(all_data) + 1]] <- df_nov16
  cat("November 2016 data used to fill missing November 2015.\n")
} else {
  cat("No November 2016 data found to substitute for November 2015.\n")
}
# ==== Combine, Sort, and Format Merged Data ====
cps_data <- bind_rows(all_data)
# Create 'YearMonth' column and format it
cps_data <- cps_data %>%
  mutate(YearMonth = parse_date_time(paste(Month, Year), orders = "B Y", quiet = TRUE),
         YearMonth = format(YearMonth, "%Y-%m")) %>%
  arrange(YearMonth)
# Set month as ordered factor
month_order <- month.name
cps_data <- cps_data %>%
  mutate(Month = factor(Month, levels = month_order, ordered = TRUE))
# ==== Display Info and Save ====
print(cps_data %>% select(Year, Month, YearMonth) %>% distinct())
print(str(cps_data))
print(head(cps_data))
print(tail(cps_data))
# Dataset Inspection
# Check the structure of the dataset: variable names and types
str(cps_data)

# Check the dimensions: number of rows and columns
dim(cps_data)

# Check for missing values in each column
colSums(is.na(cps_data))

# Check for duplicate rows in the dataset
sum(duplicated(cps_data)) 

# View the actual duplicate rows
cps_data[duplicated(cps_data), ]

# Save to CSV
output_path <- "cps_2014_2015_merged.csv"
write_csv(cps_data, output_path)
cat(sprintf("Data saved to %s\n", output_path))


## 3. DATA CLEANING
# === Load and Clean Data ===
cps_df <- read_csv("cps_2014_2015_merged.csv", show_col_types = FALSE)

cps_df <- cps_df %>%
  rename_with(str_trim) %>%
  clean_names()

if ("x1" %in% names(cps_df)) {
  names(cps_df)[names(cps_df) == "x1"] <- "region"
} else if ("...1" %in% names(cps_df)) {
  names(cps_df)[names(cps_df) == "...1"] <- "region"
}

if (!"region" %in% colnames(cps_df)) {
  stop("ERROR: No 'region' column found in the dataset.")
}

# === Region and Date Cleaning ===
cps_df <- cps_df %>%
  mutate(region = str_to_lower(str_replace_all(as.character(region), " ", ""))) %>%
  filter(region != "national")

if ("year_month" %in% colnames(cps_df)) {
  cps_df <- cps_df %>%
    mutate(year_month = parse_date_time(year_month, orders = "ym"))
}


# Dataset Inspection
# Check the structure of the dataset: variable names and types
str(cps_df)

# Check the dimensions: number of rows and columns
dim(cps_df)

# Check for missing values in each column
colSums(is.na(cps_df))

# Check for duplicate rows in the dataset
sum(duplicated(cps_df)) 

# View the actual duplicate rows
cps_df[duplicated(cps_df), ]


# === Clean Percentage and Count Columns ===
percentage_cols <- cps_df %>% select(contains("percentage")) %>% names()
excluded_cols <- c("region", "month", "year_month")
count_cols <- setdiff(setdiff(names(cps_df), excluded_cols), percentage_cols)

cps_df <- cps_df %>%
  mutate(across(all_of(percentage_cols), ~ str_replace_all(as.character(.), c("%" = "", "-" = "0"))),
         across(all_of(percentage_cols), as.numeric),
         across(all_of(count_cols), ~ str_replace_all(as.character(.), c("," = "", "-" = "0"))),
         across(all_of(count_cols), as.numeric))

# === Add Summary Columns ===
conviction_cols <- names(cps_df)[str_detect(names(cps_df), "convictions") & !str_detect(names(cps_df), "percentage")]
unsuccessful_cols <- names(cps_df)[str_detect(names(cps_df), "unsuccessful") & !str_detect(names(cps_df), "percentage")]

cps_df <- cps_df %>%
  mutate(
    total_convictions = rowSums(select(., all_of(conviction_cols)), na.rm = TRUE),
    total_unsuccessful = rowSums(select(., all_of(unsuccessful_cols)), na.rm = TRUE)
  )

mean_conviction_cols <- names(cps_df)[str_detect(names(cps_df), "convictions") & str_detect(names(cps_df), "percentage")]
cps_df <- cps_df %>%
  mutate(mean_conviction_percent = rowMeans(select(., all_of(mean_conviction_cols)), na.rm = TRUE))

# === Group Regions ===
region_groups <- list(
  North = c("greatermanchester", "lancashire", "cumbria", "merseyside", "northumbria", "durham", "northyorkshire",
            "westyorkshire", "southyorkshire", "cleveland", "humberside", "cheshire"),
  Midlands = c("derbyshire", "nottinghamshire", "leicestershire", "lincolnshire", "westmidlands", "warwickshire",
               "staffordshire", "northamptonshire", "westmercia"),
  South = c("essex", "kent", "hampshire", "surrey", "sussex", "thamesvalley", "cambridgeshire", "hertfordshire",
            "bedfordshire", "wiltshire"),
  South_West = c("avonandsomerset", "dorset", "devonandcornwall", "gloucestershire"),
  Wales = c("southwales", "northwales", "dyfedpowys", "gwent"),
  London = c("metropolitanandcity"),
  East = c("norfolk", "suffolk")
)

region_map <- unlist(lapply(names(region_groups), function(group) {
  setNames(rep(group, length(region_groups[[group]])), region_groups[[group]])
}))
cps_df$region_group <- region_map[cps_df$region]

# Basic descriptive stats for all numeric columns
cps_df %>%
  select(where(is.numeric)) %>%
  summary()


## 4. === DATA VISUALISATION ===
# === Skewness Histograms for Numeric Columns ===
conviction_data_cleaned <- cps_df %>%
  select(all_of(conviction_cols)) %>%
  mutate(across(everything(), ~ as.numeric(.)))

valid_conviction_data <- conviction_data_cleaned %>%
  select(where(~ sd(., na.rm = TRUE) > 0))

batch_size <- 6
split_cols <- split(colnames(valid_conviction_data), ceiling(seq_along(colnames(valid_conviction_data)) / batch_size))

for (batch in split_cols) {
  par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
  for (col in batch) {
    data <- na.omit(valid_conviction_data[[col]])
    hist(data, breaks = 30, probability = TRUE,
         main = paste0(col, "\nSkewness: ", round(skewness(data), 2)),
         xlab = "Count", col = "skyblue", border = "white")
  }
  if (interactive()) readline(prompt = "Press [Enter] to continue...")
}
par(mfrow = c(1, 1))


# Monthly Total Convictions
cps_df %>%
  group_by(year_month, region_group) %>%
  summarise(Total = sum(total_convictions, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year_month, y = Total, color = region_group)) +
  geom_line() +
  geom_point() +
  labs(title = "Monthly Total Convictions by Region Group", x = "Year-Month", y = "Total Convictions") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Conviction Rate Over Time
cps_df %>%
  group_by(year_month, region_group) %>%
  summarise(Rate = mean(mean_conviction_percent, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year_month, y = Rate, color = region_group)) +
  geom_line() + geom_point() +
  labs(title = "Average Conviction Rate by Region Group", y = "Conviction Rate (%)") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Monthly Unsuccessful Outcomes
cps_df %>%
  group_by(year_month, region_group) %>%
  summarise(Total = sum(total_unsuccessful, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year_month, y = Total, color = region_group)) +
  geom_line() + geom_point() +
  labs(title = "Monthly Unsuccessful Outcomes by Region Group", y = "Total Unsuccessful Outcomes") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# National Average Conviction Rate
cps_df %>%
  group_by(year_month) %>%
  summarise(NationalRate = mean(mean_conviction_percent, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year_month, y = NationalRate)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  labs(title = "National Average Conviction Rate Over Time", y = "Mean Conviction Rate (%)") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar Plot: Average Convictions by Region
cps_df %>%
  group_by(region_group) %>%
  summarise(Average = mean(total_convictions, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = reorder(region_group, -Average), y = Average)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Total Convictions by Region Group", y = "Average Convictions") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Conviction vs Unsuccessful Ratios
cps_df %>%
  mutate(
    TotalOutcomes = total_convictions + total_unsuccessful,
    conviction_ratio = total_convictions / TotalOutcomes,
    unsuccessful_ratio = total_unsuccessful / TotalOutcomes
  ) %>%
  group_by(region_group) %>%
  summarise(across(c(conviction_ratio, unsuccessful_ratio), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = c(conviction_ratio, unsuccessful_ratio), names_to = "Outcome", values_to = "Proportion") %>%
  ggplot(aes(x = region_group, y = Proportion, fill = Outcome)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Average Outcome Ratio by Region Group", y = "Proportion") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# === Boxplot of Total Convictions by Region Group ===
ggplot(cps_df, aes(x = region_group, y = total_convictions)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot of Total Convictions by Region Group", x = "Region Group", y = "Total Convictions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# === Distribution of Total Convictions with KDE ===
ggplot(cps_df, aes(x = total_convictions)) +
  geom_histogram(bins = 30, fill = "skyblue", alpha = 0.7) +
  geom_density(color = "darkblue", size = 1) +
  labs(title = "Distribution of Total Convictions", x = "Total Convictions", y = "Frequency") +
  theme_minimal()


# === Rolling Average Conviction Rate (3-Month Smoothing) ===
cps_df %>%
  group_by(year_month) %>%
  summarise(Rate = mean(mean_conviction_percent, na.rm = TRUE), .groups = "drop") %>%
  mutate(RollingAvg = zoo::rollmean(Rate, 3, fill = NA, align = "right")) %>%
  ggplot(aes(x = year_month)) +
  geom_line(aes(y = Rate), color = "blue", size = 1, linetype = "solid") +
  geom_line(aes(y = RollingAvg), color = "red", linetype = "dashed") +
  labs(title = "National Average Conviction Rate with 3-Month Smoothing", x = "Year-Month", y = "Mean Conviction Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  guides(color = "none")


# === Seasonal Bar Plot: Total Convictions by Season and Year ===
get_season <- function(month) {
  case_when(
    month %in% c("december", "january", "february") ~ "Winter",
    month %in% c("march", "april", "may") ~ "Spring",
    month %in% c("june", "july", "august") ~ "Summer",
    TRUE ~ "Autumn"
  )
}

# Assign season to each record
cps_df <- cps_df %>%
  mutate(season = get_season(tolower(month)))

# Aggregate by season and year
seasonal_totals <- cps_df %>%
  group_by(season, year) %>%
  summarise(total_convictions = sum(total_convictions, na.rm = TRUE), .groups = "drop") %>%
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn")))

# Calculate % for annotation
seasonal_totals <- seasonal_totals %>%
  group_by(year) %>%
  mutate(pct = 100 * total_convictions / sum(total_convictions)) %>%
  ungroup()

# Plot
ggplot(seasonal_totals, aes(x = season, y = total_convictions, fill = factor(year))) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("2014" = "red", "2015" = "blue")) +
  labs(title = "Total Convictions by Season (2014 vs 2015)", x = "Season", y = "Total Convictions", fill = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


# ==== Offence Outcomes by Region Group / Region ====
offence_types <- unique(
  str_remove_all(
    str_extract(names(cps_df), "number_of_(.*?)(_convictions|_unsuccessful)"),
    "number_of_|_convictions|_unsuccessful"
  )
)
offence_types <- offence_types[!is.na(offence_types)]

plot_offence_distribution <- function(df, offence_type, by_group = TRUE) {
  conv_col <- paste0("number_of_", offence_type, "_convictions")
  unsucc_col <- paste0("number_of_", offence_type, "_unsuccessful")
  
  if (!(conv_col %in% names(df)) || !(unsucc_col %in% names(df))) return(NULL)
  
  group_var <- if (by_group) "region_group" else "region"
  
  df_grouped <- df %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      convictions = sum(.data[[conv_col]], na.rm = TRUE),
      unsuccessful = sum(.data[[unsucc_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(total = convictions + unsuccessful,
           conviction_pct = 100 * convictions / total,
           unsuccessful_pct = 100 * unsuccessful / total)
  
  df_long <- df_grouped %>%
    pivot_longer(cols = c(convictions, unsuccessful), names_to = "Outcome", values_to = "Count")
  
  ggplot(df_long, aes(x = .data[[group_var]], y = Count, fill = Outcome)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = ifelse(Outcome == "convictions", 
                                 sprintf("%.1f%%", conviction_pct), 
                                 sprintf("%.1f%%", unsuccessful_pct))),
              position = position_stack(vjust = 0.5), size = 3, color = "white") +
    labs(
      title = paste(str_to_title(str_replace_all(offence_type, "_", " ")),
                    "Outcomes by", ifelse(by_group, "Region Group", "Region")),
      x = ifelse(by_group, "Region Group", "Region"),
      y = "Number of Cases"
    ) +
    scale_fill_manual(values = c("convictions" = "steelblue", "unsuccessful" = "firebrick")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = ifelse(by_group, 45, 90), hjust = 1))
}


# ==== Top 5 Offences by Region ====
# Extract conviction columns
conviction_cols <- names(cps_df)[str_detect(names(cps_df), "number_of_.*_convictions")]

# Prepare data
top5_df <- cps_df %>%
  group_by(region) %>%
  summarise(across(all_of(conviction_cols), sum, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(-region, names_to = "offence", values_to = "convictions") %>%
  mutate(
    offence = offence %>%
      str_replace_all("number_of_|_convictions", "") %>%
      str_replace_all("_", " ") %>%
      str_to_title()
  ) %>%
  group_by(region) %>%
  slice_max(order_by = convictions, n = 5, with_ties = FALSE) %>%
  ungroup()

# Plot individually by region
unique_regions <- unique(top5_df$region)

for (reg in unique_regions) {
  region_data <- top5_df %>% filter(region == reg)
  
  p <- ggplot(region_data, aes(x = convictions, y = fct_reorder(offence, convictions))) +
    geom_col(fill = "#0073C2FF") +
    labs(
      title = paste("Top 5 Offences in", reg),
      x = "Total Convictions",
      y = "Offence Type"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
      axis.text.y = element_text(size = 11),
      axis.text.x = element_text(size = 11)
    )
  
  print(p)
}

# === Grouped Bar Charts per Region Group for 3 of the Top offences ===
# Choose selected offences
selected_offences <- c("theft_and_handling", "motoring_offences", "public_order_offences")

# Aggregate total convictions per offence by region group
bar_data <- cps_df %>%
  group_by(region_group) %>%
  summarise(across(all_of(paste0("number_of_", selected_offences, "_convictions")), 
                   sum, na.rm = TRUE), .groups = "drop")

# Reshape data into long format
bar_data_long <- bar_data %>%
  pivot_longer(
    cols = -region_group,
    names_to = "offence",
    values_to = "count"
  ) %>%
  mutate(
    offence = str_remove(offence, "number_of_"),
    offence = str_remove(offence, "_convictions"),
    offence = str_replace_all(offence, "_", " "),
    offence = str_to_title(offence)
  )

# Plot grouped bar chart
ggplot(bar_data_long, aes(x = region_group, y = count, fill = offence)) +
  geom_col(position = "dodge") +
  labs(
    title = "Total Convictions by Offence Type and Region Group",
    x = "Region Group", y = "Total Convictions", fill = "Offence Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ==== Conviction Success Rate by Offence Type ====
success_df <- map_dfr(offence_types, function(offence) {
  conv_col <- paste0("number_of_", offence, "_convictions")
  unsucc_col <- paste0("number_of_", offence, "_unsuccessful")
  if (conv_col %in% names(cps_df) && unsucc_col %in% names(cps_df)) {
    total_conv <- sum(cps_df[[conv_col]], na.rm = TRUE)
    total_unsucc <- sum(cps_df[[unsucc_col]], na.rm = TRUE)
    total <- total_conv + total_unsucc
    if (total > 0) {
      return(tibble(offence = str_to_title(str_replace_all(offence, "_", " ")),
                    success_rate = total_conv / total))
    }
  }
  NULL
})

ggplot(success_df, aes(x = reorder(offence, success_rate), y = success_rate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Conviction Success Rate by Offence Type", x = "Offence Type", y = "Success Rate") +
  theme_minimal()

# ==== National Outcome Distribution ====
# Total values
total_conv <- sum(cps_df$total_convictions, na.rm = TRUE)
total_unsucc <- sum(cps_df$total_unsuccessful, na.rm = TRUE)
total_admin <- if ("number_of_admin_finalised_unsuccessful" %in% names(cps_df)) {
  sum(cps_df$number_of_admin_finalised_unsuccessful, na.rm = TRUE)
} else 0

outcome_data <- tibble(
  Outcome = c("Convicted", "Unsuccessful", "Admin Finalised"),
  Count = c(total_conv, total_unsucc, total_admin)
)

ggplot(outcome_data, aes(x = "", y = Count, fill = Outcome)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Count / sum(Count) * 100, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "National Outcome Distribution") +
  theme_void() +
  scale_fill_manual(values = c("steelblue", "tomato", "grey"))


# ==== Total Unsuccessful Outcomes by Offence Type ====
# Identify offence types based on unsuccessful columns
unsuccessful_cols <- names(cps_df)[str_detect(names(cps_df), "number_of_.*_unsuccessful")]

# Aggregate unsuccessful counts per offence type across the dataset
unsuccessful_summary <- cps_df %>%
  summarise(across(all_of(unsuccessful_cols), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "offence", values_to = "count") %>%
  mutate(
    offence = offence %>%
      str_replace_all("number_of_|_unsuccessful", "") %>%
      str_replace_all("_", " ") %>%
      str_to_title()
  ) %>%
  arrange(desc(count))

# Plot
ggplot(unsuccessful_summary, aes(x = reorder(offence, count), y = count)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Total Unsuccessful Outcomes by Offence Type",
    x = "Offence Type",
    y = "Number of Unsuccessful Outcomes"
  ) +
  theme_minimal(base_size = 13)


# === HEATMAP ===
# === Define Heatmap Function with Custom Color Palette ===
plot_gg_correlation_heatmap <- function(data, keyword, title = "Correlation Heatmap") {
  cols <- grep(keyword, names(data), value = TRUE)
  
  sub_data <- data %>%
    select(all_of(cols)) %>%
    mutate(across(everything(), as.numeric)) %>%
    select(where(~ sd(., na.rm = TRUE) > 0))
  
  if (ncol(sub_data) < 2) {
    warning("Not enough columns with variation for correlation heatmap.")
    return(NULL)
  }
  
  corr_matrix <- cor(sub_data, use = "pairwise.complete.obs")
  melted_corr <- melt(corr_matrix)
  
  # Clip values below 0.5
  melted_corr$value_clipped <- pmax(melted_corr$value, 0.5)
  
  # Custom distinct color palette
  custom_colors <- c(
    "#FFE5B4",   # Cream
    "#FF7F50",   # Coral / Orange
    "#FF0000",   # Red
    "#87CEEB",   # Light Blue
    "#4682B4",   # Steel Blue
    "#0000CD",   # Medium Blue
    "#800080"    # Purple
  )
  
  # Define stops corresponding to 0.5 → 1.0
  color_breaks <- rescale(c(0.5, 0.6, 0.7, 0.78, 0.85, 0.93, 1.0))
  
  ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value_clipped)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(
      colours = custom_colors,
      values = color_breaks,
      limits = c(0.5, 1.0),
      name = "Correlation"
    ) +
    geom_text(aes(label = sprintf("%.2f", value)), size = 3, color = "black") +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(size = 9),
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(title = title, x = NULL, y = NULL)
}

# === Identify Conviction and Unsuccessful Columns ===
all_cols <- names(cps_df)

conviction_cols <- all_cols[grepl("number_of_.*_convictions$", all_cols) &
                              !grepl("percentage", all_cols) &
                              !grepl("total", all_cols)]

unsuccessful_cols <- all_cols[grepl("number_of_.*_unsuccessful$", all_cols) &
                                !grepl("percentage", all_cols)]

conviction_cols <- sort(conviction_cols)
unsuccessful_cols <- sort(unsuccessful_cols)

# === Plot Conviction Correlation Heatmap ===
plot_gg_correlation_heatmap(
  cps_df,
  keyword = paste(conviction_cols, collapse = "|"),
  title = "Conviction Type Correlation"
)

# === Plot Unsuccessful Correlation Heatmap ===
plot_gg_correlation_heatmap(
  cps_df,
  keyword = paste(unsuccessful_cols, collapse = "|"),
  title = "Unsuccessful Outcome Correlation"
)

# === Create Total Case Columns per Offence ===
offence_names <- str_replace(conviction_cols, "number_of_", "")
offence_names <- str_replace(offence_names, "_convictions", "")

for (offence in offence_names) {
  conv_col <- paste0("number_of_", offence, "_convictions")
  unsucc_col <- paste0("number_of_", offence, "_unsuccessful")
  total_col <- paste0("total_", offence)
  
  if (conv_col %in% names(cps_df) && unsucc_col %in% names(cps_df)) {
    cps_df[[total_col]] <- rowSums(cps_df[, c(conv_col, unsucc_col)], na.rm = TRUE)
  }
}

# Add admin finalised
cps_df$total_admin_finalised <- cps_df$number_of_admin_finalised_unsuccessful

# Pattern to match total case columns
combined_cols_pattern <- paste(c(
  paste0("total_", offence_names),
  "total_admin_finalised"
), collapse = "|")

# === Plot Total Case Outcome Correlation Heatmap ===
plot_gg_correlation_heatmap(
  cps_df,
  keyword = combined_cols_pattern,
  title = "Total Case Outcome Correlation"
)



# Pairplots
# Match column names that begin with "total_" and include "total_admin_finalised"
total_case_cols <- names(cps_df)[grepl("^total_", names(cps_df))]

# Create Subset for Pair Plot
# Select only total case columns for scatterplot matrix
total_cases_df <- cps_df %>%
  select(all_of(total_case_cols)) %>%
  select(where(~ sd(., na.rm = TRUE) > 0))  # remove non-varying cols

# Plot Scatterplot Pair Plot

ggpairs(total_cases_df,
        title = "Pair Plot of Total Case Counts by Offence Type",
        upper = list(continuous = wrap("cor", size = 3)),
        lower = list(continuous = wrap("points", alpha = 0.4, size = 1.2)),
        diag = list(continuous = wrap("densityDiag")))


## 5. HYPOTHESIS TESTING USING ANOVA
# === Null Hypothesis (H₀):
# There is no significant difference in the average number of unsuccessful case outcomes 
# across offence types (e.g., sexual offences, drug offences, fraud).

# Alternative Hypothesis (H₁):
# There is a significant difference in the average number of unsuccessful case outcomes 
# across offence types.

# Filter out unwanted columns
exclude_keywords <- c("percentage", "total", "unsuccessful_ratio")

unsuccessful_cols <- names(cps_df)[
  str_detect(names(cps_df), regex("unsuccessful", ignore_case = TRUE)) &
    !str_detect(names(cps_df), regex(paste(exclude_keywords, collapse = "|"), ignore_case = TRUE)) &
    sapply(cps_df, is.numeric)
]

# Reshape the data to long format
df_melted <- cps_df %>%
  select(all_of(unsuccessful_cols)) %>%
  pivot_longer(cols = everything(), names_to = "Offence", values_to = "Unsuccessful") %>%
  mutate(
    Offence = str_to_title(
      str_replace_all(Offence, c("number_of_" = "", "_unsuccessful" = "", "_" = " "))
    )
  ) %>%
  drop_na()

# View included offence types
cat("Final offence types included:\n")
unique_offences <- unique(df_melted$Offence)
for (i in seq_along(unique_offences)) {
  cat(paste0(i, ". ", unique_offences[i], "\n"))
}

# Run ANOVA and Kruskal-Wallis tests
anova_result <- df_melted %>%
  anova_test(Unsuccessful ~ Offence)

kruskal_result <- df_melted %>%
  kruskal_test(Unsuccessful ~ Offence)

cat("\nANOVA Results:\n")
print(anova_result)

cat("\nKruskal-Wallis Results:\n")
print(kruskal_result)

# Post-hoc Dunn’s test with Bonferroni correction
dunn_results <- dunnTest(Unsuccessful ~ Offence, data = df_melted, method = "bonferroni")

# Format and display the post-hoc table
dunn_table <- dunn_results$res %>%
  arrange(P.adj) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

cat("\nPost-hoc Dunn Test Results (Bonferroni-corrected):\n")
kable(dunn_table, format = "markdown")



# ==== Create a copy of the DataFrame ====
cps_ml <- cps_df

# ==== View summary of columns, data types, and missing value counts ====
glimpse(cps_ml)
summary(cps_ml)    # More detailed overview of each column

# ==== Display the first few rows of data ====
head(cps_ml)

# ==== Get descriptive statistics for numeric columns ====
cps_ml %>%
  select(where(is.numeric)) %>%
  summary()


## 6. REGRESSION TECHNIQUE
# Predict the total number of unsuccessful outcomes using only violent offence conviction counts as predictors.

# === Prepare the data ===
cps_lr <- cps_df

# Select predictors and target
predictors <- c(
  "number_of_homicide_convictions",
  "number_of_offences_against_the_person_convictions",
  "number_of_sexual_offences_convictions",
  "number_of_robbery_convictions"
)
target <- "total_unsuccessful"

# Filter complete cases
data <- cps_lr %>% select(all_of(c(predictors, target))) %>% drop_na()

# Split into train/test (70/30)
set.seed(42)
train_index <- createDataPartition(data[[target]], p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data  <- data[-train_index, ]

X_train <- as.matrix(train_data[, predictors])
y_train <- train_data[[target]]
X_test  <- as.matrix(test_data[, predictors])
y_test  <- test_data[[target]]

# === Train models ===
lm_model <- lm(total_unsuccessful ~ ., data = train_data)
ridge_model <- cv.glmnet(X_train, y_train, alpha = 0)
lasso_model <- cv.glmnet(X_train, y_train, alpha = 1)

# === Predict & Evaluate ===
predict_and_evaluate <- function(model, model_type) {
  if (model_type == "Linear") {
    preds <- predict(model, newdata = test_data)
  } else {
    preds <- predict(model, newx = X_test, s = "lambda.min")
  }
  rmse_val <- rmse(y_test, preds)
  mae_val  <- mae(y_test, preds)
  r2_val   <- caret::R2(preds, y_test)
  return(c(RMSE = rmse_val, MAE = mae_val, R2 = r2_val))
}

results <- rbind(
  Linear = predict_and_evaluate(lm_model, "Linear"),
  Ridge = predict_and_evaluate(ridge_model, "Ridge"),
  Lasso = predict_and_evaluate(lasso_model, "Lasso")
)

cat("\nModel Evaluation Metrics:\n")
as.data.frame(results) %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  print()

# === Coefficients ===
coef_df <- data.frame(
  Feature = predictors,
  Linear = coef(lm_model)[-1],
  Ridge  = as.numeric(coef(ridge_model, s = "lambda.min")[-1]),
  Lasso  = as.numeric(coef(lasso_model, s = "lambda.min")[-1])
)

cat("\nFeature Importance (Model Coefficients):\n")
coef_df %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  print()

# === 5-Fold Cross Validation ===
ctrl <- trainControl(method = "cv", number = 5)

cv_lm <- train(total_unsuccessful ~ ., data = data, method = "lm", trControl = ctrl, metric = "Rsquared")

cv_ridge <- train(
  x = X_train, y = y_train, method = "glmnet",
  tuneGrid = expand.grid(alpha = 0, lambda = ridge_model$lambda.min),
  trControl = ctrl, metric = "Rsquared"
)

cv_lasso <- train(
  x = X_train, y = y_train, method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = lasso_model$lambda.min),
  trControl = ctrl, metric = "Rsquared"
)

cv_results <- data.frame(
  Model = c("Linear", "Ridge", "Lasso"),
  Mean_R2 = c(
    mean(cv_lm$results$Rsquared),
    mean(cv_ridge$results$Rsquared),
    mean(cv_lasso$results$Rsquared)
  ),
  SD_R2 = c(
    sd(cv_lm$resample$Rsquared),
    sd(cv_ridge$resample$Rsquared),
    sd(cv_lasso$resample$Rsquared)
  )
)

cat("\n5-Fold Cross-Validation R² Scores:\n")
cv_results %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  print()

# === Actual vs Predicted Plots ===
plot_actual_vs_pred <- function(model, model_type, title) {
  preds <- if (model_type == "Linear") {
    predict(model, newdata = test_data)
  } else {
    predict(model, newx = X_test, s = "lambda.min")
  }
  ggplot(data = NULL, aes(x = y_test, y = preds)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(title = title, x = "Actual", y = "Predicted") +
    theme_minimal()
}

p1 <- plot_actual_vs_pred(lm_model, "Linear", "Actual vs Predicted - Linear")
p2 <- plot_actual_vs_pred(ridge_model, "Ridge", "Actual vs Predicted - Ridge")
p3 <- plot_actual_vs_pred(lasso_model, "Lasso", "Actual vs Predicted - Lasso")

grid.arrange(p1, p2, p3, ncol = 3)

# === Residual Plots ===
plot_residuals <- function(model, model_type, title) {
  preds <- if (model_type == "Linear") {
    predict(model, newdata = test_data)
  } else {
    predict(model, newx = X_test, s = "lambda.min")
  }
  residuals <- y_test - preds
  ggplot(data = NULL, aes(x = preds, y = residuals)) +
    geom_point(alpha = 0.6, color = "darkorange") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(title = title, x = "Predicted", y = "Residuals") +
    theme_minimal()
}

r1 <- plot_residuals(lm_model, "Linear", "Residuals - Linear")
r2 <- plot_residuals(ridge_model, "Ridge", "Residuals - Ridge")
r3 <- plot_residuals(lasso_model, "Lasso", "Residuals - Lasso")

grid.arrange(r1, r2, r3, ncol = 3)



## 7. CLUSTERING
# Null Hypothesis (H₀): There is no meaningful regional grouping based on offence types
# Alternative Hypothesis (H₁): Regions can be grouped into meaningful clusters based on offence types, which reflect systematic similarities or differences.

#  K-MEans Clustering
# === Create total offence columns ===
cps_ml <- cps_df %>%
  mutate(
    total_theft_offences = number_of_theft_and_handling_convictions + number_of_theft_and_handling_unsuccessful,
    total_violence_offences = number_of_offences_against_the_person_convictions + number_of_offences_against_the_person_unsuccessful,
    total_motoring_offences = number_of_motoring_offences_convictions + number_of_motoring_offences_unsuccessful,
    total_drugs_offences = number_of_drugs_offences_convictions + number_of_drugs_offences_unsuccessful,
    total_public_order_offences = number_of_public_order_offences_convictions + number_of_public_order_offences_unsuccessful
  )

# === Aggregate by region ===
region_cluster_df <- cps_ml %>%
  group_by(region) %>%
  summarise(
    across(starts_with("total_"), mean, na.rm = TRUE),
    .groups = "drop"
  )

# === Scale features ===
offence_columns <- c(
  "total_theft_offences",
  "total_violence_offences",
  "total_motoring_offences",
  "total_drugs_offences",
  "total_public_order_offences"
)
scaled_matrix <- scale(region_cluster_df[, offence_columns])

# === Elbow and Silhouette Methods ===
wss <- vector()
silhouette_scores <- vector()

for (k in 2:10) {
  kmeans_model <- kmeans(scaled_matrix, centers = k, nstart = 25)
  wss[k] <- kmeans_model$tot.withinss
  sil <- silhouette(kmeans_model$cluster, dist(scaled_matrix))
  silhouette_scores[k] <- mean(sil[, 3])
}

# Plot Elbow Method
plot(2:10, wss[2:10], type = "b", pch = 19,
     xlab = "Number of Clusters (k)",
     ylab = "Within-cluster Sum of Squares (WSS)",
     main = "Elbow Method for Optimal k")

# Plot Silhouette Scores
plot(2:10, silhouette_scores[2:10], type = "b", pch = 19, col = "forestgreen",
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Scores for Various k")

# === Clustering with k = 3 ===
set.seed(42)
final_kmeans <- kmeans(scaled_matrix, centers = 3, nstart = 25)
region_cluster_df$Cluster <- as.factor(final_kmeans$cluster)

# Print silhouette score for k=3
sil <- silhouette(final_kmeans$cluster, dist(scaled_matrix))
silhouette_avg <- mean(sil[, 3])
cat(sprintf("\nSilhouette Score for k=3: %.4f\n", silhouette_avg))

# Show cluster assignments
summary_df <- region_cluster_df %>% select(region, Cluster) %>% arrange(Cluster)
cat("\nSummary of Region Cluster Assignments:\n")
print(summary_df)


# === Pairwise Plots ===
pairs <- combn(offence_columns, 2, simplify = FALSE)

for (pair in pairs) {
  p <- ggplot(region_cluster_df, aes(x = .data[[pair[1]]], y = .data[[pair[2]]], color = Cluster)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_text_repel(aes(label = region), size = 3, max.overlaps = 10) +
    labs(
      title = paste(str_to_title(gsub("_", " ", pair[1])), "vs", str_to_title(gsub("_", " ", pair[2]))),
      x = str_to_title(gsub("_", " ", pair[1])),
      y = str_to_title(gsub("_", " ", pair[2]))
    ) +
    theme_minimal() +
    theme(legend.position = "top") +
    scale_color_viridis_d()
  
  print(p)
}


# New Clustering with Excluded regions for better clustering visualisations
# === Create total offence columns ===
cps_ml <- cps_df %>%
  mutate(
    total_theft_offences = number_of_theft_and_handling_convictions + number_of_theft_and_handling_unsuccessful,
    total_violence_offences = number_of_offences_against_the_person_convictions + number_of_offences_against_the_person_unsuccessful,
    total_motoring_offences = number_of_motoring_offences_convictions + number_of_motoring_offences_unsuccessful,
    total_drugs_offences = number_of_drugs_offences_convictions + number_of_drugs_offences_unsuccessful,
    total_public_order_offences = number_of_public_order_offences_convictions + number_of_public_order_offences_unsuccessful
  )

# === Aggregate by Region ===
region_cluster_df <- cps_ml %>%
  group_by(region) %>%
  summarise(
    across(starts_with("total_"), mean, na.rm = TRUE),
    .groups = "drop"
  )

# === Exclude Specific Regions ===
excluded_regions <- c("metropolitanandcity")
region_cluster_df <- region_cluster_df %>%
  filter(!region %in% excluded_regions)

# === Scale Features ===
offence_columns <- c(
  "total_theft_offences",
  "total_violence_offences",
  "total_motoring_offences",
  "total_drugs_offences",
  "total_public_order_offences"
)
scaled_matrix <- scale(region_cluster_df[, offence_columns])

# === Elbow and Silhouette Methods ===
wss <- vector()
silhouette_scores <- vector()

for (k in 2:10) {
  kmeans_model <- kmeans(scaled_matrix, centers = k, nstart = 25)
  wss[k] <- kmeans_model$tot.withinss
  sil <- silhouette(kmeans_model$cluster, dist(scaled_matrix))
  silhouette_scores[k] <- mean(sil[, 3])
}

# Elbow Plot
plot(2:10, wss[2:10], type = "b", pch = 19,
     xlab = "Number of Clusters (k)",
     ylab = "Within-cluster Sum of Squares (WSS)",
     main = "Elbow Method for Optimal k")

# Silhouette Plot
plot(2:10, silhouette_scores[2:10], type = "b", pch = 19, col = "forestgreen",
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Scores for Various k")

# === Clustering with k = 3 ===
set.seed(42)
final_kmeans <- kmeans(scaled_matrix, centers = 3, nstart = 25)
region_cluster_df$Cluster <- as.factor(final_kmeans$cluster)

# Print Silhouette Score
sil <- silhouette(final_kmeans$cluster, dist(scaled_matrix))
silhouette_avg <- mean(sil[, 3])
cat(sprintf("\nSilhouette Score for k=3: %.4f\n", silhouette_avg))

# Show Cluster Assignments
summary_df <- region_cluster_df %>% select(region, Cluster) %>% arrange(Cluster)
cat("\nSummary of Region Cluster Assignments:\n")
print(summary_df)

# === Pairwise Scatter Plots with Region Labels ===
pairs <- combn(offence_columns, 2, simplify = FALSE)

for (pair in pairs) {
  p <- ggplot(region_cluster_df, aes(x = .data[[pair[1]]], y = .data[[pair[2]]], color = Cluster)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_text_repel(aes(label = region), size = 3, max.overlaps = 10) +
    labs(
      title = paste(str_to_title(gsub("_", " ", pair[1])), "vs", str_to_title(gsub("_", " ", pair[2]))),
      x = str_to_title(gsub("_", " ", pair[1])),
      y = str_to_title(gsub("_", " ", pair[2]))
    ) +
    theme_minimal() +
    theme(legend.position = "top") +
    scale_color_viridis_d()
  
  print(p)
}


# Agglomerative Clustering
# === Compute distance matrix and linkage ===
distance_matrix <- dist(scaled_matrix, method = "euclidean")
hc_model <- hclust(distance_matrix, method = "ward.D2")

# === Plot Dendrogram ===
plot(hc_model, labels = region_cluster_df$region, main = "Hierarchical Clustering Dendrogram (Ward Linkage)",
     xlab = "Region", ylab = "Distance", cex = 0.8)

# === Enhanced Dendrogram with Color ===
dend <- as.dendrogram(hc_model)
dend <- color_branches(dend, k = 3)
plot(dend, main = "Colored Dendrogram - 3 Clusters", ylab = "Height")

# === Silhouette Scores for Different k ===
sil_scores <- c()
for (k in 2:10) {
  cluster_labels <- cutree(hc_model, k = k)
  sil <- silhouette(cluster_labels, distance_matrix)
  sil_scores[k] <- mean(sil[, 3])
}

# Cut at distance approxiamately equals to 5.0 and number of clusters equals 3 at this point (visually inferred from dendrogram)

# Plot silhouette scores
plot(2:10, sil_scores[2:10], type = "b", pch = 19, col = "steelblue",
     xlab = "Number of Clusters (k)", ylab = "Average Silhouette Score",
     main = "Silhouette Scores for Agglomerative Clustering")

# === Apply Final Clustering (k = 3) ===
region_cluster_df$Agglomerative_Cluster <- cutree(hc_model, k = 3)
region_cluster_df$Agglomerative_Cluster <- as.factor(region_cluster_df$Agglomerative_Cluster)

# === Report Silhouette Score for k = 3 ===
final_sil <- silhouette(as.numeric(region_cluster_df$Agglomerative_Cluster), distance_matrix)
final_avg <- mean(final_sil[, 3])
cat(sprintf("Silhouette Score for Agglomerative Clustering (k=3): %.4f\n", final_avg))

# === Pairwise Cluster Plots ===
pairs <- combn(offence_columns, 2, simplify = FALSE)

for (pair in pairs) {
  p <- ggplot(region_cluster_df, aes(x = .data[[pair[1]]], y = .data[[pair[2]]], color = Agglomerative_Cluster)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_text_repel(aes(label = region), size = 3, max.overlaps = 10) +
    labs(
      title = paste("Agglomerative Clustering:", str_to_title(gsub("_", " ", pair[1])),
                    "vs", str_to_title(gsub("_", " ", pair[2]))),
      x = str_to_title(gsub("_", " ", pair[1])),
      y = str_to_title(gsub("_", " ", pair[2]))
    ) +
    theme_minimal() +
    theme(legend.position = "top") +
    scale_color_viridis_d()
  
  print(p)
}



# DBScan
# === Compute sorted k-distances ===
minPts <- 10
k_distances <- kNNdist(scaled_matrix, k = minPts)
sorted_k_distances <- sort(k_distances)

# === Automatically detect elbow point ===

# Use second derivative method to find the inflection point (elbow)
elbow <- findiplist(1:length(sorted_k_distances), sorted_k_distances, index = 1)

# i1 and i2 are possible elbow points  but i2 is preffered for DBScan
i2 <- elbow[2]
eps_estimate <- sorted_k_distances[i2]

# === Plot with eps line and label ===

ggplot(data = data.frame(index = seq_along(sorted_k_distances), distance = sorted_k_distances),
       aes(x = index, y = distance)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = eps_estimate, linetype = "dashed", color = "red") +
  annotate("text",
           x = length(sorted_k_distances) * 0.75,
           y = eps_estimate + 0.01,
           label = paste("Estimated eps =", round(eps_estimate, 3)),
           color = "red", size = 4, hjust = 0) +
  labs(
    title = "k-Distance Plot with Automatically Estimated eps",
    x = paste0("Points (sorted by ", minPts, "-NN distance)"),
    y = paste0(minPts, "-NN distance")
  ) +
  theme_minimal()


# === Apply DBSCAN ===
eps_value <- eps_estimate  # estimated eps_value was 1.638 from the graph
dbscan_model <- dbscan::dbscan(scaled_matrix, eps = eps_value, minPts = minPts)


# Assign cluster labels from DBSCAN
region_cluster_df$DBSCAN_Cluster <- dbscan_model$cluster

# Convert DBSCAN noise label 0 → -1
region_cluster_df$DBSCAN_Cluster[region_cluster_df$DBSCAN_Cluster == 0] <- -1

# Remap non-noise cluster labels to start from 0
non_noise <- region_cluster_df$DBSCAN_Cluster[region_cluster_df$DBSCAN_Cluster != -1]
unique_clusters <- sort(unique(non_noise))
cluster_mapping <- setNames(seq_along(unique_clusters) - 1, unique_clusters)

region_cluster_df$DBSCAN_Cluster <- sapply(
  region_cluster_df$DBSCAN_Cluster,
  function(x) if (x == -1) -1 else cluster_mapping[as.character(x)]
)


# Convert to factor for plotting
region_cluster_df$DBSCAN_Cluster <- factor(region_cluster_df$DBSCAN_Cluster)

# === Summary ===
n_clusters <- length(unique(region_cluster_df$DBSCAN_Cluster)) - any(region_cluster_df$DBSCAN_Cluster == -1)
n_noise <- sum(region_cluster_df$DBSCAN_Cluster == -1)

cat(sprintf("\nDBSCAN found %d cluster(s) and %d noise point(s).\n", n_clusters, n_noise))

table(region_cluster_df$DBSCAN_Cluster)


# === Silhouette Score (excluding noise) ===
if (n_clusters >= 2) {
  mask <- region_cluster_df$DBSCAN_Cluster != -1
  sil <- silhouette(
    as.numeric(region_cluster_df$DBSCAN_Cluster[mask]),
    dist(scaled_matrix[mask, ])
  )
  sil_score <- mean(sil[, 3])
  cat(sprintf("Silhouette Score for DBSCAN (excluding noise): %.4f\n", sil_score))
} else {
  sil_score <- NA
  cat("Silhouette Score could not be computed because DBSCAN found fewer than 2 clusters.\n")
}


# === Region-Cluster Summary ===
cat("\nDBSCAN Region Cluster Assignments:\n")
print(region_cluster_df %>% select(region, DBSCAN_Cluster) %>% arrange(DBSCAN_Cluster))

# === Define offence columns ===
offence_columns <- c(
  "total_theft_offences",
  "total_violence_offences",
  "total_motoring_offences",
  "total_drugs_offences",
  "total_public_order_offences"
)

# === Pairwise Cluster Visualisations ===
pairs <- combn(offence_columns, 2, simplify = FALSE)

for (pair in pairs) {
  x_var <- pair[1]
  y_var <- pair[2]
  
  p <- ggplot(region_cluster_df, aes(x = .data[[x_var]], y = .data[[y_var]], color = DBSCAN_Cluster)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_text_repel(aes(label = region), size = 3, max.overlaps = 10) +
    labs(
      title = paste("DBSCAN Clusters:", str_to_title(gsub("_", " ", x_var)),
                    "vs", str_to_title(gsub("_", " ", y_var))),
      x = str_to_title(gsub("_", " ", x_var)),
      y = str_to_title(gsub("_", " ", y_var)),
      color = "Cluster"
    ) +
    theme_minimal() +
    theme(legend.position = "top") +
    scale_color_manual(
      values = c("grey40", scales::hue_pal()(n_clusters)),
      breaks = levels(region_cluster_df$DBSCAN_Cluster)
    )
  
  print(p)
}



## 8. CLASSIFICATION
# ==== HYPOTHESIS: Predicting High Conviction Outcomes Using Crime Type and Regional Attributes: A Binary Classification Approach ====
# Create a copy of the DataFrame
cps_cl <- cps_df

# ==== Define Metric‐set and Confusion‐Matrix Plot ====
# ==== Metric set ====
metrics_all <- metric_set(
  yardstick::accuracy,
  yardstick::precision,
  yardstick::recall,
  yardstick::f_meas,
  yardstick::roc_auc
)

plot_conf_mat_heatmap <- function(preds, title) {
  cm <- conf_mat(preds, truth = high_conviction, estimate = .pred_class)
  autoplot(cm, type = "heatmap") +
    scale_fill_gradient(low = "#D6EAF8", high = "#2E86C1") +  # Optional: change colors
    labs(
      title = title,
      x = "Actual",
      y = "Predicted",
      fill = "Count"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "right"
    )
}

# ==== Prepare Dataset ====
offence_types <- c(
  "homicide", "offences_against_the_person", "sexual_offences",
  "burglary", "robbery", "theft_and_handling", "fraud_and_forgery",
  "criminal_damage", "drugs_offences", "public_order_offences",
  "all_other_offences_excluding_motoring", "motoring_offences"
)

for (offence in offence_types) {
  conviction_col <- paste0("number_of_", offence, "_convictions")
  unsuccessful_col <- paste0("number_of_", offence, "_unsuccessful")
  total_col      <- paste0("total_",         offence, "_cases")
  
  if (all(c(conviction_col, unsuccessful_col) %in% names(cps_cl))) {
    cps_cl[[conviction_col]]   <- as.numeric(as.character(cps_cl[[conviction_col]]))
    cps_cl[[unsuccessful_col]]  <- as.numeric(as.character(cps_cl[[unsuccessful_col]]))
    cps_cl[[total_col]]         <- cps_cl[[conviction_col]] + cps_cl[[unsuccessful_col]]
  } else {
    warning(paste("Columns missing for offence:", offence))
  }
}

# Coerce the admin‐finalised unsuccessful because there is no admin‐finalised conviction
cps_cl$number_of_admin_finalised_unsuccessful <- 
  as.numeric(as.character(cps_cl$number_of_admin_finalised_unsuccessful))
cps_cl$total_admin_finalised_cases <- 
  cps_cl$number_of_admin_finalised_unsuccessful

# ==== Create Binary Target Variable ====
median_conviction <- median(cps_cl$mean_conviction_percent, na.rm = TRUE)

cps_cl$high_conviction <- ifelse(cps_cl$mean_conviction_percent > median_conviction, "1", "0")

cps_cl$high_conviction <- factor(cps_cl$high_conviction, levels = c("0", "1"))


# ==== Features & Preprocess ====
feature_vars <- c(
  paste0("total_", offence_types, "_cases"),
  "total_admin_finalised_cases",
  "region_group", "season", "month", "year"
)

df <- cps_cl %>%
  select(all_of(feature_vars), high_conviction) %>%
  mutate(
    across(c("region_group", "season", "month", "year"), as.factor),
    high_conviction = factor(high_conviction, levels = c(0, 1))
  ) %>%
  drop_na()

# ==== Train-Test Split ====
set.seed(42)
data_split <- initial_split(df, prop = 0.8, strata = high_conviction)
train_data  <- training(data_split)
test_data   <- testing(data_split)

# ==== Recipe and Models ====
cps_recipe <- recipe(high_conviction ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors())

rf_model <- rand_forest(mtry = 5, trees = 500, min_n = 10) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

dt_model <- decision_tree(tree_depth = 5, cost_complexity = 0.01) %>%
  set_engine("rpart") %>%
  set_mode("classification")

gb_model <- boost_tree(trees = 500, learn_rate = 0.1, tree_depth = 3) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

rf_wf <- workflow() %>% add_model(rf_model) %>% add_recipe(cps_recipe)
dt_wf <- workflow() %>% add_model(dt_model) %>% add_recipe(cps_recipe)
gb_wf <- workflow() %>% add_model(gb_model) %>% add_recipe(cps_recipe)

# ==== Fit Models ====
rf_fit <- fit(rf_wf, data = train_data)
dt_fit <- fit(dt_wf, data = train_data)
gb_fit <- fit(gb_wf, data = train_data)

# ==== Predictions Function ====
get_preds <- function(fit, data) {
  predict(fit, data, type = "prob") %>%
    bind_cols(predict(fit, data)) %>%
    bind_cols(data %>% select(high_conviction))
}

rf_preds <- get_preds(rf_fit, test_data)
dt_preds <- get_preds(dt_fit, test_data)
gb_preds <- get_preds(gb_fit, test_data)

# ==== Evaluation Metrics ====
rf_metrics <- metrics_all(
  rf_preds,
  truth       = high_conviction,
  estimate    = .pred_class,
  .pred_1,
  event_level = "second"
)
dt_metrics <- metrics_all(
  dt_preds,
  truth       = high_conviction,
  estimate    = .pred_class,
  .pred_1,
  event_level = "second"
)
gb_metrics <- metrics_all(
  gb_preds,
  truth       = high_conviction,
  estimate    = .pred_class,
  .pred_1,
  event_level = "second"
)

all_metrics <- bind_rows(
  rf_metrics %>% mutate(Model = "Random Forest"),
  dt_metrics %>% mutate(Model = "Decision Tree"),
  gb_metrics %>% mutate(Model = "Gradient Boosting")
)
print(all_metrics)


# Plot with models on x-axis and values shown on bars
all_metrics %>%
  ggplot(aes(x = Model, y = .estimate, fill = .metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  geom_text(
    aes(label = scales::percent(.estimate, accuracy = 0.1)),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3.5
  ) +
  labs(
    title = "Model Performance by Metric",
    x = "Model",
    y = "Score",
    fill = "Metric"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.1)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )


# ==== Confusion Matrix Plots ====
plot_conf_mat_heatmap(rf_preds, "Random Forest Confusion Matrix")
plot_conf_mat_heatmap(dt_preds, "Decision Tree Confusion Matrix")
plot_conf_mat_heatmap(gb_preds, "Gradient Boosting Confusion Matrix")


# ==== Combined ROC Curve Plot with AUC ====
# Model AUCs
rf_auc <- roc_auc(rf_preds, truth = high_conviction, .pred_1, event_level = "second") %>% pull(.estimate)
dt_auc <- roc_auc(dt_preds,  truth = high_conviction, .pred_1, event_level = "second") %>% pull(.estimate)
gb_auc <- roc_auc(gb_preds,  truth = high_conviction, .pred_1, event_level = "second") %>% pull(.estimate)

# Generate ROC curve data with labels
rf_roc <- roc_curve(rf_preds, truth = high_conviction, .pred_1, event_level = "second") %>%
  mutate(Model = glue("Random Forest (AUC = {round(rf_auc, 3)})"))

dt_roc <- roc_curve(dt_preds, truth = high_conviction, .pred_1, event_level = "second") %>%
  mutate(Model = glue("Decision Tree (AUC = {round(dt_auc, 3)})"))

gb_roc <- roc_curve(gb_preds, truth = high_conviction, .pred_1, event_level = "second") %>%
  mutate(Model = glue("Gradient Boosting (AUC = {round(gb_auc, 3)})"))

# Combine all ROC data
combined_roc <- bind_rows(rf_roc, dt_roc, gb_roc)

# Plot combined ROC curves
ggplot(combined_roc, aes(x = 1 - specificity, y = sensitivity, color = Model)) +
  geom_line(size = 1.2) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(
    title = "Combined ROC Curves with AUC",
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)",
    color = "Model"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ==== Feature Importance ====
vip(extract_fit_parsnip(rf_fit), num_features = 10) + 
  ggtitle("Feature Importance - Random Forest")

vip(extract_fit_parsnip(dt_fit), num_features = 10) + 
  ggtitle("Feature Importance - Decision Tree")

vip(extract_fit_parsnip(gb_fit), num_features = 10) + 
  ggtitle("Feature Importance - Gradient Boosting")



# 9. OPTIMISATION

# ==== Define Multi-Metric Set ====
my_metrics <- yardstick::metric_set(
  yardstick::roc_auc,
  yardstick::accuracy,
  yardstick::precision,
  yardstick::recall,
  yardstick::f_meas
)


# ==== Define Tunable Models ====
rf_tune_model <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = tune()
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

dt_tune_model <- decision_tree(
  tree_depth = tune(),
  min_n = tune(),
  cost_complexity = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

# ==== Define Gradient Boosting Model ====
gb_tune_model <- boost_tree(
  trees = tune(),
  learn_rate = tune(),
  tree_depth = tune()
) %>%
  set_engine("xgboost", subsample = 0.8) %>%  # set fixed subsample
  set_mode("classification")


# ==== Define Smarter Grids ====
rf_grid <- grid_regular(
  mtry(range = c(5, 10)),
  min_n(range = c(5, 15)),
  trees(range = c(200, 400)),
  levels = 3
)

dt_grid <- grid_regular(
  tree_depth(range = c(5, 20)),
  min_n(range = c(5, 15)),
  cost_complexity(range = c(-4, -1), trans = log10_trans()),
  levels = 3
)

gb_grid <- grid_regular(
  trees(range = c(100, 300)),
  learn_rate(range = c(0.01, 0.2)),
  tree_depth(range = c(3, 6)),
  levels = 3
)


# ==== Define Workflows ====

rf_tune_wf <- workflow() %>%
  add_model(rf_tune_model) %>%
  add_recipe(cps_recipe)

dt_tune_wf <- workflow() %>%
  add_model(dt_tune_model) %>%
  add_recipe(cps_recipe)

gb_tune_wf <- workflow() %>%
  add_model(gb_tune_model) %>%
  add_recipe(cps_recipe)

# ==== Run Grid Search ====
# ==== Cross-Validation Folds ====
set.seed(42)
cv_folds <- vfold_cv(train_data, v = 5, strata = high_conviction)

# ==== Set Control ====
control <- control_grid(save_pred = TRUE, verbose = TRUE)


# Tune all
rf_tuned <- tune_grid(rf_tune_wf, resamples = cv_folds, grid = rf_grid, metrics = my_metrics, control = control)
dt_tuned <- tune_grid(dt_tune_wf, resamples = cv_folds, grid = dt_grid, metrics = my_metrics, control = control)
gb_tuned <- tune_grid(gb_tune_wf, resamples = cv_folds, grid = gb_grid, metrics = my_metrics, control = control)


# ==== Select Best Parameters and Final Fit ====

rf_best <- select_best(rf_tuned, metric = "accuracy")
dt_best <- select_best(dt_tuned, metric = "accuracy")
gb_best <- select_best(gb_tuned, metric = "accuracy")


rf_final <- finalize_workflow(rf_tune_wf, rf_best) %>% fit(data = train_data)
dt_final <- finalize_workflow(dt_tune_wf, dt_best) %>% fit(data = train_data)
gb_final <- finalize_workflow(gb_tune_wf, gb_best) %>% fit(data = train_data)

# ==== Evaluate Optimized Models ====

rf_opt_preds <- get_preds(rf_final, test_data)
dt_opt_preds <- get_preds(dt_final, test_data)
gb_opt_preds <- get_preds(gb_final, test_data)

rf_opt_metrics <- metrics_all(rf_opt_preds, truth = high_conviction, estimate = .pred_class, .pred_1, event_level = "second")
dt_opt_metrics <- metrics_all(dt_opt_preds, truth = high_conviction, estimate = .pred_class, .pred_1, event_level = "second")
gb_opt_metrics <- metrics_all(gb_opt_preds, truth = high_conviction, estimate = .pred_class, .pred_1, event_level = "second")

# ==== Summary Table of Best Parameters and AUC ====

summary_df <- tibble::tibble(
  Model = c("Random Forest", "Decision Tree", "Gradient Boosting"),
  `Best Score (ROC-AUC)` = c(
    rf_best$.metric == "roc_auc" ~ rf_best$.estimate,
    dt_best$.metric == "roc_auc" ~ dt_best$.estimate,
    gb_best$.metric == "roc_auc" ~ gb_best$.estimate
  ),
  `Best Parameters` = c(
    paste(names(rf_best)[-1], rf_best[-1], collapse = ", "),
    paste(names(dt_best)[-1], dt_best[-1], collapse = ", "),
    paste(names(gb_best)[-1], gb_best[-1], collapse = ", ")
  )
)

kable(summary_df, align = "c", caption = "Grid Search Results Summary")




# ==== Compare Baseline vs Optimized Metrics ====
compare_df <- bind_rows(
  all_metrics %>% mutate(Type = "Baseline"),
  rf_opt_metrics %>% mutate(Model = "Random Forest", Type = "Optimized"),
  dt_opt_metrics %>% mutate(Model = "Decision Tree", Type = "Optimized"),
  gb_opt_metrics %>% mutate(Model = "Gradient Boosting", Type = "Optimized")
)

# Show side-by-side for each model
compare_df %>%
  pivot_wider(names_from = Type, values_from = .estimate) %>%
  filter(.metric == "roc_auc") %>%
  arrange(desc(Optimized)) %>%
  kable(digits = 4, caption = "ROC-AUC: Baseline vs Optimized")


# ==== Optimized Confusion Matrices ====
plot_conf_mat_heatmap(rf_opt_preds, "Optimized Random Forest Confusion Matrix")
plot_conf_mat_heatmap(dt_opt_preds, "Optimized Decision Tree Confusion Matrix")
plot_conf_mat_heatmap(gb_opt_preds, "Optimized Gradient Boosting Confusion Matrix")


# ==== Optimized ROC Curve Plot ====

rf_opt_auc <- roc_auc(rf_opt_preds, truth = high_conviction, .pred_1, event_level = "second") %>% pull(.estimate)
dt_opt_auc <- roc_auc(dt_opt_preds, truth = high_conviction, .pred_1, event_level = "second") %>% pull(.estimate)
gb_opt_auc <- roc_auc(gb_opt_preds, truth = high_conviction, .pred_1, event_level = "second") %>% pull(.estimate)

rf_opt_roc <- roc_curve(rf_opt_preds, truth = high_conviction, .pred_1, event_level = "second") %>%
  mutate(Model = glue("Optimized RF (AUC = {round(rf_opt_auc, 3)})"))
dt_opt_roc <- roc_curve(dt_opt_preds, truth = high_conviction, .pred_1, event_level = "second") %>%
  mutate(Model = glue("Optimized DT (AUC = {round(dt_opt_auc, 3)})"))
gb_opt_roc <- roc_curve(gb_opt_preds, truth = high_conviction, .pred_1, event_level = "second") %>%
  mutate(Model = glue("Optimized GB (AUC = {round(gb_opt_auc, 3)})"))

optimized_combined_roc <- bind_rows(rf_opt_roc, dt_opt_roc, gb_opt_roc)

# Plot
ggplot(optimized_combined_roc, aes(x = 1 - specificity, y = sensitivity, color = Model)) +
  geom_line(size = 1.2) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(
    title = "Optimized Models - ROC Curves with AUC",
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)",
    color = "Model"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



get_classification_summary <- function(preds, label) {
  preds <- preds %>%
    mutate(
      high_conviction = factor(as.character(high_conviction), levels = c("0", "1")),
      .pred_class = factor(as.character(.pred_class), levels = c("0", "1"))
    )
  
  support <- conf_mat(preds, truth = high_conviction, estimate = .pred_class) %>%
    pluck("table") %>%
    as_tibble() %>%
    filter(Prediction == "1") %>%
    summarise(Support = sum(n)) %>%
    pull(Support)
  
  acc <- yardstick::accuracy(preds, truth = high_conviction, estimate = .pred_class)$.estimate
  prec <- yardstick::precision(preds, truth = high_conviction, estimate = .pred_class, event_level = "second")$.estimate
  rec <- yardstick::recall(preds, truth = high_conviction, estimate = .pred_class, event_level = "second")$.estimate
  f1 <- yardstick::f_meas(preds, truth = high_conviction, estimate = .pred_class, event_level = "second")$.estimate
  
  tibble(
    Model = label,
    Accuracy = round(acc, 4),
    Precision = round(prec, 4),
    Recall = round(rec, 4),
    `F1-Score` = round(f1, 4),
    Support = support
  )
}



rf_optimized_report <- get_classification_summary(rf_opt_preds, "Random Forest - Optimized")
dt_optimized_report <- get_classification_summary(dt_opt_preds, "Decision Tree - Optimized")
gb_optimized_report <- get_classification_summary(gb_opt_preds, "Gradient Boosting - Optimized")

optimized_summary <- bind_rows(
  rf_optimized_report,
  dt_optimized_report,
  gb_optimized_report
)

kable(optimized_summary, align = "c", caption = "Classification Summary: Optimized Models Only")
