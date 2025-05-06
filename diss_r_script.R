#1 #####
setwd("/Users/tescocharlie/Desktop/Dissertation")

# === Log File Setup ===
# Define log file
log_file <- "diss_log.txt"
log_con <- file(log_file, open = "a")  # append mode

# Log script source (optional)
script_file <- "diss_r_script.R"
if (file.exists(script_file)) {
  cat("===== R Script Source Code =====\n", file = log_file)
  cat(readLines(script_file), sep = "\n", file = log_file, append = TRUE)
} else {
  cat("SCRIPT FILE NOT FOUND:", script_file, "\n", file = log_file, append = TRUE)
}

# Start logging
sink(log_con, split = TRUE)                # Console output, also goes to file
sink(log_con, type = "message")            # Messages go only to file (no split)

cat("\n===== Script started at", format(Sys.time()), "=====\n")


install.packages(c("tidyverse", "zoo", "forecast", "vars", "tseries", "readxl", "psych", "lubridate", "dynlm", "sandwich", "lmtest", "ARDL", "urca", "car", "scales", "grid", "strucchange", "AICcmodavg", "broom", "stargazer", "sjPlot", "knitr"))
# Load libraries
library(tidyverse)
library(zoo)
library(forecast)
library(vars)
library(tseries)
library(readxl)
library(psych)
library(lubridate)
library(dynlm)
library(sandwich)
library(lmtest)
library(ARDL)
library(urca)
library(car)
library(scales)
library(grid)
library(strucchange)
library(broom)
library(stargazer)
library(sjPlot)
library(knitr)

#2 #####
rus_gdp <- read_excel("GDP Data.xlsx")
rus_gdp_simple <- read_excel("GDP Simplified.xlsx")
##used excel to clean data


#3 #####
gsdb <- read.csv("GSDB_V4.csv")

#filter for Russia sanctions
rus_sanctions <- gsdb %>%
  filter(sanctioned_state == "Russia")

#drop unwanted years
rus_sanctions <- rus_sanctions %>%
  filter(!(begin %in% c(1993)))

#drop irrelevant columns
cols_to_drop <- c("case_id", "sanctioned_state", "target_mult", "success")  # Drop multi targets & success
rus_sanctions <- rus_sanctions %>%
  dplyr::select(-all_of(cols_to_drop))

# Reorder descr_trade trade columns
rus_sanctions <- rus_sanctions %>%
  relocate(descr_trade, .before = trade)


# Define custom weights for sanction categories
sanction_weights <- c(
  financial = 3,
  military = 2,
  trade    = 3,
  arms     = 2,
  travel   = 1,
  other    = 1
)

# Compute Sanctions Intensity Index
rus_sanctions$sanction_intensity <- with(rus_sanctions,
                                            financial * sanction_weights["financial"] +
                                              military  * sanction_weights["military"] +
                                              trade     * sanction_weights["trade"] +
                                              arms      * sanction_weights["arms"] +
                                              travel    * sanction_weights["travel"] +
                                              other    * sanction_weights["other"]
)


# Reorder descr_trade trade columns
rus_sanctions <- rus_sanctions %>%
  relocate(sanction_intensity, .before = sender_mult)


# Classify sanctions purely by year
rus_sanctions <- rus_sanctions %>%
  mutate(
    sanction_category = case_when(
      begin == 2014 ~ "crimea",
      begin >= 2022 ~ "invasion",
      begin %in% c(2008, 2017, 2020, 2021) ~ "other",
      TRUE ~ "other"
    )
  )

rus_sanctions <- rus_sanctions %>%
  mutate(
    is_crimea = ifelse(sanction_category == "crimea", 1, 0),
    is_invasion = ifelse(sanction_category == "invasion", 1, 0),
    is_other = ifelse(sanction_category == "other", 1, 0)
  )

# Create a function to convert year to quarter (default Q1)
year_to_quarter <- function(year, quarter = "Q1") {
  paste(year, quarter)
}

# Add start quarters based on your information
rus_sanctions_quarter <- rus_sanctions %>%
  mutate(
    start_quarter = case_when(
      sanctioning_state == "Georgia" ~ "2008 Q3",
      sanctioning_state == "Australia" & begin == 2014 ~ "2014 Q1",
      sanctioning_state == "Australia" & begin == 2022 ~ "2022 Q1",
      sanctioning_state == "Canada" & begin == 2014 ~ "2014 Q1",
      sanctioning_state == "Canada" & begin == 2022 ~ "2022 Q1",
      sanctioning_state == "EU" ~ "2014 Q1",
      sanctioning_state == "EU, Montenegro, Iceland, Albania, Liechtenstein, Norway, Ukraine" ~ "2014 Q1",
      sanctioning_state == "Japan" & begin == 2014 ~ "2014 Q1",
      sanctioning_state == "Japan" & begin == 2022 ~ "2022 Q1",
      sanctioning_state == "New Zealand" & begin == 2014 ~ "2014 Q1",
      sanctioning_state == "New Zealand" & begin == 2022 ~ "2022 Q1",
      sanctioning_state == "Switzerland" & begin == 2014 ~ "2014 Q1",
      sanctioning_state == "Switzerland" & begin == 2022 ~ "2022 Q1",
      sanctioning_state == "United Kingdom" & begin == 2014 ~ "2014 Q1",
      sanctioning_state == "United Kingdom" & begin == 2021 ~ "2021 Q2",
      sanctioning_state == "United Kingdom" & begin == 2022 ~ "2022 Q1",
      sanctioning_state == "United States" & begin == 2014 ~ "2014 Q1",
      sanctioning_state == "United States" & begin == 2017 ~ "2017 Q3",
      sanctioning_state == "United States" & begin == 2020 ~ "2020 Q3",
      sanctioning_state == "United States" & begin == 2022 ~ "2022 Q1",
      sanctioning_state == "EU, Macedonia, Albania, Kosovo" ~ "2022 Q1",
      sanctioning_state == "G7, EU" ~ "2022 Q1",
      sanctioning_state == "Germany" & begin == 2022 ~ "2022 Q1",
      sanctioning_state == "Iceland" ~ "2022 Q1",
      sanctioning_state == "Korea, South" ~ "2022 Q1",
      sanctioning_state == "Liechtenstein" ~ "2022 Q1",
      sanctioning_state == "Monaco" ~ "2022 Q1",
      sanctioning_state == "Norway" ~ "2022 Q1",
      sanctioning_state == "Poland" ~ "2022 Q1",
      sanctioning_state == "Singapore" ~ "2022 Q1",
      sanctioning_state == "Taiwan" ~ "2022 Q1",
      sanctioning_state == "United States, EU, United Kingdom, Canada, France, Germany, Italy, Japan" ~ "2022 Q1",
      sanctioning_state == "United States, United Kingdom, Canada" ~ "2022 Q1",
      sanctioning_state == "United States, United Kingdom, Japan, Canada" ~ "2022 Q1",
      sanctioning_state == "Czech Republic" ~ "2023 Q1",
      sanctioning_state == "Kazakhstan" ~ "2023 Q1",
      sanctioning_state == "Latvia, Lithuania, Estonia" ~ "2023 Q1",
      sanctioning_state == "Ukraine" & begin == 2023 ~ "2023 Q1",
      sanctioning_state == "United States, United Kingdom" ~ "2023 Q1",
      TRUE ~ paste0(begin, " Q1") # Default to Q1 if no specific info
    ),
    
    # Add end quarters (most are ongoing)
    end_quarter = case_when(
      sanctioning_state == "Georgia" ~ "2011 Q2", # Only Georgia has ended
      end == 2023 ~ "2023 Q4", # Assuming current sanctions end at 2023 Q4
      TRUE ~ NA_character_ # Ongoing sanctions have no end quarter
    )
  )

# Create a binary indicator for each quarter (1998 Q1 - 2024 Q4)
quarters <- expand.grid(
  year = 1998:2024,
  quarter = paste0("Q", 1:4)
) %>%
  mutate(
    quarter_label = paste(year, quarter),
    date = as.Date(paste(
      year, 
      case_when(
        quarter == "Q1" ~ "01-01",
        quarter == "Q2" ~ "04-01",
        quarter == "Q3" ~ "07-01",
        quarter == "Q4" ~ "10-01"
      ), 
      sep = "-"
    ), format = "%Y-%m-%d")
  ) %>%
  arrange(date)
    
# Function to check if a sanction was active in a given quarter
    is_active <- function(start_q, end_q, current_q, quarters_df) {
      start_idx <- which(quarters_df$quarter_label == start_q)
      current_idx <- which(quarters_df$quarter_label == current_q)
      
      if (is.na(end_q)) {
        # Ongoing sanction - active from start quarter onward
        current_idx >= start_idx
      } else {
        end_idx <- which(quarters_df$quarter_label == end_q)
        current_idx >= start_idx & current_idx <= end_idx
      }
    }
    
# Create a panel dataset with binary indicators
    rus_sanctions_quarter <- rus_sanctions_quarter %>%
      rowwise() %>%
      do({
        tibble(
          sanction_id = .$sanctioning_state,
          start_quarter = .$start_quarter,
          end_quarter = .$end_quarter,
          quarter = quarters$quarter_label,
          date = quarters$date,
          active = mapply(is_active, 
                          .$start_quarter, 
                          .$end_quarter, 
                          quarters$quarter_label, 
                          MoreArgs = list(quarters_df = quarters)),
          # Carry forward all other sanction attributes
          descr_trade = .$descr_trade,
          trade = .$trade,
          arms = .$arms,
          military = .$military,
          financial = .$financial,
          travel = .$travel,
          other = .$other,
          sanction_intensity = .$sanction_intensity,
          sender_mult = .$sender_mult,
          objective = .$objective,
          sanction_category = .$sanction_category,
          is_crimea = .$is_crimea,
          is_invasion = .$is_invasion,
          is_other = .$is_other
        )
      }) %>%
      ungroup()
    

colnames(rus_sanctions_quarter)[colnames(rus_sanctions_quarter) == "quarter"] <- "year"

    

#only active
rus_sanctions_quarter <- rus_sanctions_quarter %>%
      filter(active == TRUE)

#drop irrelevant columns
  cols_to_drop <- c("date", "active")  
  rus_sanctions_quarter <- rus_sanctions_quarter %>%
  dplyr::select(-all_of(cols_to_drop))


# Reorder descr_trade trade columns
rus_sanctions_quarter <- rus_sanctions_quarter %>%
  relocate(year, .before = start_quarter)


# Add weighted scores for each sanction type and total sanction intensity
rus_sanctions_quarter <- rus_sanctions_quarter %>%
  mutate(
    financial_weighted = financial * sanction_weights["financial"],
    military_weighted  = military  * sanction_weights["military"],
    trade_weighted     = trade     * sanction_weights["trade"],
    arms_weighted      = arms      * sanction_weights["arms"],
    travel_weighted    = travel    * sanction_weights["travel"],
    other_weighted     = other     * sanction_weights["other"],
    
    sanction_intensity = financial_weighted + military_weighted +
      trade_weighted + arms_weighted +
      travel_weighted + other_weighted
  )


# Summarise by year and sanction category
rus_agg_sanctions <- rus_sanctions_quarter %>%
  group_by(year, sanction_category) %>%
  summarise(total_intensity = sum(sanction_intensity, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = sanction_category,
    values_from = total_intensity,
    names_prefix = "",
    values_fill = 0
  ) %>%
  rename(
    year = year,
    total_crimea_intensity = crimea,
    total_invasion_intensity = invasion,
    total_other_intensity = other
  ) %>%
  mutate(
    total_intensity_score = total_crimea_intensity + total_invasion_intensity + total_other_intensity
  ) %>%
  arrange(year)


write.csv(rus_agg_sanctions, file = "russia_sanctions.csv", row.names = FALSE)






#4 #####
crude_oil_full <- read_excel("crude oil.xlsx")
# Create a new column for quarters
crude_oil_full$year <- paste0(year(crude_oil_full$Date), " Q", quarter(crude_oil_full$Date))

# Ensure Date column is in Date format
crude_oil_full$Date <- as.Date(crude_oil_full$Date)

# Create year_Quarter column
crude_oil <- crude_oil_full %>%
  mutate(year = paste0(year(Date), " Q", quarter(Date)))

# Group by year_Quarter and calculate averages for both prices
crude_oil <- crude_oil_full %>%
  group_by(year) %>%
  summarise(
    WTI_price = mean(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`, na.rm = TRUE),
    Brent_price = mean(`Europe Brent Spot Price FOB (Dollars per Barrel)`, na.rm = TRUE)
  )


#5 #####
real_gdp_growth <- read_excel("real gdp growth.xlsx")

# Convert first column to numeric safely
real_gdp_growth[[1]] <- as.numeric(real_gdp_growth[[1]])

# Drop rows with NA years (usually header rows or footnotes)
real_gdp_growth <- real_gdp_growth[!is.na(real_gdp_growth[[1]]), ]

# Create quarterly time points
year_quarters <- seq(min(real_gdp_growth[[1]]), max(real_gdp_growth[[1]]), by = 0.25)

# Function to convert annual % growth to index (base 100)
growth_to_index <- function(growth) {
  index <- numeric(length(growth))
  index[1] <- 100
  for (i in 2:length(growth)) {
    index[i] <- index[i - 1] * (1 + growth[i - 1] / 100)
  }
  return(index)
}

# Apply to all regions (skip year column)
index_data <- as.data.frame(
  lapply(real_gdp_growth[-1], growth_to_index)
)

# Interpolate index to quarterly using spline
interpolated_index <- as.data.frame(
  lapply(index_data, function(col) spline(real_gdp_growth[[1]], col, xout = year_quarters, method = "natural")$y)
)

# Convert to quarterly % changes
quarterly_growth <- as.data.frame(
  lapply(interpolated_index, function(x) c(NA, diff(x) / head(x, -1) * 100))
)

# Create formatted year-quarter labels
formatted_years <- paste0(
  floor(year_quarters), " Q", round((year_quarters - floor(year_quarters)) * 4 + 1)
)

# Combine final output
real_gdp_growth <- cbind(year = formatted_years, quarterly_growth)


real_gdp_growth <- real_gdp_growth %>%
  dplyr::filter(!(year %in% c("1997 Q1", "1997 Q2", "1997 Q3", "1997 Q4")))



#6 #####
# Read the Excel file
raw_exp <- read_excel("Russia Expenditure.xlsx")

# Convert the first column (year) to numeric safely
raw_exp[[1]] <- suppressWarnings(as.numeric(raw_exp[[1]]))

# Drop rows with NA in the year column
rus_exp <- raw_exp[!is.na(raw_exp[[1]]), ]

# Store the year values
year_col <- rus_exp[[1]]

# Generate quarterly points (e.g., 1997.00, 1997.25, ..., 2002.75)
year_quarters <- seq(min(year_col), max(year_col), by = 0.25)

# Function: annual % → index (base 100)
growth_to_index <- function(growth) {
  index <- numeric(length(growth))
  index[1] <- 100
  for (i in 2:length(growth)) {
    index[i] <- index[i - 1] * (1 + growth[i - 1] / 100)
  }
  return(index)
}

# Apply to all % growth columns (excluding year)
index_data <- as.data.frame(
  lapply(rus_exp[-1], growth_to_index)
)

# Interpolate index quarterly using spline
interpolated_index <- as.data.frame(
  lapply(index_data, function(col) spline(year_col, col, xout = year_quarters, method = "natural")$y)
)

# Convert interpolated index → quarterly % change
quarterly_growth <- as.data.frame(
  lapply(interpolated_index, function(x) c(NA, diff(x) / head(x, -1) * 100))
)

# Format the year column as "YYYY QX"
formatted_years <- paste0(
  floor(year_quarters), " Q", round((year_quarters - floor(year_quarters)) * 4 + 1)
)

# Combine into final dataset (same object)
rus_exp <- cbind(year = formatted_years, quarterly_growth)

rus_exp <- rus_exp %>%
  dplyr::filter(!(year %in% c("1997 Q1", "1997 Q2", "1997 Q3", "1997 Q4")))





























#7 #####
#merging the dataset
# Start by merging the dependent variable and main independent variable
master_ds <- merge(rus_gdp_simple, rus_agg_sanctions, by = "year", all = TRUE)

# Merge in control variables one at a time
master_ds <- merge(master_ds, crude_oil, by = "year", all = TRUE)
master_ds <- merge(master_ds, rus_exp, by = "year", all = TRUE)
master_ds <- merge(master_ds, real_gdp_growth, by = "year", all = TRUE)

master_ds$log_gdp <- log(master_ds$`Real GDP (Billion USD)`)
master_ds$log_WTI <- log(master_ds$WTI_price)
master_ds$log_Brent <- log(master_ds$Brent_price)
# Log-transform total intensity score (+1 to avoid log(0))
master_ds$log_TIS <- log(master_ds$total_intensity_score + 1)
master_ds <- master_ds %>% 
  rename(gdp_exp = Government.expenditure....of.GDP.)



#8 #####
summary(master_ds)
describe(master_ds)

summary(rus_gdp_simple)
describe(rus_gdp_simple)

summary(rus_sanctions)
describe(rus_sanctions)

summary(rus_agg_sanctions)
describe(rus_agg_sanctions)

summary(crude_oil)
describe(crude_oil)

summary(rus_agg_sanctions)
describe(rus_agg_sanctions)

summary(rus_exp)
describe(rus_exp)

summary(real_gdp_growth)
describe(real_gdp_growth)


#9 #####

# Filter out invalid year formats
rus_gdp_simple <- rus_gdp_simple[grepl("^[0-9]{4} Q[1-4]$", rus_gdp_simple$year), ]
rus_agg_sanctions <- rus_agg_sanctions[grepl("^[0-9]{4} Q[1-4]$", rus_agg_sanctions$year), ]

# Create year_date column with robust quarter-to-date mapping
rus_gdp_simple$year_date <- as.Date(
  sprintf(
    "%s-%02d-01",
    sub(" Q.*", "", rus_gdp_simple$year),
    as.numeric(sub(".*Q", "", rus_gdp_simple$year)) * 3 - 2
  ),
  format = "%Y-%m-%d"
)

rus_agg_sanctions$year_date <- as.Date(
  sprintf(
    "%s-%02d-01",
    sub(" Q.*", "", rus_agg_sanctions$year),
    as.numeric(sub(".*Q", "", rus_agg_sanctions$year)) * 3 - 2
  ),
  format = "%Y-%m-%d"
)

# Check for NA in year_date
if (any(is.na(rus_gdp_simple$year_date))) {
  warning("NA values found in rus_gdp_simple$year_date")
  print(rus_gdp_simple$year[is.na(rus_gdp_simple$year_date)])
}
if (any(is.na(rus_agg_sanctions$year_date))) {
  warning("NA values found in rus_agg_sanctions$year_date")
  print(rus_agg_sanctions$year[is.na(rus_agg_sanctions$year_date)])
}

# Create plotting datasets with date-based filtering (start at 2008 Q3)
rus_gdp_simple_plot <- rus_gdp_simple[rus_gdp_simple$year_date >= as.Date("1998-01-01"), ]
rus_agg_sanctions_plot <- rus_agg_sanctions[rus_agg_sanctions$year_date >= as.Date("2008-07-01"), ]

# Remove any rows with NA in year_date or y-variable
rus_gdp_simple_plot <- rus_gdp_simple_plot[!is.na(rus_gdp_simple_plot$year_date) & 
                                             !is.na(rus_gdp_simple_plot$`Real GDP (Billion USD)`), ]
rus_agg_sanctions_plot <- rus_agg_sanctions_plot[!is.na(rus_agg_sanctions_plot$year_date) & 
                                                   !is.na(rus_agg_sanctions_plot$total_intensity_score), ]


# Add log-transformed GDP variable
master_ds$log_gdp <- log(master_ds$`Real GDP (Billion USD)`)

summary(master_ds$log_gdp)
hist(master_ds$log_gdp, main = "Log of Russia Real GDP", xlab = "log(GDP)", col = "steelblue")



#10 #####
# Extract Year and Quarter
rus_gdp_simple_plot <- rus_gdp_simple_plot %>%
  mutate(
    Quarter = paste0("Q", quarter(year_date)),
    Year = year(year_date)
  )

# Subset for ticks every quarter
quarter_ticks <- rus_gdp_simple_plot %>%
  filter(Quarter %in% c("Q1", "Q2", "Q3", "Q4"))

# Choose x-positions for year labels (centered between Q2 and Q3)
year_labels_df <- rus_gdp_simple_plot %>%
  group_by(Year) %>%
  slice(2) %>%
  ungroup()

# Get y-range for positioning
y_min <- min(rus_gdp_simple_plot$`Real GDP (Billion USD)`, na.rm = TRUE)
y_max <- max(rus_gdp_simple_plot$`Real GDP (Billion USD)`, na.rm = TRUE)
y_range <- y_max - y_min

# Plot
ggplot(rus_gdp_simple_plot, aes(x = year_date, y = `Real GDP (Billion USD)`)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  
  # Custom x-axis: Quarters only
  scale_x_date(
    breaks = quarter_ticks$year_date,
    labels = quarter_ticks$Quarter,
    expand = c(0.01, 0.01)
  ) +
  
  # Ensure y-axis has enough space
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  
  labs(x = "Year", y = "Real GDP (Billion USD)") +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 7,),  # Ensure quarters are visible
    axis.ticks.x = element_line(color = "black"),  # Add ticks for clarity
    plot.margin = margin(t = 10, r = 10, b = 100, l = 10),  # Extra bottom margin
    panel.grid.minor = element_blank()
  ) +
  
  # Add year labels below quarters
  annotate(
    "text",
    x = year_labels_df$year_date,
    y = y_min - (0.1 * y_range),  # 10% of y-range below y_min
    label = year_labels_df$Year,
    size = 3,
    vjust = 1.5,  # Push downward
    color = "black"
  )




#11 #####
ggplot(master_ds, aes(x = total_intensity_score)) +
  geom_density(fill = "tomato", alpha = 0.6) +
  labs(x = "Intensity Score", y = "Density") +
  theme_minimal()


#12 #####

# Verify rus_sanctions exists
if (!exists("rus_sanctions")) stop("rus_sanctions dataset not found")
cat("Rows in rus_sanctions:", nrow(rus_sanctions), "\n")

# Check unique values in sanction type columns to confirm they are binary
sapply(c("trade", "financial", "military", "arms", "travel"), function(col) {
  unique_vals <- unique(rus_sanctions[[col]])
  cat("Unique values in", col, ":", unique_vals, "\n")
})

# Summarize counts of sanction types for each category (exclude Other sanction type)
sanction_types_by_category <- rus_sanctions %>%
  summarise(
    # Crimea: count non-zero sanctions where is_crimea = 1
    Crimea_Trade = sum(trade[is_crimea == 1] > 0, na.rm = TRUE),
    Crimea_Financial = sum(financial[is_crimea == 1] > 0, na.rm = TRUE),
    Crimea_Military = sum(military[is_crimea == 1] > 0, na.rm = TRUE),
    Crimea_Arms = sum(arms[is_crimea == 1] > 0, na.rm = TRUE),
    Crimea_Travel = sum(travel[is_crimea == 1] > 0, na.rm = TRUE),
    # Invasion: count non-zero sanctions where is_invasion = 1
    Invasion_Trade = sum(trade[is_invasion == 1] > 0, na.rm = TRUE),
    Invasion_Financial = sum(financial[is_invasion == 1] > 0, na.rm = TRUE),
    Invasion_Military = sum(military[is_invasion == 1] > 0, na.rm = TRUE),
    Invasion_Arms = sum(arms[is_invasion == 1] > 0, na.rm = TRUE),
    Invasion_Travel = sum(travel[is_invasion == 1] > 0, na.rm = TRUE),
    # Other: count non-zero sanctions where is_other = 1
    Other_Trade = sum(trade[is_other == 1] > 0, na.rm = TRUE),
    Other_Financial = sum(financial[is_other == 1] > 0, na.rm = TRUE),
    Other_Military = sum(military[is_other == 1] > 0, na.rm = TRUE),
    Other_Arms = sum(arms[is_other == 1] > 0, na.rm = TRUE),
    Other_Travel = sum(travel[is_other == 1] > 0, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("category", "sanction_type"),
    names_sep = "_",
    values_to = "count"
  ) %>%
  mutate(
    category = factor(category, levels = c("Crimea", "Invasion", "Other")),
    sanction_type = factor(
      sanction_type,
      levels = c("Trade", "Financial", "Military", "Arms", "Travel")
    )
  )

# Check summarized data
cat("Summarized sanction counts:\n")
print(sanction_types_by_category)
cat("Rows in sanction_types_by_category:", nrow(sanction_types_by_category), "\n")

# Create faceted bar plot
sanction_plot <- ggplot(
  sanction_types_by_category,
  aes(x = sanction_type, y = count, fill = sanction_type)
) +
  geom_bar(stat = "identity") +
  facet_wrap(~ category, ncol = 3, scales = "free_y") +
  labs(
    x = "Sanction Type",
    y = "Count",
    fill = "Sanction Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "top"
  ) +
  scale_fill_brewer(
    palette = "Set2",
    labels = c("Trade", "Financial", "Military", "Arms", "Travel")
  )

# Display plot
print(sanction_plot)

# Save plot to file
ggsave("sanction_distribution_plot.png", plot = sanction_plot, width = 10, height = 6)
cat("Plot saved as sanction_distribution_plot.png\n")




#13 #####
# Create a function to convert year to quarter (default Q1)
year_to_quarter <- function(year, quarter = "Q1") {
  paste(year, quarter)
}

# Add start quarters based on your information
rus_sanctions <- rus_sanctions %>%
  mutate(
    start_quarter = case_when(
      sanctioning_state == "Georgia" ~ "2008 Q3",
      sanctioning_state == "Australia" & begin == 2014 ~ "2014 Q1",
      sanctioning_state == "Australia" & begin == 2022 ~ "2022 Q1",
      sanctioning_state == "Canada" & begin == 2014 ~ "2014 Q1",
      sanctioning_state == "Canada" & begin == 2022 ~ "2022 Q1",
      sanctioning_state == "EU" ~ "2014 Q1",
      sanctioning_state == "EU, Montenegro, Iceland, Albania, Liechtenstein, Norway, Ukraine" ~ "2014 Q1",
      sanctioning_state == "Japan" & begin == 2014 ~ "2014 Q1",
      sanctioning_state == "Japan" & begin == 2022 ~ "2022 Q1",
      sanctioning_state == "New Zealand" & begin == 2014 ~ "2014 Q1",
      sanctioning_state == "New Zealand" & begin == 2022 ~ "2022 Q1",
      sanctioning_state == "Switzerland" & begin == 2014 ~ "2014 Q1",
      sanctioning_state == "Switzerland" & begin == 2022 ~ "2022 Q1",
      sanctioning_state == "United Kingdom" & begin == 2014 ~ "2014 Q1",
      sanctioning_state == "United Kingdom" & begin == 2021 ~ "2021 Q2",
      sanctioning_state == "United Kingdom" & begin == 2022 ~ "2022 Q1",
      sanctioning_state == "United States" & begin == 2014 ~ "2014 Q1",
      sanctioning_state == "United States" & begin == 2017 ~ "2017 Q3",
      sanctioning_state == "United States" & begin == 2020 ~ "2020 Q3",
      sanctioning_state == "United States" & begin == 2022 ~ "2022 Q1",
      sanctioning_state == "EU, Macedonia, Albania, Kosovo" ~ "2022 Q1",
      sanctioning_state == "G7, EU" ~ "2022 Q1",
      sanctioning_state == "Germany" & begin == 2022 ~ "2022 Q1",
      sanctioning_state == "Iceland" ~ "2022 Q1",
      sanctioning_state == "Korea, South" ~ "2022 Q1",
      sanctioning_state == "Liechtenstein" ~ "2022 Q1",
      sanctioning_state == "Monaco" ~ "2022 Q1",
      sanctioning_state == "Norway" ~ "2022 Q1",
      sanctioning_state == "Poland" ~ "2022 Q1",
      sanctioning_state == "Singapore" ~ "2022 Q1",
      sanctioning_state == "Taiwan" ~ "2022 Q1",
      sanctioning_state == "United States, EU, United Kingdom, Canada, France, Germany, Italy, Japan" ~ "2022 Q1",
      sanctioning_state == "United States, United Kingdom, Canada" ~ "2022 Q1",
      sanctioning_state == "United States, United Kingdom, Japan, Canada" ~ "2022 Q1",
      sanctioning_state == "Czech Republic" ~ "2023 Q1",
      sanctioning_state == "Kazakhstan" ~ "2023 Q1",
      sanctioning_state == "Latvia, Lithuania, Estonia" ~ "2023 Q1",
      sanctioning_state == "Ukraine" & begin == 2023 ~ "2023 Q1",
      sanctioning_state == "United States, United Kingdom" ~ "2023 Q1",
      TRUE ~ paste0(begin, " Q1") # Default to Q1 if no specific info
    ),
    
    # Add end quarters (most are ongoing)
    end_quarter = case_when(
      sanctioning_state == "Georgia" ~ "2011 Q2", # Only Georgia has ended
      end == 2023 ~ "2023 Q4", # Assuming current sanctions end at 2023 Q4
      TRUE ~ NA_character_ # Ongoing sanctions have no end quarter
    )
  )

# Create a binary indicator for each quarter (2008 Q1 - 2024 Q4)
quarters <- expand.grid(
  year = 2008:2024,  # Adjusted to 2008 to match the chart
  quarter = paste0("Q", 1:4)
) %>%
  mutate(
    quarter_label = paste(year, quarter),
    date = as.Date(paste(
      year, 
      case_when(
        quarter == "Q1" ~ "01-01",
        quarter == "Q2" ~ "04-01",
        quarter == "Q3" ~ "07-01",
        quarter == "Q4" ~ "10-01"
      ), 
      sep = "-"
    ), format = "%Y-%m-%d")
  ) %>%
  arrange(date)

# Function to check if a sanction was active in a given quarter
is_active <- function(start_q, end_q, current_q, quarters_df) {
  start_idx <- which(quarters_df$quarter_label == start_q)
  current_idx <- which(quarters_df$quarter_label == current_q)
  
  if (is.na(end_q)) {
    # Ongoing sanction - active from start quarter onward
    current_idx >= start_idx
  } else {
    end_idx <- which(quarters_df$quarter_label == end_q)
    current_idx >= start_idx & current_idx <= end_idx
  }
}

# Create a panel dataset with binary indicators
rus_sanctions <- rus_sanctions %>%
  rowwise() %>%
  do({
    tibble(
      sanction_id = .$sanctioning_state,
      start_quarter = .$start_quarter,
      end_quarter = .$end_quarter,
      quarter = quarters$quarter_label,
      date = quarters$date,
      active = mapply(is_active, 
                      .$start_quarter, 
                      .$end_quarter, 
                      quarters$quarter_label, 
                      MoreArgs = list(quarters_df = quarters)),
      # Carry forward all other sanction attributes
      descr_trade = .$descr_trade,
      trade = .$trade,
      arms = .$arms,
      military = .$military,
      financial = .$financial,
      travel = .$travel,
      other = .$other,
      sanction_intensity = .$sanction_intensity,
      sender_mult = .$sender_mult,
      objective = .$objective,
      sanction_category = .$sanction_category,
      is_crimea = .$is_crimea,
      is_invasion = .$is_invasion,
      is_other = .$is_other
    )
  }) %>%
  ungroup()

# Rename quarter to year
colnames(rus_sanctions)[colnames(rus_sanctions) == "quarter"] <- "year"

# Only active sanctions
rus_sanctions <- rus_sanctions %>%
  filter(active == TRUE)

# Drop irrelevant columns
cols_to_drop <- c("date", "active")  
rus_sanctions <- rus_sanctions %>%
  dplyr::select(-all_of(cols_to_drop))

# Reorder columns
rus_sanctions <- rus_sanctions %>%
  relocate(year, .before = start_quarter)

# Summarize to count the number of sanctions per quarter and category
rus_sanctions_count <- rus_sanctions %>%
  group_by(year, sanction_category) %>%
  summarise(number = n(), .groups = "drop") %>%
  # Pivot to have a column for each sanction category
  pivot_wider(
    names_from = sanction_category,
    values_from = number,
    names_prefix = "num_",
    values_fill = 0
  ) %>%
  # Join with quarters to ensure all quarters are included
  right_join(quarters %>% dplyr::select(quarter_label), by = c("year" = "quarter_label")) %>%
  # Fill in missing values with 0
  mutate(
    num_crimea = replace_na(num_crimea, 0),
    num_invasion = replace_na(num_invasion, 0),
    num_other = replace_na(num_other, 0)
  ) %>%
  # Add a total count column
  mutate(
    total_sanctions = num_crimea + num_invasion + num_other
  ) %>%
  arrange(year)  # No need to rename since 'year' is already the column name

# Reshape for plotting
rus_sanctions_count_long <- rus_sanctions_count %>%
  pivot_longer(
    cols = c(num_crimea, num_invasion, num_other),
    names_to = "sanction_category",
    values_to = "number",
    names_prefix = "num_",
    names_transform = list(sanction_category = ~gsub("_intensity", "", .))
  )

# Plot the summarized data
ggplot(rus_sanctions_count_long, aes(x = year, y = number, fill = sanction_category)) +
  geom_col() +
  scale_fill_manual(values = c("crimea" = "maroon", "invasion" = "#4682B4", "other" = "#355E3B"),
                    labels = c("Crimea", "Invasion", "Other")) +
  labs(x = "Quarter", y = "Number of Sanctions", fill = "Sanction Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))









#14 ####
# Convert master_ds to time series object
ts_master <- ts(master_ds, start = c(1998, 1), frequency = 4)

# === ADF Test ===
adf.test(na.omit(master_ds$log_gdp))
adf.test(na.omit(master_ds$log_TIS))
adf.test(na.omit(master_ds$log_Brent))
adf.test(na.omit(master_ds$`gdp_exp`))
adf.test(na.omit(master_ds$World))
#difference the model 
adf.test(diff(na.omit(master_ds$log_gdp)), k = 4)
adf.test(diff(na.omit(master_ds$log_TIS)), k = 4)
adf.test(diff(na.omit(master_ds$log_Brent)), k = 4)


# Collect ADF test statistics
adf_results <- data.frame(
  Variable = c("log_gdp", "log_TIS", "log_Brent", 
               "Gov.Exp", "World",
               "D.log_gdp", "D.log_TIS", "D.log_Brent"),
  Statistic = c(
    adf.test(na.omit(master_ds$log_gdp))$statistic,
    adf.test(na.omit(master_ds$log_TIS))$statistic,
    adf.test(na.omit(master_ds$log_Brent))$statistic,
    adf.test(na.omit(master_ds$`gdp_exp`))$statistic,
    adf.test(na.omit(master_ds$World))$statistic,
    adf.test(diff(na.omit(master_ds$log_gdp)), k = 4)$statistic,
    adf.test(diff(na.omit(master_ds$log_TIS)), k = 4)$statistic,
    adf.test(diff(na.omit(master_ds$log_Brent)), k = 4)$statistic
  ),
  P_value = c(
    adf.test(na.omit(master_ds$log_gdp))$p.value,
    adf.test(na.omit(master_ds$log_TIS))$p.value,
    adf.test(na.omit(master_ds$log_Brent))$p.value,
    adf.test(na.omit(master_ds$`gdp_exp`))$p.value,
    adf.test(na.omit(master_ds$World))$p.value,
    adf.test(diff(na.omit(master_ds$log_gdp)), k = 4)$p.value,
    adf.test(diff(na.omit(master_ds$log_TIS)), k = 4)$p.value,
    adf.test(diff(na.omit(master_ds$log_Brent)), k = 4)$p.value
  )
)

stargazer(adf_results, type = "text", summary = FALSE,
          title = "ADF Unit Root Test Results",
          out = "/Users/tescocharlie/Desktop/Dissertation/adf_test_results.txt")

# Granger causality test (example: sanctions → GDP)
grangertest(log_gdp ~ log_TIS, order = 2, data = master_ds)

stargazer(grangertest(log_gdp ~ log_TIS, order = 2, data = master_ds), type = "text", summary = FALSE,
          title = "Granger Test Results",
          out = "/Users/tescocharlie/Desktop/Dissertation/granger_test_results.txt")


# === ARDL Model ===
model_ardl <- dynlm(
  log_gdp ~ L(log_gdp, 1:2) +            # Added 2 lags for log_gdp
    L(log_TIS, 0:3) +                    # Increased lags for log_TIS to 3
    L(log_Brent, 0:2) +                  # Increased lags for log_Brent to 2
    L(gdp_exp, 0:2) +                    # Increased lags for gdp_exp to 2
    L(World, 0:2),                       # Increased lags for World to 2
  data = ts_master
)

summary(model_ardl)
coeftest(model_ardl, vcov = vcovHAC(model_ardl))
BIC(model_ardl)

stargazer(model_ardl, type = "text", summary = FALSE,
          title = "ARDL Results",
          out = "/Users/tescocharlie/Desktop/Dissertation/ARDL_results.html")

# === Fit models with fewer lags for comparison ===
model_lag1 <- dynlm(
  log_gdp ~ L(log_gdp, 1) +
    L(log_TIS, 0:1) +
    L(log_Brent, 0:1) +
    L(gdp_exp, 0:1) +
    L(World, 0:1),
  data = ts_master
)

model_lag2 <- update(model_lag1, . ~ . + L(log_TIS, 2))

summary(model_lag1)
BIC(model_lag1)
summary(model_lag2)
BIC(model_lag2)




# === Long-run elasticity of sanctions ===
# Extract all sanction coefficients
logtis_coefs <- coef(model_ardl)[grepl("log_TIS", names(coef(model_ardl)))]
# Use the first lag of GDP in the denominator
lagged_gdp_coef <- coef(model_ardl)["L(log_gdp, 1:2)1"]
# Calculate long-run elasticity
long_run_elasticity <- sum(logtis_coefs, na.rm = TRUE) / (1 - lagged_gdp_coef)
# Display result
cat("Long-run elasticity of log_TIS on log_GDP:", round(long_run_elasticity, 4), "\n")


# === Autocorrelation ===
dwtest(model_ardl)

# === Heteroskedasticity ===
bptest(model_ardl)

# === Multicollinearity (VIF) ===
vif_data <- na.omit(master_ds[, c(
  "log_gdp", "log_TIS",
  "log_Brent", "gdp_exp",
  "World"
)])
vif_model <- lm(log_gdp ~ ., data = vif_data)
vif_values <- vif(vif_model)


# Turn VIF values into a data frame
vif_table <- data.frame(
  Variable = names(vif_values),
  VIF = round(vif_values, 2)
)

# Print it nicely
kable(vif_table, caption = "Table: Variance Inflation Factors (VIFs)")



#15 ####
# === Residuals ===
resids <- residuals(model_ardl)
resids_clean <- ts(na.omit(as.numeric(resids)), frequency = 4, start = c(2000, 1))
# Residual plot
plot(resids, main = "Residuals from ARDL Model", ylab = "Residuals", col = "darkred", type = "l")
abline(h = 0, lty = 2)
# Extract and clean residuals from the model
resids <- residuals(model_ardl)
# Drop NAs explicitly
resids_clean <- ts(na.omit(as.numeric(resids)), frequency = 4, start = c(1998 + 2, 1))  # Adjust start if needed
# Plot ACF and PACF
acf(resids_clean, main = "ACF of ARDL Residuals")
pacf(resids_clean, main = "PACF of ARDL Residuals")

# === Confidence Intervals ===
tidy_model <- tidy(model_ardl, conf.int = TRUE)

ggplot(tidy_model, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  theme_minimal() +
  labs(title = "ARDL Coefficients with 95% Confidence Intervals", x = "Lagged Terms", y = "Estimate")



#16 ####
# === Actual vs Fitted GDP (with Shaded Sanctions) ===
# Time series setup
log_gdp_full <- ts_master[, "log_gdp"]
full_time <- time(log_gdp_full)
# Fitted values and time
fitted_vals <- fitted(model_ardl)
fitted_time <- time(fitted_vals)
# Actual values used in model
actual_fit <- model_ardl$model$log_gdp
# Convert to dates
plot_df <- data.frame(
  time = as.Date(as.yearqtr(full_time)),
  Actual = as.numeric(log_gdp_full)
)

fitted_df <- data.frame(
  time = as.Date(as.yearqtr(fitted_time)),
  Fitted = as.numeric(fitted_vals)
)


# Define sanction regions manually (approximate)
# Define sanction regions manually (with transparency intensity)
sanctions_df <- data.frame(
  category = c("Other", "Other", "Other", "Other", "Crimea", "Invasion"),  # Invasion last
  label = c("Other", "Other", "Other", "Other", "Crimea", "Invasion"),
  start = as.Date(c("2020-07-01", "2008-07-01", "2017-07-01", "2021-01-01", "2014-01-01", "2022-01-01")),
  end   = as.Date(c("2024-12-31", "2011-04-01", "2024-12-31", "2024-12-31", "2024-12-31", "2024-12-31")),
  alpha = c(0.1, 0.1, 0.1, 0.1, 0.2, 0.2)
)

scale_fill_manual(values = c(
  "Crimea" = "firebrick",
  "Invasion" = "steelblue",
  "Other" = "darkgreen"
),
labels = c(
  "Crimea Sanctions",
  "Invasion Sanctions",
  "Other Sanctions"
)
)

# Define date limits
start_date <- as.Date("1998-01-01")
end_date <- as.Date("2024-12-31")
# Filter actual and fitted GDP data to desired range
plot_df <- plot_df %>%
  filter(time >= start_date & time <= end_date)
fitted_df <- fitted_df %>%
  filter(time >= start_date & time <= end_date)
# Filter sanctions shading data
sanctions_df <- sanctions_df %>%
  filter(end >= start_date & start <= end_date)
# Plot
ggplot() +
  # Sanctions shaded areas
  geom_rect(data = sanctions_df,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = category),
            alpha = c(0.1, 0.1, 0.1, 0.1, 0.3, 0.2),
            inherit.aes = FALSE) +
  # Actual and Fitted log GDP lines with mapped colors
  geom_line(data = plot_df, aes(x = time, y = Actual, color = "Actual GDP"), size = 1) +
  geom_line(data = fitted_df, aes(x = time, y = Fitted, color = "Fitted GDP"), size = 1) +
  # Titles and axis labels
  labs(
    title = "Russia: Log Real GDP with Sanctions Shaded",
    subtitle = "Fitted values from ARDL model; Sanctions shaded by type",
    x = "Quarter",
    y = "Log GDP",
    fill = "Sanctions Type",
    color = "GDP Lines"
  ) +
  # Manual fill colors for sanctions
  scale_fill_manual(
    values = c(
      "Crimea" = "firebrick",
      "Invasion" = "darkblue",
      "Other" = "forestgreen"
    ),
    labels = c(
      "Crimea Sanctions",
      "Invasion Sanctions",
      "Other Sanctions"
    )
  ) +
  # Manual line colors for GDP lines
  scale_color_manual(
    values = c(
      "Actual GDP" = "black",
      "Fitted GDP" = "magenta"
    )
  ) +
  # Quarterly x-axis formatting and limiting display range
  scale_x_date(
    limits = c(start_date, end_date),
    date_breaks = "3 months",
    labels = function(x) {
      paste0(format(x, "%Y"), " Q", (as.numeric(format(x, "%m")) - 1) %/% 3 + 1)
    },
    expand = c(0, 0)
  ) +
  # Theme and layout
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5, size = 7),
    axis.ticks.x = element_line(color = "black"),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.8, "lines"),
    legend.spacing.y = unit(0.2, "cm")
  ) +
  
  # Legends for shaded areas and lines
  guides(
    fill = guide_legend(override.aes = list(alpha = 0.3)),
    color = guide_legend(override.aes = list(linetype = c("solid", "solid")))
  )


#17 ####
# === Intensity Shading Plot ===
intensity_df <- data.frame(
  time = as.Date(as.yearqtr(time(ts_master))),
  intensity = as.numeric(ts_master[, "total_intensity_score"])
)
ggplot() +
  # Intensity shading layer
  geom_tile(data = intensity_df,
            aes(x = time, y = 4.5, fill = intensity),
            height = Inf,
            alpha = 0.5) +
  # Actual GDP line
  geom_line(data = plot_df, aes(x = time, y = Actual), color = "black", size = 1) +
  # Fitted GDP line
  geom_line(data = fitted_df, aes(x = time, y = Fitted), color = "magenta", size = 1) +
  # Axis labels and title
  labs(
    title = "Russia: Log Real GDP with Sanction Intensity Shading",
    subtitle = "Higher sanction intensity = deeper shading",
    x = "Quarter",
    y = "Log of Real GDP",
    fill = "Sanction\nIntensity"
  ) +
  
  # Gradient fill for intensity
  scale_fill_gradientn(
    colours = c("#EBD379", "#EB7900", "#EA4600", "#de2d26", "#a50f15"),
    values = rescale(c(1, 65, 88, 221, 238)),
    limits = c(1, 238),
    name = "Sanction\nIntensity",
    na.value = NA
  )+
  # Quarterly x-axis formatting
  scale_x_date(
    limits = c(as.Date("1998-01-01"), as.Date("2024-12-31")),
    date_breaks = "3 months",
    labels = function(x) {
      paste0(format(x, "%Y"), " Q", (as.numeric(format(x, "%m")) - 1) %/% 3 + 1)
    },
    expand = c(0, 0)
  ) +
  # Theme and layout
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5, size = 7),
    axis.ticks.x = element_line(color = "black"),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.8, "lines"),
    legend.spacing.y = unit(0.2, "cm")
  ) +
  # Legends for shaded areas and lines
  guides(
    fill = guide_legend(override.aes = list(alpha = 0.3)),
    color = guide_legend(override.aes = list(linetype = c("solid", "solid")))
  )






#18 ####
# === Robustness Check: VAR ===
# Subset for VAR (remove log_gdp to avoid redundancy)
var_data <- na.omit(master_ds[, c("log_gdp", "log_TIS", "log_Brent", "gdp_exp", "World")])
# Convert to ts object
var_ts <- ts(var_data, start = c(1998, 1), frequency = 4)
# Lag selection
lag_select <- VARselect(var_ts, lag.max = 6, type = "const")
cat("\n--- Optimal Lags by Criteria ---\n")
print(lag_select$selection)
# Fit VAR model
var_model <- VAR(var_ts, p = lag_select$selection[1], type = "const")
summary(var_model)

# Export stargazer output to a text file
stargazer(var_model$varresult,
          type = "html",                       # or use "latex" for .tex file
          out = "/Users/tescocharlie/Desktop/Dissertation/VAR_results.html",
          title = "Vector Autoregression (VAR) Model Results",
          dep.var.labels.include = TRUE)




# === Impulse Response (Sanctions → GDP) ===
irf_res <- irf(var_model, impulse = "log_TIS", response = "log_gdp", n.ahead = 12, boot = TRUE)
# Plot IRF
plot(irf_res, main = "Impulse Response: Sanctions → Log GDP")





#19 ####
# === GDP Forecast from ARDL ===
# Assume forecast_ardl is a numeric vector with your forecasts
forecast_ardl <- predict(model_ardl, n.ahead = 8)

# Create the forecast time series object
forecast_ts <- ts(forecast_ardl, start = c(2023, 1), frequency = 4)

# Create a data frame for plotting
forecast_df <- data.frame(
  Quarter = as.yearqtr(time(forecast_ts)),  # Converts time to year-quarter format
  Forecast = as.numeric(forecast_ts)
)

# Plot forecast
ggplot(forecast_df, aes(x = Quarter, y = Forecast)) +
  geom_line(color = "blue", size = 1.2) +  # Thicker line for clarity
  geom_point(color = "blue", size = 2) +   # Points on forecast
  labs(
    title = "ARDL Forecast: Log Real GDP",
    x = "Quarter",
    y = "Log of Real GDP"
  ) +
  scale_x_yearqtr(
    format = "%Y Q%q",    # Show quarter format
    breaks = forecast_df$Quarter  # Place a label at every forecasted quarter
  ) +
  theme_minimal(base_size = 14) +           # Clean white background
  theme(
    panel.grid.major = element_line(color = "lightgrey"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),  # White background
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)             # Rotate x-axis labels for clarity
  )

#20 ####
# === Close Log File ===
cat("===== Script ended at", format(Sys.time()), "=====\n")
sink(type = "message")  # Close message sink first
sink()                  # Then close normal output sink
close(log_con)

