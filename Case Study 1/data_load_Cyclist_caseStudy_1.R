
# Load necessary library
list.of.packages <- c("data.table", "shiny", "ggplot2", "shinydashboard", "shinyWidgets",
                      "plotly", "DT", "lubridate", "readxl", "stringr", "BBmisc",
                      "scales", "rintrojs", "shinyjs", "shinythemes", "ggalluvial",
                      "packcircles", "RColorBrewer", "shinycssloaders", "colorspace", "plotly",
                      "dplyr", "openxlsx", "tidyr", "shinymanager", "scales", "fst", "flextable", 
                      "officer","magrittr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#######################################################################################################
#######################################################################################################
#######################################################################################################
############################################# Load Data

main_folder <- paste0("bike_share_data")

# Define the full path including the folder and the desired file name
file_path <- file.path(main_folder, "combined_cyclist_data_cleaned.fst")

# Read the dataframe from the fst file located in the specified folder
combined_cyclist_df <- read_fst(file_path)

#######################################################################################################
#######################################################################################################
#######################################################################################################
############################################# Demographics

# Calculate the total number of data points
total_data_points <- nrow(combined_cyclist_df) * ncol(combined_cyclist_df)

# Print the total number of data points
print(total_data_points)

# Calculate ride length in minutes
combined_cyclist_df$ride_length_minutes <- as.numeric(difftime(
  combined_cyclist_df$ended_at, 
  combined_cyclist_df$started_at, 
  units = "mins"
))

# Check for zero or negative ride lengths
zero_length_counts <- combined_cyclist_df %>%
  filter(ride_length_minutes <= 0) %>%
  group_by(member_casual) %>%
  summarise(Count = n())

# Print the counts of zero/negative rides
print("Zero or negative ride lengths:")
print(zero_length_counts)

# Filter out zero or negative ride_lengths
filtered_cyclist_df <- combined_cyclist_df %>%
  filter(ride_length_minutes > 0)

# Proceed with computations using the filtered dataset

########################
# Start Date
sort(filtered_cyclist_df$started_at)[1]
sort(filtered_cyclist_df$ended_at)[1]

# End Date
sort(filtered_cyclist_df$started_at, decreasing = TRUE)[1]
sort(filtered_cyclist_df$ended_at, decreasing = TRUE)[1]

########################
### Reformat Date
original_startDate <- sort(filtered_cyclist_df$started_at)[1]
original_endDate <- sort(filtered_cyclist_df$ended_at, decreasing = TRUE)[1]

# Convert the string to a Date object
original_startDate <- as.Date(original_startDate)
original_endDate <- as.Date(original_endDate)

# Reformat the date
startDate <- format(original_startDate, "%B %d, %Y")
endDate <- format(original_endDate, "%B %d, %Y")

# Print the reformatted date
print(startDate)
print(endDate)

######################### Number of Casual Rides

# Count occurrences of each category using dplyr
ride_type_counts <- filtered_cyclist_df %>%
  group_by(member_casual) %>%
  summarise(Count = n())

# Print the result
print(ride_type_counts)
num_casual_rides <- ride_type_counts$Count[1]
num_membership_rides <- ride_type_counts$Count[2]

# Format numbers with commas
num_casual_rides_table <- comma(num_casual_rides)
num_membership_rides_table <- comma(num_membership_rides)
total_data_points_table <- comma(total_data_points)

################################################ Descriptive Statistics Table

# Get the number of casual and member rides
ride_counts <- filtered_cyclist_df %>%
  group_by(member_casual) %>%
  summarise(Count = n()) %>%
  # summarise(Count = n(), .groups = "drop") %>%
  pull(Count)

demographics_table <- data.frame(
  Metric = c("Start Date", "End Date", "Casual Rides", "Member Rides", "Datapoints"),
  Value = as.character(c(startDate, endDate, num_casual_rides_table, num_membership_rides_table, total_data_points_table))
)


# Print the summary table
# print(demographics_table)

#############################################
############################################# Create the Demographics Flextable

#Customize Flextable
demographics_ft <- flextable(demographics_table) %>%
  theme_box() %>%
  bold(part = "header") %>%
  color(part = "header", color = "white") %>%
  bg(part = "header", bg = "#4472c4") %>% 
  align(align = "center", part = "all") #center cell objects

# Manually specify alternating row colors
# This step is a workaround for the direct approach
row_indices <- seq_len(nrow(demographics_table))
even_rows <- row_indices %% 2 == 0
odd_rows <- !even_rows

# Apply zebra striping
demographics_ft <- demographics_ft %>%
  bg(i = odd_rows, bg = "#e6e6e6", part = "body") %>%
  bg(i = even_rows, bg = "white", part = "body")

# Adjust Width
demographics_ft <- demographics_ft %>%
  width(j = c("Metric", "Value"), width = 1.75) 

############################################# Create the Demographics Pie Plot

# Calculate the percentage of the total for each group
total_rides <- sum(num_casual_rides, num_membership_rides)
# Assuming num_casual_rides and num_membership_rides are already defined
ride_type_counts <- data.frame(
  MemberCasual = c("Casual", "Member"),
  Count = c(num_casual_rides, num_membership_rides)
)


# Calculate the percentage of the total for each group
ride_type_counts$Percentage <- round((ride_type_counts$Count / total_rides) * 100, 1)

# Rides Pie Plot
rides_pie_plot <- ggplot(ride_type_counts, aes(x = "", y = Count, fill = MemberCasual)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +  # This transforms the bar plot into a pie plot
  theme_void() +  # Removes background, gridlines, and text
  labs(fill = "Rider Type") +  # Legend title
  # geom_text(aes(label = paste0(Percentage, "%", " (", Count, ")")), position = position_stack(vjust = 0.5))
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5))

# Display the plot
# print(rides_pie_plot)

#######################################################################################################
#######################################################################################################
#######################################################################################################
############################################# Data Manipulation / Analysis

# Add a new column for the day of the week
filtered_cyclist_df <- filtered_cyclist_df %>%
  mutate(day_of_week = weekdays(started_at))

# Display the structure to confirm the new column has been added
str(filtered_cyclist_df)

##################################### Calculations for Entire Dataset

# Calculate the mean of ride_length
ride_length_minutes_mean <- round(mean(filtered_cyclist_df$ride_length_minutes), 2)

# Calculate the max/min ride_length
ride_length_minutes_max <- round(max(filtered_cyclist_df$ride_length_minutes), 2)
ride_length_minutes_min <- round(min(filtered_cyclist_df$ride_length_minutes), 2)

# Calculate the mode of day_of_week
calculate_mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
day_of_week_mode <- calculate_mode(filtered_cyclist_df$day_of_week)

# Print the mode
print(day_of_week_mode)

##################################### Calculations for Casual vs Members: Mean, Max, Day of Week 
######
# Perform the summarization
casual_member_summary_stats <- filtered_cyclist_df %>%
  group_by(member_casual) %>%
  summarise(
    ride_length_mean = round(mean(ride_length_minutes, na.rm = TRUE)),
    ride_length_max_hours = floor(max(ride_length_minutes, na.rm = TRUE) / 60),
    day_of_week_mode = calculate_mode(day_of_week)
  )

# Modify the column names and capitalize the values in member_casual
names(casual_member_summary_stats)[names(casual_member_summary_stats) == "member_casual"] <- "Rider Type"
casual_member_summary_stats$`Rider Type` <- tools::toTitleCase(casual_member_summary_stats$`Rider Type`)

# Modify the column names to be title case and replace underscores
names(casual_member_summary_stats) <- gsub(
  "_", " ", 
  tools::toTitleCase(names(casual_member_summary_stats))
)

names(casual_member_summary_stats)[names(casual_member_summary_stats) == "Ride length mean"] <- "Ride Length (Mean)"
names(casual_member_summary_stats)[names(casual_member_summary_stats) == "Ride length max hours"] <- "Ride Length (Max Hours)"
names(casual_member_summary_stats)[names(casual_member_summary_stats) == "Day of week mode"] <- "Day of Week (Mode)"

# Print the results for member and casual users
cat("Grouped by Rider Type:\n")
print(casual_member_summary_stats)

#############################################
############################################# Create the Summary Flextable

#Customize Flextable
casual_member_summary_stats_ft <- flextable(casual_member_summary_stats) %>%
  theme_box() %>%
  bold(part = "header") %>%
  color(part = "header", color = "white") %>%
  bg(part = "header", bg = "#4472c4") %>% 
  align(align = "center", part = "all") #center cell objects

# Manually specify alternating row colors
# This step is a workaround for the direct approach
row_indices <- seq_len(nrow(casual_member_summary_stats))
even_rows <- row_indices %% 2 == 0
odd_rows <- !even_rows

# Apply zebra striping
casual_member_summary_stats_ft <- casual_member_summary_stats_ft %>%
  bg(i = odd_rows, bg = "#e6e6e6", part = "body") %>%
  bg(i = even_rows, bg = "white", part = "body")

# Adjust Width
casual_member_summary_stats_ft <- casual_member_summary_stats_ft %>%
  width(j = c("Rider Type"), width = 1) %>%
  width(j = c("Ride Length (Mean)", "Ride Length (Max Hours)", "Day of Week (Mode)"), width = 1.25) 

print(casual_member_summary_stats_ft)

#############################################
############################################# Bin Data: Ride Lengths

# Calculate totals for new bins
total_bins <- filtered_cyclist_df %>%
  summarise(
    total_5s_to_30s = sum(ride_length_minutes > 0.05 & ride_length_minutes <= .5, na.rm = TRUE),
    total_30s_to_2m = sum(ride_length_minutes > 0.5 & ride_length_minutes <= 2, na.rm = TRUE),
    total_2m_to_5m = sum(ride_length_minutes > 2 & ride_length_minutes <= 5, na.rm = TRUE),
    total_5m_to_10m = sum(ride_length_minutes > 5 & ride_length_minutes <= 10, na.rm = TRUE),
    total_10m_to_20m = sum(ride_length_minutes > 10 & ride_length_minutes <= 20, na.rm = TRUE),
    total_20m_to_60m = sum(ride_length_minutes > 20 & ride_length_minutes <= 60, na.rm = TRUE),
    total_60m_to_120m = sum(ride_length_minutes > 60 & ride_length_minutes <= 120, na.rm = TRUE),
    total_over_120m = sum(ride_length_minutes > 120, na.rm = TRUE)
  )

# Calculate count and percentage for each rider type
ride_length_counts <- filtered_cyclist_df %>%
  group_by(member_casual) %>%
  summarise(
    count_5s_to_30s = sum(ride_length_minutes > 0.05 & ride_length_minutes <= .5, na.rm = TRUE),
    count_30s_to_2m = sum(ride_length_minutes > 0.5 & ride_length_minutes <= 2, na.rm = TRUE),
    count_2m_to_5m = sum(ride_length_minutes > 2 & ride_length_minutes <= 5, na.rm = TRUE),
    count_5m_to_10m = sum(ride_length_minutes > 5 & ride_length_minutes <= 10, na.rm = TRUE),
    count_10m_to_20m = sum(ride_length_minutes > 10 & ride_length_minutes <= 20, na.rm = TRUE),
    count_20m_to_60m = sum(ride_length_minutes > 20 & ride_length_minutes <= 60, na.rm = TRUE),
    count_60m_to_120m = sum(ride_length_minutes > 60 & ride_length_minutes <= 120, na.rm = TRUE),
    count_over_120m = sum(ride_length_minutes > 120, na.rm = TRUE)
  )

# Reshape the data to the desired format
reshaped_counts <- ride_length_counts %>%
  pivot_longer(
    cols = -member_casual,
    names_to = "time_bin",
    values_to = "value"
  ) %>%
  separate(time_bin, into = c("metric", "interval"), sep = "_", extra = "merge") %>%
  pivot_wider(
    names_from = c(member_casual, metric),
    values_from = value
  )

# Clean the 'interval' column by replacing with formatted labels
reshaped_counts <- reshaped_counts %>%
  mutate(
    interval = gsub("_", " ", gsub("_to_", " - ", tools::toTitleCase(interval)))
  ) %>%
  select(interval, casual_count, member_count)

# Clean column names and capitalize
names(reshaped_counts) <- gsub("_", " ", names(reshaped_counts))
names(reshaped_counts) <- tools::toTitleCase(names(reshaped_counts))

#############################################
############################################# Create the Counts Flextable

#Customize Flextable
reshaped_counts_ft <- flextable(reshaped_counts) %>%
  theme_box() %>%
  bold(part = "header") %>%
  color(part = "header", color = "white") %>%
  bg(part = "header", bg = "#4472c4") %>% 
  align(align = "center", part = "all") #center cell objects

# Manually specify alternating row colors
# This step is a workaround for the direct approach
row_indices <- seq_len(nrow(reshaped_counts))
even_rows <- row_indices %% 2 == 0
odd_rows <- !even_rows

# Apply zebra striping
reshaped_counts_ft <- reshaped_counts_ft %>%
  bg(i = odd_rows, bg = "#e6e6e6", part = "body") %>%
  bg(i = even_rows, bg = "white", part = "body")

# Adjust Width
reshaped_counts_ft <- reshaped_counts_ft %>%
  width(j = c("Interval"), width = 1) %>%
  width(j = c("Casual Count", "Member Count"), width = 1.25) 

#############################################
############################################# ### Visualize Day of week

# Calculate the day of the week from the start date
filtered_cyclist_df$day_of_week <- weekdays(as.Date(filtered_cyclist_df$started_at))

# Correct the order of the days
day_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Convert day_of_week to a factor with the specified levels
filtered_cyclist_df$day_of_week <- factor(filtered_cyclist_df$day_of_week, levels = day_levels)

# Summarize ride counts by day of the week and rider type
day_usage <- filtered_cyclist_df %>%
  group_by(day_of_week, member_casual) %>%
  summarise(ride_count = n(), .groups = 'drop') %>%
  arrange(day_of_week)
# View(day_usage)
# Plot the data with a side-by-side bar chart
day_usage_plot <- ggplot(day_usage, aes(x = day_of_week, y = ride_count, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c("casual" = "skyblue", "member" = "orange"),
    labels = c("casual" = "Casual", "member" = "Member")
  ) +
  labs(x = "Weekday", y = "Ride Count", fill = "Rider Type") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  )

#############################################
############################################# Days of the Week + Bins: Table

# Calculate totals for new bins
total_bins_dayOfWeek <- filtered_cyclist_df %>%
  summarise(
    total_5s_to_30s = sum(ride_length_minutes > 0.05 & ride_length_minutes <= .5, na.rm = TRUE),
    total_30s_to_2m = sum(ride_length_minutes > 0.5 & ride_length_minutes <= 2, na.rm = TRUE),
    total_2m_to_5m = sum(ride_length_minutes > 2 & ride_length_minutes <= 5, na.rm = TRUE),
    total_5m_to_10m = sum(ride_length_minutes > 5 & ride_length_minutes <= 10, na.rm = TRUE),
    total_10m_to_20m = sum(ride_length_minutes > 10 & ride_length_minutes <= 20, na.rm = TRUE),
    total_20m_to_60m = sum(ride_length_minutes > 20 & ride_length_minutes <= 60, na.rm = TRUE),
    total_60m_to_120m = sum(ride_length_minutes > 60 & ride_length_minutes <= 120, na.rm = TRUE),
    total_over_120m = sum(ride_length_minutes > 120, na.rm = TRUE)
  )

# Calculate count and percentage for each rider type, also factoring in days
ride_length_counts_dayOfWeek <- filtered_cyclist_df %>%
  mutate(day_of_week = weekdays(as.Date(started_at))) %>%
  group_by(day_of_week, member_casual) %>%
  summarise(
    count_5s_to_30s = sum(ride_length_minutes > 0.05 & ride_length_minutes <= 0.5, na.rm = TRUE),
    percent_5s_to_30s = round((sum(ride_length_minutes > 0.05 & ride_length_minutes <= 0.5, na.rm = TRUE) / total_bins_dayOfWeek$total_5s_to_30s) * 100, 1),
    count_30s_to_2m = sum(ride_length_minutes > 0.5 & ride_length_minutes <= 2, na.rm = TRUE),
    percent_30s_to_2m = round((sum(ride_length_minutes > 0.5 & ride_length_minutes <= 2, na.rm = TRUE) / total_bins_dayOfWeek$total_30s_to_2m) * 100, 1),
    count_2m_to_5m = sum(ride_length_minutes > 2 & ride_length_minutes <= 5, na.rm = TRUE),
    percent_2m_to_5m = round((sum(ride_length_minutes > 2 & ride_length_minutes <= 5, na.rm = TRUE) / total_bins_dayOfWeek$total_2m_to_5m) * 100, 1),
    count_5m_to_10m = sum(ride_length_minutes > 5 & ride_length_minutes <= 10, na.rm = TRUE),
    percent_5m_to_10m = round((sum(ride_length_minutes > 5 & ride_length_minutes <= 10, na.rm = TRUE) / total_bins_dayOfWeek$total_5m_to_10m) * 100, 1),
    count_10m_to_20m = sum(ride_length_minutes > 10 & ride_length_minutes <= 20, na.rm = TRUE),
    percent_10m_to_20m = round((sum(ride_length_minutes > 10 & ride_length_minutes <= 20, na.rm = TRUE) / total_bins_dayOfWeek$total_10m_to_20m) * 100, 1),
    count_20m_to_60m = sum(ride_length_minutes > 20 & ride_length_minutes <= 60, na.rm = TRUE),
    percent_20m_to_60m = round((sum(ride_length_minutes > 20 & ride_length_minutes <= 60, na.rm = TRUE) / total_bins_dayOfWeek$total_20m_to_60m) * 100, 1),
    count_60m_to_120m = sum(ride_length_minutes > 60 & ride_length_minutes <= 120, na.rm = TRUE),
    percent_60m_to_120m = round((sum(ride_length_minutes > 60 & ride_length_minutes <= 120, na.rm = TRUE) / total_bins_dayOfWeek$total_60m_to_120m) * 100, 1),
    count_over_120m = sum(ride_length_minutes > 120, na.rm = TRUE),
    percent_over_120m = round((sum(ride_length_minutes > 120, na.rm = TRUE) / total_bins_dayOfWeek$total_over_120m) * 100, 1),
    .groups = 'drop'
  )

# Reshape the data to the desired format
reshaped_counts_dayOfWeek <- ride_length_counts_dayOfWeek %>%
  pivot_longer(
    cols = -c(member_casual, day_of_week),
    names_to = c("metric", "interval"), 
    names_sep = "_", 
    values_to = "value"
  ) %>%
  mutate(
    interval = paste(interval, ifelse(metric == "count", "(Count)", "(%)")),
    day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  ) %>%
  pivot_wider(
    names_from = c(day_of_week, member_casual),
    values_from = value,
    values_fill = 0
  )

# Clean column names and capitalize
names(reshaped_counts_dayOfWeek) <- gsub("_", " ", names(reshaped_counts_dayOfWeek))
names(reshaped_counts_dayOfWeek) <- tools::toTitleCase(names(reshaped_counts_dayOfWeek))

#############################################
############################################# Days of the Week + Bins: Plot

# Create factor for ordering intervals
interval_levels <- c("5s_to_30s", "30s_to_2m", "2m_to_5m", "5m_to_10m", "10m_to_20m", "20m_to_60m", "60m_to_120m", "over_120m")

# Filter data for Casual riders and specific days
casual_weekend_data <- ride_length_counts_dayOfWeek %>%
  filter(member_casual == "casual", day_of_week %in% c("Friday", "Saturday", "Sunday")) %>%
  pivot_longer(cols = starts_with("count_"), names_to = "interval", values_to = "count") %>%
  filter(str_detect(interval, "count")) %>%
  mutate(interval = str_remove(interval, "count_")) %>%
  mutate(interval = factor(interval, levels = interval_levels))

# Create bar plots for each weekday
ggplot(casual_weekend_data, aes(x = interval, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  facet_wrap(~ day_of_week, scales = "free_y") +
  labs(
    title = "Ride Length Distribution for Casual Riders on Weekend Days",
    x = "Ride Length Interval",
    y = "Ride Count"
  ) +
  scale_x_discrete(labels = function(x) gsub("_", " ", gsub("_to_", " - ", tools::toTitleCase(x)))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#############################################
############################################# Subset for the Weekend

# Filter and summarise only casual riders and specific days (Fri, Sat, Sun)
weekend_casual_counts <- filtered_cyclist_df %>%
  mutate(day_of_week = weekdays(as.Date(started_at))) %>%
  filter(member_casual == "casual", day_of_week %in% c("Friday", "Saturday", "Sunday")) %>%
  group_by(day_of_week) %>%
  summarise(
    count_5s_to_30s = sum(ride_length_minutes > 0.05 & ride_length_minutes <= 0.5, na.rm = TRUE),
    count_30s_to_2m = sum(ride_length_minutes > 0.5 & ride_length_minutes <= 2, na.rm = TRUE),
    count_2m_to_5m = sum(ride_length_minutes > 2 & ride_length_minutes <= 5, na.rm = TRUE),
    count_5m_to_10m = sum(ride_length_minutes > 5 & ride_length_minutes <= 10, na.rm = TRUE),
    count_10m_to_20m = sum(ride_length_minutes > 10 & ride_length_minutes <= 20, na.rm = TRUE),
    count_20m_to_60m = sum(ride_length_minutes > 20 & ride_length_minutes <= 60, na.rm = TRUE),
    count_60m_to_120m = sum(ride_length_minutes > 60 & ride_length_minutes <= 120, na.rm = TRUE),
    count_over_120m = sum(ride_length_minutes > 120, na.rm = TRUE),
    .groups = 'drop'
  )

# Optional: Reshape data if necessary
reshaped_weekend_casual_counts <- weekend_casual_counts %>%
  pivot_longer(
    cols = starts_with("count"),
    names_to = "interval",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = day_of_week,
    values_from = value,
    values_fill = 0
  )

# Clean column names and capitalize
names(reshaped_weekend_casual_counts) <- gsub("_", " ", names(reshaped_weekend_casual_counts))
names(reshaped_weekend_casual_counts) <- tools::toTitleCase(names(reshaped_weekend_casual_counts))

# Remove underscores and the word 'count' from the Interval column
reshaped_weekend_casual_counts$Interval <- gsub("_", " ", reshaped_weekend_casual_counts$Interval)
reshaped_weekend_casual_counts$Interval <- gsub("count ", "", reshaped_weekend_casual_counts$Interval)
# names(reshaped_weekend_casual_counts$Interval) <- tools::toTitleCase(names(reshaped_weekend_casual_counts$Interval))

# Optional: Capitalize each word in the Interval column
reshaped_weekend_casual_counts$Interval <- tools::toTitleCase(reshaped_weekend_casual_counts$Interval)


#############################################
############################################# Create the Counts Flextable: Weekend

#Customize Flextable
reshaped_weekend_casual_counts_ft <- flextable(reshaped_weekend_casual_counts) %>%
  theme_box() %>%
  bold(part = "header") %>%
  color(part = "header", color = "white") %>%
  bg(part = "header", bg = "#4472c4") %>% 
  align(align = "center", part = "all") #center cell objects

# Manually specify alternating row colors
# This step is a workaround for the direct approach
row_indices <- seq_len(nrow(reshaped_counts))
even_rows <- row_indices %% 2 == 0
odd_rows <- !even_rows

# Apply zebra striping
reshaped_weekend_casual_counts_ft <- reshaped_weekend_casual_counts_ft %>%
  bg(i = odd_rows, bg = "#e6e6e6", part = "body") %>%
  bg(i = even_rows, bg = "white", part = "body")

# Adjust Width
reshaped_weekend_casual_counts_ft <- reshaped_weekend_casual_counts_ft %>%
  width(j = c("Interval"), width = 1) %>%
  width(j = c("Friday", "Saturday", "Sunday"), width = 1.1) 



#############################################
############################################# Plot for Weekend
# Create factor for ordering intervals
interval_levels <- c("5s_to_30s", "30s_to_2m", "2m_to_5m", "5m_to_10m", "10m_to_20m", "20m_to_60m", "60m_to_120m", "over_120m")

# Filter data for Casual riders and specific days
casual_weekend_data <- ride_length_counts_dayOfWeek %>%
  filter(member_casual == "casual", day_of_week %in% c("Friday", "Saturday", "Sunday")) %>%
  pivot_longer(cols = starts_with("count_"), names_to = "interval", values_to = "count") %>%
  filter(str_detect(interval, "count")) %>%
  mutate(interval = str_remove(interval, "count_")) %>%
  mutate(interval = factor(interval, levels = interval_levels))

# Create bar plots for each weekday
casualWeekendPlot <- ggplot(casual_weekend_data, aes(x = interval, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  facet_wrap(~ day_of_week, scales = "free_y") +
  labs(
    title = "Ride Length Distribution for Casual Riders on Weekend Days",
    x = "Ride Length Interval",
    y = "Ride Count"
  ) +
  scale_x_discrete(labels = function(x) gsub("_", " ", gsub("_to_", " - ", tools::toTitleCase(x)))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#############################################
############################################# Seasonal Data Subset

# Extract just the relevant columns from filtered_cyclist_df
filtered_cyclist_df <- filtered_cyclist_df %>%
  select(started_at, ride_length_minutes, member_casual) %>%
  mutate(
    month_year = floor_date(as.Date(started_at), "month")  # Extract month-year
  )

# Define a function to calculate totals for each month for a specific rider type
calculate_rider_type_totals <- function(data, rider_type) {
  data %>%
    filter(member_casual == rider_type) %>%
    filter(between(month_year, as.Date("2023-07-01"), as.Date("2024-06-30"))) %>% 
    group_by(month_year) %>%
    summarise(
      total_5s_to_30s = sum(ride_length_minutes > 0.05 & ride_length_minutes <= 0.5, na.rm = TRUE),
      total_30s_to_2m = sum(ride_length_minutes > 0.5 & ride_length_minutes <= 2, na.rm = TRUE),
      total_2m_to_5m = sum(ride_length_minutes > 2 & ride_length_minutes <= 5, na.rm = TRUE),
      total_5m_to_10m = sum(ride_length_minutes > 5 & ride_length_minutes <= 10, na.rm = TRUE),
      total_10m_to_20m = sum(ride_length_minutes > 10 & ride_length_minutes <= 20, na.rm = TRUE),
      total_20m_to_60m = sum(ride_length_minutes > 20 & ride_length_minutes <= 60, na.rm = TRUE),
      total_60m_to_120m = sum(ride_length_minutes > 60 & ride_length_minutes <= 120, na.rm = TRUE),
      total_over_120m = sum(ride_length_minutes > 120, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    pivot_longer(
      cols = -month_year,
      names_to = "interval",
      values_to = "counts"
    ) %>%
    pivot_wider(
      names_from = month_year,
      values_from = counts
    ) %>%
    mutate(
      interval = gsub("_", " ", gsub("_to_", " - ", tools::toTitleCase(gsub("total_", "", interval))))
    )
}

# Calculate totals for 'Casual' and 'Members' tables
casual_totals <- calculate_rider_type_totals(filtered_cyclist_df, "casual")
member_totals <- calculate_rider_type_totals(filtered_cyclist_df, "member")

# Clean column names and capitalize
names(casual_totals) <- gsub("_", " ", names(casual_totals))
names(casual_totals) <- tools::toTitleCase(names(casual_totals))

# Function to convert date strings in column names to "Month YYYY" format
convert_column_names <- function(df) {
  # Extract current names
  current_names <- colnames(df)
  
  # Convert date strings to desired format
  new_names <- sapply(current_names, function(x) {
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) {  # If the name looks like a date
      format(ymd(x), "%B %Y")
    } else {
      x  # Leave non-date names unchanged
    }
  })
  
  # Set new column names
  colnames(df) <- new_names
  df
}

# Apply the function to both data frames
casual_totals <- convert_column_names(casual_totals)
member_totals <- convert_column_names(member_totals)

#############################################
############################################# Line Plot for Season

# Summarize rides per month for casual and members
rides_summary <- filtered_cyclist_df %>%
  mutate(
    month_year = floor_date(as.Date(started_at), "month")
  ) %>%
  filter(between(month_year, as.Date("2023-07-01"), as.Date("2024-06-30"))) %>%
  group_by(member_casual, month_year) %>%
  summarise(total_rides = n()) %>%
  ungroup()

# Plotting the data
memberCasual_seasonal_linePlot <- ggplot(rides_summary, aes(x = month_year, y = total_rides, color = member_casual)) +
  geom_line(size = 1) +  # Line plot
  geom_point(size = 2) + # Points to highlight data points
  scale_x_date(
    date_labels = "%B %Y",
    breaks = seq(as.Date("2023-07-01"), as.Date("2024-06-01"), by = "1 month")
  ) +
  scale_color_manual(
    values = c("casual" = "skyblue", "member" = "orange"), # Customize colors if needed
    labels = c("Casual", "Member")  # Capitalize labels
  ) +
  labs(
    title = "Total Rides per Month",
    x = "Month",
    y = "Total Rides",
    color = "Rider Type"
  ) +
  theme_minimal() + # A clean theme for the plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability


#############################################
############################################# Seasonal: July / Aug 2023
######## Table
casual_july_august_2023 <- casual_totals %>%
  select(Interval, `July 2023`, `August 2023`)

# Print the subset data frame
print(casual_july_august_2023)

#################################################### Visualization

# Define the desired order of the intervals
interval_levels <- c("5s_to_30s", "30s - 2m", "2m - 5m", "5m - 10m", "10m - 20m", "20m - 60m", "60m - 120m", "Over 120m")

# Convert the data to long format for ggplot2
casual_long <- casual_july_august_2023 %>%
  pivot_longer(cols = -Interval, names_to = "Month", values_to = "Ride_Counts")

# Set the levels of the Interval factor to ensure proper ordering
casual_long$Interval <- factor(casual_long$Interval, levels = interval_levels)

# Plotting the data
seasonal_barMonthPlot <- ggplot(casual_long, aes(x = Interval, y = Ride_Counts, fill = Month)) +
  geom_bar(stat = "identity", position = position_dodge()) + # Position_dodge for side-by-side bars
  labs(
    title = "Casual Riders: Ride Counts by Interval for July and August 2023",
    x = "Interval",
    y = "Ride Counts",
    fill = "Month"
  ) +
  theme_minimal() + # Use a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability


#############################################
############################################# Popular Routes

##### Casual Rider Popular Routes and Average Time
# Function to get top 5 routes with average ride length
get_top_routes <- function(data, rider_type) {
  data %>%
    filter(member_casual == rider_type) %>%
    group_by(start_station_name, end_station_name) %>%
    summarise(
      route_count = n(),
      avg_ride_length = round(mean(ride_length_minutes, na.rm = TRUE), 1),
      .groups = "drop"  # Ensures that the output is ungrouped
    ) %>%
    arrange(desc(route_count)) %>%
    rename(
      "Start Station" = start_station_name,
      "End Station" = end_station_name,
      "Route Count" = route_count,
      "Avg. Ride Length (minutes)" = avg_ride_length
    ) %>%
    slice_head(n = 5)  # Selects the top 5 routes
}

# Get top 5 routes for casual riders
top_routes_casual <- get_top_routes(cleaned_cyclist_df, "casual")
print("Top 5 Most Popular Routes for Casual Riders:")
print(top_routes_casual)

# Get top 5 routes for member riders
top_routes_members <- get_top_routes(cleaned_cyclist_df, "member")
print("Top 5 Most Popular Routes for Member Riders:")
print(top_routes_members)

################################# Flextable
#Customize Flextable
# Define a function to customize a flextable
customize_flextable <- function(data) {
  # Create a base flextable
  ft <- flextable(data) %>%
    theme_box() %>%
    bold(part = "header") %>%
    color(part = "header", color = "white") %>%
    bg(part = "header", bg = "#4472c4") %>%
    align(align = "center", part = "all") # center all parts
  
  # Zebra striping
  row_indices <- seq_len(nrow(data))
  even_rows <- row_indices %% 2 == 0
  odd_rows <- !even_rows
  
  # Apply zebra striping
  ft <- ft %>%
    bg(i = odd_rows, bg = "#e6e6e6", part = "body") %>%
    bg(i = even_rows, bg = "white", part = "body")
  
  # Adjust column widths
  ft <- ft %>%
    width(j = c("Start Station", "End Station"), width = 1.5) %>%
    width(j = c("Route Count", "Avg. Ride Length (minutes)"), width = 1.1)
  
  return(ft)
}

popular_casual_ft <- customize_flextable(top_routes_casual)
popular_member_ft <- customize_flextable(top_routes_members)

