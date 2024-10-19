
# Title Slide
title_slide_title_description <- paste0("Marketing Report")

####################################################################################################
# Business Task
####################################################################################################

businessTask_Description_contentOnly <- paste0("The task is to analyze Cyclistic's historical bike trip data to identify differences in usage patterns between annual members and casual riders, in order to provide insights that can be leveraged to convert more casual riders into annual members. This analysis will focus on key metrics such as ride length, trip frequency, time of usage, and popular routes, aiming to reveal distinctive behaviors and preferences that can inform targeted marketing strategies.")

####################################################################################################
# Business Task
####################################################################################################

# Executive Descriptions
executive_header1 <- ftext("Objective\n", prop = fp_text(bold = TRUE, font.size = 12))
executive_text1 <- ftext("This report provides a comprehensive analysis of the Inhabit Sales Team, dissecting productivity and collaboration patterns across four distinct groups within the organization - Group A, Group B, Group C, and Group D. Our objective was to unearth pivotal patterns and trends that would illuminate best practices and pinpoint areas for improvement, ultimately boosting productivity and fostering a more collaborative environment within the all sales teams.\n\n", prop = fp_text(bold = FALSE, font.size = 12))
executive_header2 <- ftext("Group Composition\n", prop = fp_text(bold = TRUE, font.size = 12))
executive_text2 <- ftext("At the heart of our analysis lies the division into four groups: 
Group A, which includes the singular figure of Harry Zola; 
Group B, a collective of four sales employees from varied companies; 
Group C, encapsulating the Streamline Sales Team with the exclusion of Harry Zola; and 
Group D, encompassing all Sales personnel integrated with Prodoscore and affiliated with Inhabit, totaling 57 employees.\n\n", prop = fp_text(bold = FALSE, font.size = 12))
executive_header3 <- ftext("Key Findings\n", prop = fp_text(bold = TRUE, font.size = 12))
executive_text3 <- ftext("Office365 Calendar Utilization: Group A's unconventional high utilization of the Office365 Calendar starkly contrasts with the norm, signaling a unique yet inefficient approach to time management that necessitates further scrutiny.\n\n", prop = fp_text(bold = FALSE, font.size = 12))
executive_Highlight1 <- ftext("Prodoscore Performance: ", prop = fp_text(bold = TRUE, font.size = 12))
executive_text4 <- ftext("A clear hierarchy in productivity emerged, with Group D leading with the highest average Prodoscore, closely followed by Group C. This starkly highlights Group A and B's lagging performance, underscoring a critical need for intervention to elevate their productivity levels.\n\n", prop = fp_text(bold = FALSE, font.size = 12))
executive_Highlight2 <- ftext("Engagement Discrepancies: ", prop = fp_text(bold = TRUE, font.size = 12))
executive_text5 <- ftext("A significant underutilization of key productivity tools such as OneDrive, Salesforce, and Teams Chat was most pronounced in Group A, pointing to a concerning detachment from essential resources and a glaring lack of engagement that demands immediate corrective action.\n\n", prop = fp_text(bold = FALSE, font.size = 12))
executive_Highlight3 <- ftext("Salesforce and Teams Calls Engagement: ", prop = fp_text(bold = TRUE, font.size = 12))
executive_text6 <- ftext("The higher engagement levels in Salesforce activities and Teams Calls by Groups C and D not only exemplifies effective technology adoption, but also sets a benchmark of productivity and collaboration that Group A and B fall markedly short of.\n\n", prop = fp_text(bold = FALSE, font.size = 12))
executive_Highlight4 <- ftext("Operational Hours Analysis: ", prop = fp_text(bold = TRUE, font.size = 12))
executive_text7 <- ftext("Despite Group A registering the longest average workday, a refined analysis excluding Office365 Calendar data revealed a shorter operational day compared to Group C. This discrepancy further emphasizes Group A's inefficiency and misalignment with organizational productivity standards.\n\n", prop = fp_text(bold = FALSE, font.size = 12))

# Combine the text elements into a single paragraph object
executiveDescription_contentOnly <- fpar(executive_header1, executive_text1, executive_header2, executive_text2, executive_header3, executive_text3, executive_Highlight1, executive_text4, executive_Highlight2, executive_text5, executive_Highlight3, executive_text6, executive_Highlight4, executive_text7)

####################################################################################################
# Descriptives
####################################################################################################
demographics_productivity_description <- paste0("An analysis of ", dom_2, "'s ridership was conducted over a time period stretching from ", startDate, ", to ", endDate, ". The assessment observed a sample of ", num_casual_rides_table, " casual rides and ", num_membership_rides_table, " membership rides.\n\n", "*All data assessed was downloaded from the following website: https://divvy-tripdata.s3.amazonaws.com/index.html. The data has been made available by Motivate International Inc. This is public data that you can use to explore how different customer types are using Cyclistic bikes. No personally identifiable information exists in this data due to data privacy issues.")

####################################################################################################
# Documentation: Cleaning
####################################################################################################
documentationCleaning_Description_contentOnly <- paste0("• RStudio was used on a local computer for the entire cleaning process. The initial step involved loading necessary R packages such as data.table, dplyr, and ggplot2. To streamline the process, the script checks for missing packages and installs them as needed.\n\n",
"• All CSV files were located in a specified directory, bike_share_data. In order to ensure data consistency across files, column names and data types from each CSV file were compared through a custom function, get_col_info, which used the fread function from the data.table package to read the first 100 rows of each file. My script then compared these structures against a reference file (the first file in the list) to identify and report any discrepancies in column names or data structures amongst all files.\n\n",
"• To check for files with mismatching structures, a comparison was performed to pinpoint the exact differences, such as missing columns or data type mismatches. This was done by my custom function, print_detailed_diff, which highlighted the specific inconsistencies. Two of my CSV files were flagged for blank cells. I replaced the blank cells with NA, but did this after I combined all my individual CSV files into a single dataset.\n\n",
"• This consolidation was achieved through a loop that read each file, converted it to a data frame for compatibility with `dplyr` functions, and then combined it with the main dataset using bind_rows. The cleaned and combined dataset was then saved in two formats: as a CSV for easy access and as an FST file.\n\n",
"• The last bit of cleaning occurred after calculating the total number of data points within the dataset and before calculating the rest of the descriptive statistics seen in the prior slide. I created a new column `ride_length_minutes`, calculating the duration of each ride in minutes by taking the difference between the `ended_at` and `started_at` timestamps. I filtered out rides with zero or negative durations, counting and printing such instances across different rider types (e.g., 'casual' or 'member'). The dataset is filtered further to exclude these invalid rides for subsequent analysis.")

####################################################################################################
# Documentation: Manipulation
####################################################################################################

documentationManipulation_Description_contentOnly <- paste0("• I started my data manipulation by calculating summary statistics by rider type: the mean ride length, maximum ride length in hours, and the mode of ride initiation day. Column names are adjusted for readability, replacing underscores with spaces and capitalizing words. The results are formatted into a styled table using the flextable package (table seen at bottom of page).\n\n",
                                                            "• I created a ride_length_minutes histogram (Scott’s) of the data and found the data bunched close to 0 (ie. More shorter than longer rides observed) and slightly influenced by outliers. I then calculated total counts for different ride duration categories (bins) ranging from 30 seconds to over 120 minutes. The summarise function in `dplyr` is used to compute these totals and analyze the data by rider type (`member_casual`), calculating both counts for each duration category. I visualized this data in a side by side bar plot.\n\n",
                                                            "• I then manipulated and visualized the data for weekday trends, casual weekend ride counts based on ride lenght, seasonal ride count trends, round count trends in the 2 months with the highers number of casual rides, and finally, trends around the most popular bike stations.\n\n\n",
                                                            "* Powerpoint development and rendering was done through use of the Officer package.")

####################################################################################################
# Analysis: Ride Length / Day of Week
####################################################################################################

analysisDescription_basic <- paste0("• Member riders record the highest number of rides in the 5-10m, 10-20m, and 2-5m ride length ranges.\n\n",
                                    "• Casual riders record the highest number of rides in the 5-10m, 10-20m, and 20-60m ride length ranges.\n\n")

####################################################################################################
# Analysis: Ride Length / Day of Week
####################################################################################################

analysisDescription_day <- paste0("• Casual Riders have the highest number of rides on Friday (278,989), Saturday (395,033), & Sunday (336,459).\n\n",
                                    "• Member Riders have the highest number of rides on Tuesday(550,297), Wednesday(566,186), and Thursday(558,780).\n\n")
######################

analysisDescription_casualWeekend <- paste0("• On Saturday and Sunday, 10m - 20m and 20m - 60m ride lengths have the highest number of rides.\n\n",
                                            "• Ride lengths of 10m - 20m and 5m - 10m ride have the highest number of rides on Friday.")

####################################################################################################
# Analysis: Seasonal
####################################################################################################
analysisDescription_seasonal<- paste0("• August, July, September, and May had the highest rider counts for Casual and Member riders.\n\n",
"• July and August 2023 were the months with the highest rider counts and 10m – 20m, 20m – 60m and 5m - 10m were the most common length of rides during those months.")

####################################################################################################
# Analysis: Popular Routes
####################################################################################################
analysisDescription_popular<- paste0("• The above tables display the Top 5 Routes for Casual and Member riders along with the average ride length per route.\n", 
"• Casual riders have longer average ride lengths on their top 5 most popular routes than Member riders.\n",
"• If data collection is accurate, it appears that it's popular for Casual riders to start and end at the same station, suggesting they are touring a specific area at leisure rather than commuting purposes.\n",
"• A large amount of Start and End Station data for both Member and Casual riders is missing, suggesting a data collection issue that requires further investigation.")

####################################################################################################
# Conclusion
####################################################################################################
conclusion_header1 <- ftext("Key Findings\n", prop = fp_text(bold = TRUE, font.size = 12))
conclusion_body1 <- ftext("• Casual riders have the highest number of rides on Friday (278,989), Saturday (395,033), and Sunday (336,459), while member riders peak on Tuesday (550,297), Wednesday (566,186), and Thursday (558,780).
• Casual riders show increased activity on Saturdays and Sundays, particularly for ride lengths between 10-20 minutes and 20-60 minutes.
• The highest rider counts for both casual and member riders occur in August, July, September, May, and June.
\n\n", prop = fp_text(bold = FALSE, font.size = 12))

conclusion_header2 <- ftext("Recommendations\n", prop = fp_text(bold = TRUE, font.size = 12))
conclusion_body2 <- ftext("• Target marketing efforts on weekends (Saturday and Sunday) and Fridays, when casual riders are most active.
• Prepare campaigns for August, July, September, May, and June to align with peak casual rider activity.
• Gear marketing initiatives towards the most frequent ride lengths for casual riders: 10-20 minutes, 20-60 minutes, and 5-10 minutes.
• Enhance marketing at popular stations and routes frequented by casual riders, like Streeter Dr & Grand Ave and DuSable Lake Shore Dr & Monroe St.
\n\n", prop = fp_text(bold = FALSE, font.size = 12))

# Combine the text elements into a single paragraph object
conclusion_description <- fpar(conclusion_header1, conclusion_body1, conclusion_header2, conclusion_body2)