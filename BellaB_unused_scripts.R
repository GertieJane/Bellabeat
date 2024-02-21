

##Independent Variable on the X axis(The cause) / Dependent on the Y (The effect, Y's results depend on X)
### Title: "Y vs. X" x is used to predict the outcome of y

### Explain scatterplots with (Direction (pos, neg), Form (linear=straight line, curvilinear= bell), 
####Strength(slope 45 degree angle= strong, weak= almost straight))

##   REMOVED THIS from analysis too much: Keeping for notes on how to filter a dataframe
### Filter out rows with 0 steps and 24 hours (1440 min) of no movement & filter out Sedentary Minutes > 10 minutes
##### These rows contained 0 values across all columns except calories which seems odd
#  filter(!(TotalSteps == 0 & SedentaryMinutes == 1440)) %>% 
daily_activity_v2 <- daily_activity %>%
  mutate(sedentary_hours= SedentaryMinutes/60) %>%
  filter(!(TotalSteps == 0 & SedentaryMinutes == 1440)) %>%
  select(-LoggedActivitiesDistance, -TrackerDistance,
         -SedentaryActiveDistance)

##Column Totals:
da_totals <- daily_activity_v2 %>% 
  group_by(Id) %>%
  summarise(total_records= n(),
            total_steps= sum(TotalSteps),
            Total_distance= sum(TotalDistance),
            total_VA_minutes= sum(VeryActiveMinutes),
            total_FA_minutes= sum(FairlyActiveMinutes),
            total_LA_minutes= sum(LightlyActiveMinutes),
            total_SED_minutes= sum(SedentaryMinutes),
            total_cal= sum(Calories)) 



########this whole block was a waste of time, Don't think this is important:

###Make new columns for Date and Time from the POSIXct "ActivityHour" column:
hourly_intensity$Date <- as.Date(hourly_intensity$ActivityHour)
hourly_intensity$Time <- format(hourly_intensity$ActivityHour, "%H:%M:%S")
###Change Time column to factor for graphing there are a total of 24 hours
###this was useless: rolled back to chr for time
hourly_intensity$Time <- as.factor(hourly_intensity$Time)


### did not need after making list_named:

# map() from the purrr package to apply a function to each data frame in the list.
# The function being applied is specified by ~data.frame(Column = names(.), DataType = sapply(., typeof))
# This function creates a data frame for each data frame in the list, 
# where "Column" contains the column names (names(.)),
# and "DataType" contains the corresponding data types (sapply(., typeof)).
summary_list <- map(list_of_dfs, ~data.frame(Column = names(.), DataType = sapply(., typeof)))


# The bind_rows() dplyr combines the individual data frame summaries into a single data frame (summary_df). 
# .id= "DataFrame" argument ensures that an additional column named "DataFrame" is added to track which data frame
# each row belongs to.
summary_df <- bind_rows(summary_list, .id = "DataFrame")
