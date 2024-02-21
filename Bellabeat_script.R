
# Set up environment to read and explore data 
library(tidyverse)
library(skimr)
library(here)


#only need to run this once:
dir.create("images")

# This study included 33 participants using a fit bit device to record personal wellness
## Starting on April 12, 2016 and ending on May 12, 2016

##### Megalist: only narrow frames: 15 : in clean_script


###################################### Explore daily_activity
daily_activity <- read_csv('dailyActivity_merged.csv')
colnames(daily_activity)
glimpse(daily_activity)
skim_without_charts(daily_activity)

## Change ActivityDate from chr to date data format
daily_activity$ActivityDate <- mdy(daily_activity$ActivityDate)

## Number of distinct users= 33
summarise(daily_activity, daily_user_count= n_distinct(Id))
## Check for duplicates= 0
sum(duplicated(daily_activity))

## Total Stats on daily_activity
daily_activity %>%  
  select(TotalSteps,
         TotalDistance,
         VeryActiveMinutes,
         FairlyActiveMinutes,
         LightlyActiveMinutes,
         SedentaryMinutes) %>%
  summary()



### Checking suspicious distance columns for usefulness 
dist_sums<- colSums(daily_activity[, c("LoggedActivitiesDistance", "TrackerDistance","TotalDistance",
                                       "SedentaryActiveDistance")])
print(dist_sums)
#### Either very little information in these columns or too similar to another column  


### create SedentaryHours column for easier to understand graph
### De-select three distance columns for analysis in daily_activity_v2
daily_activity_v2 <- daily_activity %>%
  mutate(SedentaryHours= SedentaryMinutes/60) %>% 
  select(-LoggedActivitiesDistance, -TrackerDistance,
          -SedentaryActiveDistance)

##Total Stats on daily_activity_v2
daily_activity_v2 %>%  
  select(TotalSteps,
         TotalDistance,
         VeryActiveMinutes,
         FairlyActiveMinutes,
         LightlyActiveMinutes,
         SedentaryMinutes, SedentaryHours) %>%
  summary()


##Averages for daily_activity_v2 users 
###min and max date to see time frame
da_averages <- daily_activity_v2 %>%
   group_by(Id) %>%
   summarise(total_records=n(),
             average_steps= mean(TotalSteps),
             average_distance= mean(TotalDistance),
             average_VA_minutes= mean(VeryActiveMinutes),
             average_FA_minutes= mean(FairlyActiveMinutes),
             average_LA_minutes= mean(LightlyActiveMinutes),
             average_SED_minutes= mean(SedentaryMinutes),
             average_cal= mean(Calories),
             min_date= min(ActivityDate), max_date= max(ActivityDate))


#Boxplot for total_records to show what falls outside of the average
ggplot(data = da_averages, aes(x = factor(1), y = total_records)) +
  geom_boxplot() +
  theme_minimal()+
  labs(title="Daily Activity Records", subtitle="Days recorded per user", y= "Count of Records", x= "All Records")

##Average total records (days worth of data) in daily_activity_v2 = 28.5
summarize(da_averages, avg_records= mean(total_records))
### user Id: 4057192912 has 4 days worth of data 

### Box plot for total steps/ over 25,000 steps is way outside of data norm
ggplot(data = daily_activity_v2, aes(x = factor(1), y = TotalSteps)) +
  geom_boxplot() +
  theme_minimal()

## sort TotalSteps and SedentaryHours 
daily_activity_v2 %>% 
  arrange(TotalSteps, SedentaryHours)

### Check for 0 activity 0 steps: 72 Rows with no steps and 24 hours of sedentary activity 
daily_activity_v2 %>%
  filter(SedentaryHours== 24, TotalSteps== 0) %>% 
  summarise(total_records= n())


# Check per user zero activity 15 users have some 0 step days and full sedentary activity
daily_activity_v2 %>%
  group_by(Id) %>% 
  filter(SedentaryHours== 24, TotalSteps== 0) %>% 
  summarise(total_records= n())

# Create id_counts data frame to store counts of unique Id values in the Id columns (users) from:
## daily_activity_v2, heart_rate, daily_sleep_v2, and weight_log
###length(unique(df$Id)) counts the number of unique "Id" values in each data frame 
id_counts <- data.frame(
  tracked_data = c("Daily Activity", "Heart Rate", "Daily Sleep", "Weight Log"),
  Count = c(
    length(unique(daily_activity_v2$Id)),
    length(unique(heart_rate$Id)),
    length(unique(daily_sleep_v2$Id)),
    length(unique(weight_log$Id))
  )
)
# Create bar graph to represent data frames and unique Id values
ggplot(id_counts, aes(x = tracked_data, y = Count)) +
  geom_bar(stat = "identity", fill = "deepskyblue3") +
  theme_minimal() +
  theme(plot.title= element_text(color= "#f28b74", size= 18))+
  labs(title= "Number of Users per Wellness Dataset", subtitle= "Each dataset measures different metrics",
       x= "Wellness Dataset", y= "Number of Users", caption= "Total number of participants in this study = 33")

# The daily_activity dataset has step, distance, activity level, and calorie data recorded for All 33 participants
### Participants used their device to record this data for an average of 28.5 days 
# The daily_sleep dataset has minutes asleep and time in bed data recorded for 24 users
### Participants recorded sleep data for an average of 17.2 days
# The heart_rate dataset includes a heart rate reading every 5 seconds for 14 users
### Participants recorded heart rate data on average for 23.9 days
# The weight_log dataset includes weight, BMI, and if the weight was manually reported for 8 users
### participants recorded weight data on average for 8.4 days**
### ** 2 users recorded data for 24 and 30 days, the other 6 were 5 days and below (unbalanced data)
  
###### changing from "deepskyblue" to F number 

#For following plot activity order to make the legend better: 
activity_order <- c("Sedentary", "Lightly Active",
                    "Fairly Active","Very Active")

# Plot Types of activity
ggplot(daily_activity_v2, aes(x= ActivityDate, group= Id)) +
  geom_line(aes(y= VeryActiveMinutes/ 60, color= "Very Active")) +
  geom_line(aes(y= FairlyActiveMinutes/ 60, color= "Fairly Active")) +
  geom_line(aes(y= LightlyActiveMinutes/ 60, color= "Lightly Active")) +
  geom_line(aes(y= SedentaryMinutes/ 60, color= "Sedentary")) +
  labs(y= "Hours", x= "Date",
       title= "Activity Levels Over Time",
       subtitle = "Per Hour, by User Id",
       caption= "33 Participants were studied over 31 days",
       color= "Activity Type") +
  scale_color_manual(values= c(
    "Very Active"= "#cd2026",
    "Fairly Active"= "#0071bc",
    "Lightly Active"= "#2e8540",
    "Sedentary"= "#4c2c92"
   ), breaks= activity_order) +
  scale_y_continuous(breaks= seq(0, 24, 8))+
  theme_minimal()+
  theme(axis.text.x= element_text(angle= 48, hjust= 1), 
        plot.title= element_text(color= "#f28b74", size= 18),
        )+
    facet_wrap(~Id)
## vjust=1 moves the labels down (doesn't do much), hjust= 1 right justifies the text(hjust is good for angled text)
## hjust=0 is left justify, hjust= .5 is center text horizontally
## breaks in the scale_color_manual function will change the order to the activity_order created above




  



    #scale_y_continuous(
#      breaks= seq(0, 24, 6)





# Plot Types of activity without Sed
ggplot(daily_activity_v2, aes(x= ActivityDate, group= Id)) +
  geom_line(aes(y= VeryActiveMinutes, color= "Very Active Minutes")) +
  geom_line(aes(y= FairlyActiveMinutes, color= "Fairly Active Minutes")) +
  geom_line(aes(y= LightlyActiveMinutes, color= "Lightly Active Minutes")) +
  labs(y= "Minutes of Activity", x= "Date",
       title= "Activity Levels Over Time",
       subtitle = "Per Minute, by User Id",
       caption= "33 Participants were studied over 31 days") +
  scale_color_manual(values= c(
    "Very Active Minutes"= "#cd2026",
    "Fairly Active Minutes"= "#0071bc",
    "Lightly Active Minutes"= "#2e8540"
     )) +
  theme_minimal()+
  theme(axis.text.x= element_text(angle= 48, hjust= 1), 
        plot.title= element_text(color= "#f28b74", size= 18))+
  facet_wrap(~Id)








# Plot Sedentary per ActivityDate for each user
ggplot(daily_activity_v2) +
  geom_line(mapping=aes(x=ActivityDate, y=SedentaryHours, group=Id),color= "#ee6344") +
  facet_wrap(~Id)+
  theme_minimal()+
  theme(axis.text.x= element_text(angle= 45, hjust= 1), 
        plot.title= element_text(color= "#f28b74", size= 18)) +
  labs(y= "Sedentary Hours", x= "Date",
       title= "Sedentary Hours per Day",
       subtitle= "For Every User Id") +
  scale_y_continuous(breaks= seq(0, max(daily_activity_v2$SedentaryHours), by= 8))


### scale_y_continuous: customizes the scale of the y-axis in a continuous (numeric)variable. 
### breaks: argument specifies the positions where tick marks are placed
### breaks uses sequence function (seq()) generates seq of values from 0 to the max value of SedHours
### "By=1" parameter sets the interval between ticks to 1 or in this case 8

## can add label (0hrs to 8hrs example) to the values on the y axis with: 
## scale_y_continuous(breaks= seq(0, max(daily_activity_v2$SedentaryHours), by= 8),
## labels= paste(seq(0,max(daily_activity-v2$SedentaryHours), by=8), "hrs"))






########### Plot Sedentary Hours vs Steps Taken
ggplot(data= daily_activity_v2, aes(x= TotalSteps, y= SedentaryHours)) + 
  geom_point() + 
  geom_smooth(method = "lm", se= FALSE) +
  geom_jitter()+
  geom_smooth(data = subset(daily_activity_v2, SedentaryHours > 0 & SedentaryHours < 24 &
                              TotalSteps > 0 & TotalSteps < 25000),
              method = "lm", se = FALSE, color = "#FE6100") +
  theme_minimal()+
  theme(plot.title= element_text(color= "#f28b74", size= 18))+
  labs(y= "Sedentary Hours",
       x= "Total Steps", 
       title="Sedentary Hours vs Steps Taken per Day")+
  annotate("text", x=20000, y= 3, label= "The shorter red line ignores outliers",
           color= "#FE6100", size= 4.8)


#annotate("text", x= 1050, y= 650, label="The shorter red line ignores outliers", 
 #        color= "#FE6100", size= 4.8)
### Seems to have a slight negative correlation (as steps increase, sedentary time decreases)
## #FE6100 Only plots w/i parameters of subset >Min $ <Max / shortens data to exclude value and over
## Since I can't ask why there are so many rows of data with a full days worth of Sed Movement and usually no steps
### I decided not to delete it, but thought it was important to show 










################################## Explore daily_sleep:
daily_sleep <- read.csv("sleepDay_merged.csv")
colnames(daily_sleep)
skim_without_charts(daily_sleep)

## Convert SleepDay from chr to Date:
daily_sleep$SleepDay <- mdy_hms(daily_sleep$SleepDay) # (Has HMS but all times are 12:00:00 AM)
### Convert datetime to date:
daily_sleep$SleepDay <- as.Date(daily_sleep$SleepDay)






## Check for dupes= 3 (total of 413 observations)
sum(duplicated(daily_sleep))
## Look at dupes, they need to go
Sleep_dupes <- daily_sleep[duplicated(daily_sleep), ]
### drop dupes with distinct (total of 410 observations)
daily_sleep <- distinct(daily_sleep)

## Number of distinct users = 24 
summarise(daily_sleep, sleep_user_count= n_distinct(Id))  

glimpse(daily_sleep)



## summarize daily_sleep (Generally one sleep record per day)
daily_sleep %>%  
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()


#Filter out TotalSleepRecords, not sure what it means most have only one
daily_sleep_v2 <- daily_sleep %>% 
  select(-TotalSleepRecords) %>% 


## Summary per Id
daily_sleep_Summary <- daily_sleep_v2 %>%
  group_by(Id) %>%
  summarise(total_records=n(),
            average_minutes_asleep= mean(TotalMinutesAsleep), total_sleep_minutes= sum(TotalMinutesAsleep),
            average_minutes_inbed= mean(TotalTimeInBed), total_minutes_inbed= sum(TotalTimeInBed)) %>% 
  arrange(total_records)

## Average days recorded = 17.1
summarize(daily_sleep_Summary, avg_days_recorded= mean(total_records))

########### Plot minutes asleep vs time in bed (ANALYSIS)
ggplot(data= daily_sleep_v2, aes(x= TotalMinutesAsleep, y= TotalTimeInBed)) + 
  geom_point() +
  geom_smooth()+
  theme_minimal()+
  theme(plot.title= element_text(color= "#f28b74", size= 18))+
  labs(title= "Total Bedtime vs. Sleep Time", 
       y= "Total Minutes in Bed",
       x= "Total Minutes Asleep",
       caption= "24 users recorded sleep habits")
# Positive relationship between spending more time in bed and getting more sleep


# Plot TotalMinutesAsleep per SleepDay
ggplot(daily_sleep_v2) +
  geom_line(mapping=aes(x=SleepDay, y=TotalMinutesAsleep, group=Id),color= "#ee6344") +
  facet_wrap(~Id)+
  theme_minimal()+
  theme(axis.text.x= element_text(angle= 45, hjust= 1), 
        plot.title= element_text(color= "#f28b74", size= 18)) +
  labs(y= "Total Sleep Minutes", x= "Date",
       title= "Daily Minutes Alseep",
       subtitle= "For Every User Id")

# Plot for Total Minutes Asleep
ggplot(data = daily_sleep, aes(x = 1, y = TotalMinutesAsleep)) +
  geom_boxplot() +
  theme_minimal()+
  labs(title="Total Minutes Asleep")
# Over 700 and under about 120 minutes outside norm





##### Prep to merge daily_sleep_v2 and daily_activity_v2 (left Join on Id and date for difference in amount of data)
## joining on date and Id to make sure the observation of both stay intact
## daily_activity_v2 has 940 obs. daily_sleep_v2 has 410 obs.

### daily_sleep_v2 rename date for merging data with daily_activity
daily_sleep_v3 <- daily_sleep_v2 %>% 
  rename(Date= SleepDay) 

### Rename ActivityDate to Date for easier merging in daily_activity
daily_activity_v3 <- daily_activity_v2 %>% 
  rename(Date=ActivityDate)


### Left Join daily_sleep and daily_activity:
daily_sleep_act <- merge(daily_sleep_v3, daily_activity_v3, by= c("Id", "Date"), all.x= TRUE)
#### total 410 obs. 

# Checking merged data frame for issues
## Total user count= 24
summarise(daily_sleep_act, total_user_count= n_distinct(Id))
## Check for dupes= 0
sum(duplicated(daily_sleep_act))


## summarize daily_sleep_act 
daily_sleep_act %>%  
  select(TotalMinutesAsleep,
         TotalTimeInBed, TotalSteps, 
         TotalDistance, SedentaryMinutes,
         LightlyActiveMinutes, FairlyActiveMinutes,
         VeryActiveMinutes) %>%
  summary()



# No real correlation Sleep vs. Steps
ggplot(data=daily_sleep_act, aes(x= TotalSteps, y= TotalMinutesAsleep)) +
  geom_point() +
  geom_smooth(method = "lm", se= FALSE) +
  theme_minimal()+
  theme(plot.title= element_text(color= "#f28b74", size= 18)) +
  labs(y= "Total Minutes Asleep",
       x= "Total Steps",
       title= "Time Spent Asleep Vs. Total Steps")



# box plot to see potential outliers DOES NOT CHANGE ANYTHING
ggplot(data = daily_sleep_act, aes(x = 1, y = TotalMinutesAsleep)) +
  geom_boxplot() +
  theme_minimal()
### Ran a box plot for both TotalMinutesAsleep and SedentaryMinutes to see potential outliers 


# Moderate negative correlation: an increase in sedentary minutes decreases sleep duration
ggplot(data=daily_sleep_act, aes(x= SedentaryMinutes, y= TotalMinutesAsleep)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  geom_smooth(data = subset(daily_sleep_act, SedentaryMinutes >= 250 & SedentaryMinutes <= 1250 &
                              TotalMinutesAsleep >= 150 & TotalMinutesAsleep <= 700),
              method = "lm", se = FALSE, color = "#FE6100") +
  theme_minimal()+
  theme(plot.title= element_text(color= "#f28b74", size= 18)) +
  labs(title= "Total Minutes Asleep Vs. Sedentary Minutes",x= "Sedentary Minutes", y= "Total Minutes Asleep")+
  annotate("text", x= 1050, y= 650, label="The shorter red line ignores outliers", 
           color= "#FE6100", size= 4.8)
# Added red (Accessible color pallet) subset line to disregard some outliers based on box plots
## of both TotalMinutesAsleep and SedentaryMinutes.Only plots w/i parameters of subset >Min $ <Max







############################# Explore heart_rate
## 14 users with heart rate data taken every 5 seconds
heart_rate <- read_csv('heartrate_seconds_merged.csv')
colnames(heart_rate)
glimpse(heart_rate)
skim_without_charts(heart_rate)
## change Time column from chr to datetime, convert am/pm
heart_rate$Time <- parse_date_time(heart_rate$Time, "%m/%d/%y %I:%M:%s %p")

## Number of distinct users = 14 
summarise(heart_rate, heartrate_user_count= n_distinct(Id))
## Check for Dupes= 0
sum(duplicated(heart_rate))

heart_rate %>%  
  select(Value) %>%
  summary()


## Summarize heart_rate
## Pull date to see how many days were used
heart_rate_summary <- heart_rate %>%
  mutate(Date= as.Date(Time)) %>%  # Extract date from dttm
  group_by(Id) %>%
  summarise(total_records= n(),
    total_days= n_distinct(Date),
    average_heart_rate= mean(Value))
## Average days recorded = 23.9
summarize(heart_rate_summary, avg_records= mean(total_days))



##################### Explore weight_log
weight_log <- read_csv('weightLoginfo_merged.csv')
colnames(weight_log)
glimpse(weight_log)
skim_without_charts(weight_log)
## change date column from chr to datetime, convert am/pm:
weight_log$Date <- parse_date_time(weight_log$Date, "%m/%d/%y %I:%M:%s %p")

## Number of distinct users= 8 
summarise(weight_log, weight_user_count= n_distinct(Id))
## check for Dupes= 0
sum(duplicated(weight_log))


##summarize weight_log
weight_log_Summary <- weight_log %>%
  group_by(Id) %>%
  summarise(total_records=n(),
            average_wight_lb= mean(WeightPounds),
            average_BMI= mean(BMI),
            total_true_manual= sum(IsManualReport),
            percent_true_manual= mean(IsManualReport)*100)

## Average days recorded = 8.4
summarize(weight_log_Summary, avg_records= mean(total_records))



###############Explore hourly_intensity (how much intensity per hour for every hour in the day)
hourly_intensity <- read_csv("hourlyIntensities_merged.csv")
glimpse(hourly_intensity)
skim_without_charts(hourly_intensity)
head(hourly_intensity)
tail(hourly_intensity)
## Convert hourly_intensity from chr to datetime, convert am/pm 
hourly_intensity$ActivityHour <- parse_date_time(hourly_intensity$ActivityHour, "%m/%d/%y %I:%M:%s %p")

## Number of distinct users = 33 
summarise(hourly_intensity, intensity_user_count= n_distinct(Id))
## Check for Dupes= 0
sum(duplicated(hourly_intensity))


hourly_intensity %>%  
  select(TotalIntensity, AverageIntensity) %>%
  summary()


# Summarize hourly_intensity
## Pull date to see how many days were used
hourly_intensity_summary <- hourly_intensity %>%
  mutate(Date= as.Date(ActivityHour)) %>%  # Extract date from dttm
  group_by(Id) %>%
  summarise(total_records= n(),
            total_days= n_distinct(Date),
            average_intensity= mean(TotalIntensity))



### (format(ActivityHour, "%H"))...format dttm and extract hour part of the time stamp. 
### Then x= as.factor converts the hour to a factor data type to make it categorical
#### stat_summary() in ggplot2 computes a summary statistic, use fun= "mean" to set the function applied to data
#### geom= "bar" - mean is represented as bars, Fill number #1E88E5 is an accessible shade of blue
ggplot(hourly_intensity, aes(x= as.factor(format(ActivityHour, "%H")), y= TotalIntensity)) +
  stat_summary(fun= "mean", geom= "bar", fill= "#1E88E5") +
  theme_minimal()+
  theme(plot.title= element_text(color= "#f28b74", size= 18)) +
  labs(title= "Average Intensity Per Hour", x= "Hour of the Day", y= "Average Intensity")
###5pm to 7pm are the most active times 





################# Explore hourly_steps: "ActivityHour" "StepTotal"
hourly_steps <- read_csv("hourlySteps_merged.csv")
glimpse(hourly_steps)
## Convert chr date to datetime. convert am/pm
hourly_steps$ActivityHour <- parse_date_time(hourly_steps$ActivityHour, "%m/%d/%y %I:%M:%s %p")

## Number of distinct users= 33 
summarise(hourly_steps, steps_user_count= n_distinct(Id))
## Check for Dupes= 0
sum(duplicated(hourly_steps))


hourly_steps %>% 
  select(StepTotal) %>% 
  summary()

## Total steps Taken: 7075356 
hourly_steps %>%  
  select(StepTotal) %>%
  summarise(total_steps= sum(StepTotal))

ggplot(hourly_steps, aes(x= as.factor(format(ActivityHour, "%H")), y= StepTotal)) +
  stat_summary(fun= "mean", geom= "bar", fill= "#1E88E5") +
  theme_minimal()+
  theme(plot.title= element_text(color= "#f28b74", size= 18)) +
  labs(title= "Average Steps Per Hour", x= "Hour of the Day", y= "Average Steps")




############################################################################
## quick process: download - click arrow_open - click file - click local file to extract to - party
##############################################################################
# Unused csv exploration 
### Hourly Calories:  "ActivityHour" "Calories"
hourly_calories <- read_csv("hourlyCalories_merged.csv")

## Convert chr date to datetime. convert am/pm
hourly_calories$ActivityHour <- parse_date_time(hourly_calories$ActivityHour, "%m/%d/%y %I:%M:%s %p")

## Number of distinct users= 33 
summarise(hourly_calories, cal_user_count= n_distinct(Id))
## Check for Dupes= 0
sum(duplicated(hourly_calories))

## Total Stats 
hourly_calories %>%  
  select(Calories) %>%
  summary()




###### minuteStepsNarrow: "ActivityMinute" "Steps" Long Form 33 users
minute_steps_n <- read_csv("minuteStepsNarrow_merged.csv")
## total 33 users, datetime in rows for every minute with steps per minute
summarize(minute_steps_n, m_step_user_count= n_distinct(Id))

## Total Steps: 7073549
minute_steps_n %>%
  select(Steps) %>%
  summarise(total_steps= sum(Steps))

##### minuteStepsWide: 33 users "Id, ActivityHour, Steps00 - 59" 
### columns for steps per minute 00-59 per each datetime hour in each row
minute_steps_w <- read_csv("minuteStepsWide_merged.csv")
summarize(minute_steps_w, user_count= n_distinct(Id))
colnames(minute_steps_w)



#### minuteCaloriesNarrow: "ActivityMinute" "Calories", 33 users
minute_calories_n <- read_csv("minuteCaloriesNarrow_merged.csv")
colnames(minute_calories_n)
summarize(minute_calories_n, m_cal_user_count= n_distinct(Id))


### minuteCaloriesWide: 33 users, "Id", "ActivityHour", "Calories00" - "Calories59"
## columns for calories per minute 00-59 for each datetime hour in each row
minute_calories_w <- read_csv("minuteCaloriesWide_merged.csv") 
summarize(minute_calories_w, w_cal_user_count= n_distinct(Id))
colnames(minute_calories_w)


### minuteIntensitiesNarrow: 33 users, "Id", "ActivityMunite", "Intensity" 
## every minute is a row with datetime and intensity per minute  
minute_intens_n <- read_csv("minuteIntensitiesNarrow_merged.csv")
colnames(minute_intens_n)
summarize(minute_intens_n, N_intens_user_count= n_distinct(Id))

### minuteIntensitiesWide: 33 users, "Id", "ActivityHour", "Intensity00 - 59"
### columns for intensity per minute 00-59 per each datetime hour in each row
minute_intens_w <- read_csv("minuteIntensitiesWide_merged.csv")
summarize(minute_intens_w, user_count= n_distinct(Id))
colnames(minute_intens_w)




### minuteSleep: 24 users "Id, date, value, logId"
### value is undefined I think it's minutes asleep, but not sure
minute_sleep <- read_csv("minuteSleep_merged.csv")
summarize(minute_sleep, user_count= n_distinct(Id))

### dailyCalories: 33 users "Id, ActivityDay, Calories
## Calories per user per day
daily_calories <- read_csv("dailyCalories_merged.csv")
summarize(daily_calories, user_count= n_distinct(Id))


### dailyIntensities: 33 Users "Id, SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMintues.."
## Activity and dist per user per day
daily_intensities <- read_csv("dailyIntensities_merged.csv")
summarize(daily_intensities, user_count= n_distinct(Id))


### dailySteps: 33 Users, "Id, ActivityDay, StepTotal
## Daily steps per user per day
daily_steps <- read_csv("dailySteps_merged.csv")
summarize(daily_steps, user_count= n_distinct(Id))

distinct(daily_steps, Id)


#### minuteMETsNarrow: 33 users, "Id, ActivityMinute, METs"
## METs per minute, every minute (used to calc intensities/activity levels)
minute_MET <- read_csv("minuteMETsNarrow_merged.csv")
summarize(minute_MET, user_count= n_distinct(Id))
colnames(minute_MET)
