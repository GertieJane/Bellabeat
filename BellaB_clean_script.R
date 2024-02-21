
#### Before cleaning run these with the Mega Script to show how they were to begin with 

daily_activity <- read_csv('dailyActivity_merged.csv')
daily_sleep <- read.csv("sleepDay_merged.csv")
heart_rate <- read_csv('heartrate_seconds_merged.csv')
weight_log <- read_csv('weightLoginfo_merged.csv')
hourly_intensity <- read_csv("hourlyIntensities_merged.csv")
hourly_steps <- read_csv("hourlySteps_merged.csv")
hourly_calories <- read_csv("hourlyCalories_merged.csv")
minute_steps_n <- read_csv("minuteStepsNarrow_merged.csv")
minute_calories_n <- read_csv("minuteCaloriesNarrow_merged.csv")
minute_intens_n <- read_csv("minuteIntensitiesNarrow_merged.csv")
minute_MET <- read_csv("minuteMETsNarrow_merged.csv")
minute_sleep <- read_csv("minuteSleep_merged.csv")
daily_calories <- read_csv("dailyCalories_merged.csv")
daily_intensities <- read_csv("dailyIntensities_merged.csv")
daily_steps <- read_csv("dailySteps_merged.csv")


#list containing all data frames.
list_of_dfs <- list(daily_activity, daily_sleep_dist, daily_calories, daily_intensities, daily_steps, heart_rate,
                    weight_log, hourly_intensity,hourly_steps, hourly_calories, minute_steps_n, minute_calories_n,
                    minute_intens_n, minute_MET, minute_sleep)

list_named <- list('daily_activity'= daily_activity, 'daily_calories'= daily_calories, 'daily_steps'= daily_steps,
                   'daily_intensities'= daily_intensities, 'daily_sleep'= daily_sleep, 'heart_rate'= heart_rate,
                   'weight_log'= weight_log, 'hourly_intensity'= hourly_intensity, 'hourly_steps'= hourly_steps,
                   'hourly_calories'= hourly_calories, 'minute_steps_n'= minute_steps_n,'minute_MET'= minute_MET,
                   'minute_caliries_n'= minute_calories_n, 'mintue_intens_n'= minute_intens_n, 
                   'minute_sleep'= minute_sleep)




