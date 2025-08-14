library(lubridate)
library(here)
library(tidyr)
library(dplyr)
library(ggplot2)
#install.packages("plotrix")
library(plotrix)

# Section 0: Save out subset zebrafish dataframe -delete this --------------------------

fish <- read.csv("C:/Users/ameli/Downloads/zebrafish_behavior.csv")

fish <- fish %>% filter(month(time) == 8)
table(fish$genotype, fish$experiment)
fish <- subset(fish, select = -c(experiment, unique_trackID))

write.csv(fish, here("zebrafish_behaviour_subset.csv"), row.names = FALSE)


# Section 1: Timecourse plots ---------------------------------------------
fish_df <- read.csv(here("zebrafish_behaviour_subset.csv"))
#look at the first 6 rows
head(fish_df)
#look at the last 6 rows
tail(fish_df)
#see how the data is structured
str(fish_df)
#convert time to a date-time object using the function as_datetime from lubridate
fish_df$time <- as_datetime(fish_df$time)
fish_df$day <- day(fish_df$time)
fish_df$hour <- hour(fish_df$time)
fish_df$minute <- minute(fish_df$time)

#plot the raw observations (870,000 observations, resolution = 6sec)
ggplot(fish_df, aes(x = time, y = Dist_6s)) + geom_line()
#plot all observations (870k), rounded to the nearest hour
ggplot(fish_df, aes(x = round_date(time, unit = "hour"), y = Dist_6s)) + geom_line()
#plot the mean speed per hour across all days
fish_df %>% group_by(day, hour) %>% summarise(Dist_6s = mean(Dist_6s)) %>% ggplot(., aes(x = hour, y = Dist_6s)) + geom_line()
fish_plot <- fish_df %>% group_by(day, hour, genotype) %>% summarise(Dist_6s = mean(Dist_6s)) %>% ggplot(., aes(x = hour, y = Dist_6s)) + geom_line(aes(x = hour, y= Dist_6s, color = genotype), size = 2) + facet_wrap(~day)
fish_plot <- fish_plot + annotate("rect", xmin = 0, xmax = 8.5, ymin = 0, ymax = Inf, fill = "slateblue", alpha = 0.5) + annotate("rect", xmin = 22.5, xmax = 24, ymin = 0, ymax = Inf, fill = "slateblue", alpha = 0.5)
fish_plot


# Section 2: Attempt at proportion plots ---------------------------
fish_WT <- fish %>% filter(genotype == "WT")

fish_counts <- fish %>% count(hour, state, genotype)

#plot total counts
#ggplot(fish_counts, aes(x = hour, y = n, color = genotype)) + geom_point() + geom_line() + facet_wrap(~state)

fish_totals <- fish_df %>% count(hour, genotype)
fish_totals <- cbind(fish_totals, rep(row.names(fish_totals), each = 2))
fish_counts <- fish_counts %>% arrange(state)
fish_counts$totals <- fish_totals$n
fish_counts$proportions <- fish_counts$n/fish_counts$totals

#plot proportions
proportion_plot <- ggplot(fish_counts, aes(x = hour, y = proportions, color = genotype)) + geom_point() + geom_line() + facet_wrap(~state)
proportion_plot

#for each individual (trackID) what is the amount of time spent in each state (wake or sleep)
#since each row is the call made off 6 seconds of observations we can use the number of rows as a measure of time (x6sec)
fish_counts <- fish_WT %>% count(hour, state, trackID)

#to find the proportion we need the total time recorded in sleep and wake for each individual
fish_totals <- fish_counts %>% group_by(hour, trackID) %>% summarise(total = sum(n))

#create a column of unique identifiers we can use to merge
fish_totals$hourtrackID <- paste(fish_totals$hour, fish_totals$trackID, sep = "_")
fish_counts$hourtrackID <- paste(fish_counts$hour, fish_counts$trackID, sep = "_")
#merge the dataframes into one
fish_counts <- merge(fish_counts,fish_totals) 

#take the proportion
fish_counts$proportion <- fish_counts$n/fish_counts$total

#take the mean of all of the trackID proportions for each state and each hour
fish_1 <- fish_counts %>% group_by(hour, state) %>% summarise(mean_proportion = mean(proportion))
#take the standard deviation of each individuals proportion of time awake and asleep (ie how much do they vary from the mean)
fish_2 <- fish_counts %>% group_by(hour, state) %>% summarise(SD = sd(proportion), SD_n = sum(n))
#SEM = SD/sqrt(n), where n is the number of observation (ie the number of timepoints - rows per individual per state per hour) 
fish_2$SEM <- fish_2$SD/sqrt(fish_2$SD_n)

fish_1 <- merge(fish_1, fish_2)
ggplot(fish_1, aes(x = hour, y = mean_proportion)) + geom_point() + geom_line() + geom_errorbar(aes(ymin = mean_proportion - SD, ymax = mean_proportion + SD)) + facet_wrap(~state)
ggplot(fish_1, aes(x = hour, y = mean_proportion)) + geom_point() + geom_line() + geom_errorbar(aes(ymin = mean_proportion - SEM, ymax = mean_proportion + SEM)) + facet_wrap(~state)

#add the standard error of the mean (SEM) which is the standard deviation /N 

fish_WT <- fish %>% filter(genotype == "WT")
data <- fish_WT$Dist_6s
std.error(data)
fish_WT %>% group_by(hour) %>% std.error()
lapply(unique(fish_WT$hour), function(x) {std.error(x)})


# Section 3: Proportion plot averaged by day -------------------------------------------------------------
fish_counts <- fish_df %>% count(day, hour, state, genotype)
#the total timepoints for both states (sleep and wake),used to get the proportion
fish_totals <- fish_counts %>% group_by(day, hour, genotype) %>% summarise(total = sum(n))
#there are about half as many rows in fish_totals since this is taking the number of rows per minute per hour per day
#and not also by the number of states (2). Not exactly half since not all minutes have both a sleep and wake observation (sometimes only awake or only sleeping)
#create a column of unique identifiers we can use to merge
fish_totals$uniquetrackID <- paste(fish_totals$day, fish_totals$hour, fish_totals$genotype, sep = "_")
fish_counts$uniquetrackID <- paste(fish_counts$day, fish_counts$hour, fish_counts$genotype, sep = "_")
#merge the dataframes into one
fish_counts <- merge(fish_counts, fish_totals)
#take the proportion (sleep proportion should be inverse of wake proportion)
fish_counts$proportion <- fish_counts$n/fish_counts$total
#find the average sleep proportion per hour (appears to still be proportional)
#not grouping by day will take the hour from each day and average them into the mean
fish_1 <- fish_counts %>% group_by(hour, state, genotype) %>% summarise(mean_proportion = mean(proportion))
#find the standard deviation of sleep proportion per hour across all days
fish_2 <- fish_counts %>% group_by(hour, state, genotype) %>% summarise(SD = sd(proportion))
#SEM = SD/sqrt(n), where n is the number of observation (ie the number of timepoints - rows per individual per state per hour?)
#fish_2$SEM <- fish_2$SD/sqrt(fish_2$SD_n)
#find the SEM of sleep proportion per hour
fish_3 <- fish_counts %>% group_by(hour, state, genotype) %>% summarise(SEM = std.error(proportion))
fish_1 <- merge(fish_1, fish_2)
fish_1 <- merge(fish_1, fish_3)
ggplot(fish_1, aes(x = hour, y = mean_proportion, colour = genotype)) + geom_point() + geom_line() + geom_errorbar(aes(ymin = mean_proportion - SD, ymax = mean_proportion + SD)) + facet_wrap(~state)
ggplot(fish_1, aes(x = hour, y = mean_proportion, colour = genotype)) + geom_point() + geom_line() + geom_errorbar(aes(ymin = mean_proportion - SEM, ymax = mean_proportion + SEM)) + facet_wrap(~state)

# Section 4: Proportion plot averaged by individual (trackID) -----------------------
fish_counts <- fish_df %>% count(day, hour, state, genotype, trackID)
#to find the proportion from the count we need the total time recorded in sleep and wake for each individual in each hour in each day
fish_totals <- fish_counts %>% group_by(day, hour, genotype, trackID) %>% summarise(total = sum(n))
#create a column of unique identifiers we can use to merge
fish_totals$uniquetrackID <- paste(fish_totals$day, fish_totals$hour, fish_totals$trackID, fish_totals$genotype, sep = "_")
#fish_counts has duplicated IDs wherever there is both a recorded bout of sleep and wake at a given hour for an individual
fish_counts$uniquetrackID <- paste(fish_counts$day, fish_counts$hour, fish_counts$trackID, fish_counts$genotype, sep = "_")
#merge the dataframes into one
fish_counts <- merge(fish_counts, fish_totals)
#take the proportion
fish_counts$proportion <- fish_counts$n/fish_counts$total
fish_1 <- fish_counts %>% group_by(day, hour, state, genotype) %>% summarise(mean_proportion = mean(proportion))
#take the standard deviation of each individuals proportion of time awake and asleep (ie how much do they vary from the mean)
fish_2 <- fish_counts %>% group_by(day, hour, state, genotype) %>% summarise(SD = sd(proportion), SD_n = sum(n))
#take the standard deviation of each individuals proportion of time awake and asleep (ie how much do they vary from the mean)
fish_2 <- fish_counts %>% group_by(day, hour, state, genotype) %>% summarise(SD = sd(proportion))
#SEM = SD/sqrt(n), where n is the number of observation (ie the number of timepoints - rows per individual per state per hour?)
#use the std.error() function from the plotrix package, gives different numbers
fish_3 <- fish_counts %>% group_by(day, hour, state, genotype) %>% summarise(SEM = std.error(proportion))
fish_1 <- merge(fish_1, fish_2)
fish_1 <- merge(fish_1, fish_3)
ggplot(fish_1, aes(x = hour, y = mean_proportion)) + geom_point() + geom_line() + geom_errorbar(aes(ymin = mean_proportion - SD, ymax = mean_proportion + SD)) + facet_wrap(~state + day)
ggplot(fish_1, aes(x = hour, y = mean_proportion)) + geom_point() + geom_line() + geom_errorbar(aes(ymin = mean_proportion - SEM, ymax = mean_proportion + SEM)) + facet_wrap(~state + day)
ggplot(fish_1, aes(x = hour, y = mean_proportion, colour = genotype)) + geom_point() + geom_line() + geom_errorbar(aes(ymin = mean_proportion - SEM, ymax = mean_proportion + SEM)) + facet_wrap(~state + day)
ggplot(fish_1, aes(x = hour, y = mean_proportion, colour = genotype)) + geom_point() + geom_line() + geom_errorbar(aes(ymin = mean_proportion - SEM, ymax = mean_proportion + SEM)) + facet_wrap(~state)

fish_1 <- fish_1 %>% group_by(hour, state, genotype) %>% summarise(mean_proportion = mean(proportion))
fish_1 <- fish_1 %>% group_by(hour, state, genotype) %>% summarise(mean_proportion = mean(mean_proportion))
fish_3 <- fish_1 %>% group_by(hour, state, genotype) %>% summarise(SEM = std.error(mean_proportion))
fish_1 <- merge(fish_1, fish_3)
ggplot(fish_1, aes(x = hour, y = mean_proportion, colour = genotype)) + geom_point() + geom_line() + geom_errorbar(aes(ymin = mean_proportion - SEM, ymax = mean_proportion + SEM)) + facet_wrap(~state)
fish_1 <- fish_counts %>% group_by(day, hour, state, genotype) %>% summarise(mean_proportion = mean(proportion))
#take the standard deviation of each individuals proportion of time awake and asleep (ie how much do they vary from the mean)
fish_2 <- fish_counts %>% group_by(day, hour, state, genotype) %>% summarise(SD = sd(proportion))
#SEM = SD/sqrt(n), where n is the number of observation (ie the number of timepoints - rows per individual per state per hour?)
#use the std.error() function from the plotrix package, gives different numbers
fish_3 <- fish_counts %>% group_by(day, hour, state, genotype) %>% summarise(SEM = std.error(proportion))
fish_1 <- merge(fish_1, fish_2)
fish_1 <- merge(fish_1, fish_3)
#averaging again by day
fish_1 <- fish_1 %>% group_by(hour, state, genotype) %>% summarise(mean_proportion = mean(mean_proportion))
fish_1 <- fish_counts %>% group_by(day, hour, state, genotype) %>% summarise(mean_proportion = mean(proportion))
#take the standard deviation of each individuals proportion of time awake and asleep (ie how much do they vary from the mean)
fish_2 <- fish_counts %>% group_by(day, hour, state, genotype) %>% summarise(SD = sd(proportion))
#SEM = SD/sqrt(n), where n is the number of observation (ie the number of timepoints - rows per individual per state per hour?)
#use the std.error() function from the plotrix package, gives different numbers
fish_3 <- fish_counts %>% group_by(day, hour, state, genotype) %>% summarise(SEM = std.error(proportion))
fish_1 <- merge(fish_1, fish_2)
fish_1 <- merge(fish_1, fish_3)
ggplot(fish_1, aes(x = hour, y = mean_proportion, colour = genotype)) + geom_point() + geom_line() + geom_errorbar(aes(ymin = mean_proportion - SEM, ymax = mean_proportion + SEM)) + facet_wrap(~state + day)
ggplot(fish_1, aes(x = hour, y = mean_proportion, colour = genotype)) + geom_point() + geom_line() + geom_errorbar(aes(ymin = mean_proportion - SEM, ymax = mean_proportion + SEM)) + facet_wrap(~state)
#averaging again by day
fish_4 <- fish_1 %>% group_by(hour, state, genotype) %>% summarise(mean_proportion = mean(mean_proportion))
fish_5 <- fish_1 %>% group_by(hour, state, genotype) %>% summarise(SEM = std.error(mean_proportion))
fish_6 <- merge(fish_4, fish_5)
ggplot(fish_6, aes(x = hour, y = mean_proportion, colour = genotype)) + geom_point() + geom_line() + geom_errorbar(aes(ymin = mean_proportion - SEM, ymax = mean_proportion + SEM)) + facet_wrap(~state)


# Section 5: Proportion plots averaged by minute --------------------------
fish_counts <- fish_df %>% count(day, hour, state, minute, genotype)
#the total timepoints for both states (sleep and wake),used to get the proportion
fish_totals <- fish_counts %>% group_by(day, hour, minute, genotype) %>% summarise(total = sum(n))
#there are about half as many rows in fish_totals since this is taking the number of rows per minute per hour per day
#and not also by the number of states (2). Not exactly half since not all minutes have both a sleep and wake observation (sometimes only awake or only sleeping)
#create a column of unique identifiers we can use to merge
fish_totals$uniquetrackID <- paste(fish_totals$day, fish_totals$hour, fish_totals$minute, fish_totals$genotype, sep = "_")
fish_counts$uniquetrackID <- paste(fish_counts$day, fish_counts$hour, fish_counts$minute, fish_counts$genotype, sep = "_")
#merge the dataframes into one
fish_counts <- merge(fish_counts, fish_totals)
#take the proportion (sleep proportion should be inverse of wake proportion)
fish_counts$proportion <- fish_counts$n/fish_counts$total
#find the average sleep proportion per hour (may not be directly proportional anymore because its the mean?)
fish_1 <- fish_counts %>% group_by(day, hour, state, genotype) %>% summarise(mean_proportion = mean(proportion))
#find the standard deviation of sleep proportion per hour
fish_2 <- fish_counts %>% group_by(day, hour, state, genotype) %>% summarise(SD = sd(proportion))
#SEM = SD/sqrt(n), where n is the number of observations
#find the SEM of sleep proportion per hour
fish_3 <- fish_counts %>% group_by(day, hour, state, genotype) %>% summarise(SEM = std.error(proportion))
fish_1 <- merge(fish_1, fish_2)
fish_1 <- merge(fish_1, fish_3)
ggplot(fish_1, aes(x = hour, y = mean_proportion, colour = genotype)) + geom_point() + geom_line() + geom_errorbar(aes(ymin = mean_proportion - SEM, ymax = mean_proportion + SEM)) + facet_wrap(~state + day)
ggplot(fish_1, aes(x = hour, y = mean_proportion, colour = genotype)) + geom_point() + geom_line() + geom_errorbar(aes(ymin = mean_proportion - SEM, ymax = mean_proportion + SEM)) + facet_wrap(~state)

#average again by day
fish_4 <- fish_1 %>% group_by(hour, state, genotype) %>% summarise(mean_proportion = mean(mean_proportion))
fish_5 <- fish_1 %>% group_by(hour, state, genotype) %>% summarise(SEM = std.error(mean_proportion))
fish_6 <- merge(fish_4, fish_5)
ggplot(fish_6, aes(x = hour, y = mean_proportion, colour = genotype)) + geom_point() + geom_line() + geom_errorbar(aes(ymin = mean_proportion - SEM, ymax = mean_proportion + SEM)) + facet_wrap(~state)

# Section 4: Sleep in day vs night zebrafish ------------------------------


