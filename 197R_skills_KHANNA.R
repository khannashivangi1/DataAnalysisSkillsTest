# Shivangi Khanna
# This was a skills test for a class I took in Spring 2020, CICS 197R: Intro to Data Analysis in R.


#This vector represents samples of bacteria levels taken at a private pool, once per day over many years, in parts per million.
level_data_vector <- level_data

#1 Report basic summary statistics of these data: mean, range, and median.
mean(level_data_vector, na.rm = T)
range(level_data_vector, na.rm = T)
median(level_data_vector, na.rm = T)

#2 Plot a histogram of the data.
hist(level_data_vector, ylim = c(0, 2000))

#3 How many values are missing? What portion of the total does this represent?
na <- sum(is.na(level_data_vector))
na
total <- length(level_data_vector)
portion <- (na/total)*100
portion
# The NA values represent 0.25% of the values

#4 Zeros are not likely to appear in a real-world scenario and likely represent equipment failure or a mistaken recording. Replace any zeros with NA. How many were there?
level_data_vector[level_data_vector == 0] <- NA
sum(is.na(level_data_vector)) - na

#5 Store the vector of data in a matrix. It should have 7 columns for each day of the week, starting on a Sunday. Name the columns.
level_data_matrix <- matrix(level_data_vector, ncol = 7)
colnames(level_data_matrix) <- c("Sunday", 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')

#6 Compute the average bacteria level on each day of the week (Monday, etc.) and plot it in a bar plot.
colMeans(level_data_matrix, na.rm = T)
barplot(colMeans(level_data_matrix, na.rm =T), main = "Barplot", xlab = 'Day', ylab = 'Mean Bacteria Level')

#7 If bacteria levels exceed 6.5, the pool must be closed. On how many days did this occur? How many weeks was the pool closed at least once? On how many weeks was it closed more than once?
total_closed <- length(level_data_vector[which(level_data_vector > 6.5)])
#45 days
closed_once <- length(level_data_matrix[which(level_data > 6.5)])
closed_once
# The pool was closed only once a week throughout the data set.
total_closed - closed_once

#8 Plot the median weekly value over the entire time period. (Whatever plot type you think is appropriate.) Describe any trends you see.
plot(apply(level_data_matrix, 1, median), type = "l", xlab = 'Week', ylab = 'Median bacterial level for the week')
#Around the 650th week, the bacterial levels fell. We can notice that the pattern starts repeating after this fall. The median bacteria level rises one week and falls the next week.

#9 Using apply and a custom (anonymous) function, find the weekly differences between the maximum and minimum daily readings. Which week had the greatest extreme? What year did it occur in? Retrieve all values from that week.
max.min.diff <- apply(level_data_matrix, 1, FUN = function(level_data_matrix) {
  max <- max(level_data_matrix)
  min <- min(level_data_matrix)
  max - min
})
week <- which.max(max.min.diff)
week
# Week 337
week/52
# The 6th year
level_data_matrix[337,]

#10 Consider that the filteration system is changed every other week, Saturday evening, starting at the end of the first week. Thus, half the weeks have relatively fresh filters; half have older filters. Find the overall mean values for both sets of weeks and compare them; is there a noticable difference?
sum_even <- 0
sum_odd <- 0
mean_odd <- 0
mean_even <- 0
odd <- 0
even <- 0
for (i in nrow(level_data_matrix)) {
  if(i %% 2 == 0) {
    sum_even = sum_even + level_data_vector[i]
    even = even + 1
  } else {
    sum_odd <- sum_odd + level_data_vector[i]
    odd = odd + 1
  }
}
mean_odd = sum_odd/odd
mean_odd
mean_even = sum_even/even
mean_even

#11 Convert your matrix into a data frame. Add three columns: one labeling which year it is (1998-2019); another for the week number within that year; and a third with the mean reading for that week.
df <- data.frame(level_data_matrix)
index_week <- 1:nrow(level_data_matrix)
year <- 1997
week <- 1
for(j in index_week) {
  if(index_week[j] %% 52 == 1) {
    mean_week = mean(level_data_matrix[j,])
    year = year + 1
    df$year[j] = year
    week = 1
    df$week[j] = week
    df$mean_week[j] = mean_week
  } else {
    df$year[j] = year
    week = week + 1
    df$week[j] = week
    mean_week = mean(level_data_matrix[j,])
    df$mean_week[j] = mean_week
  }
}

#12 Calculate the mean (of the weekly mean) in each year. Also calculate the mean across years by week number, and plot them.
for(k in index_week) {
  week = 1
  if(index_week[j] %% 52 == 1) { 
    yearly_mean <- mean(level_data_vector[week:(week+52)])
  } else {
    week = week + 1
  }
}
sum_week_num <- 0
vec <- 1:21
for(m in 1:52) {
  if(df$week[m] == m) {
    sum_week_num = sum_week_num + df$mean_week[m]
  }
  vec[m] = sum_week_num/21
  sum_week_num = 0
}
plot(vec)

