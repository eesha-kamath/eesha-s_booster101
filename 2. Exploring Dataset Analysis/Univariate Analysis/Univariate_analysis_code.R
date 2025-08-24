getwd()
df<-read.csv("tutoring_data.csv", header=TRUE)
head(df)
str(df)
sapply(df, function(x) length(unique(x)))

#unnecessary columns to be removed
df_clean<-subset(df, select=-c(User_ID, Location, Course_Name))
head(df_clean)
df_clean<-subset(df_clean, select=-c(Subscription_Cost, Subscription_Length_in_Months, Exercises_Started, Referrals))
head(df_clean)
df=df_clean
str(df)
#______________________________________________________

#descriptive statistics for numeric variables
basic_stats<-sapply(df[, c("Age_in_Months", "Total_Time_Spent_in_Minutes", "Completion_Rate", "Average_Score", "Days_Completed_Activity")],
       function(x) c(
         Mean = mean(x, na.rm = TRUE),
         Median = median(x, na.rm = TRUE),
         SD = sd(x, na.rm = TRUE),
         Var = var(x, na.rm = TRUE),
         Min = min(x, na.rm = TRUE),
         Max = max(x, na.rm = TRUE)
       ))
basic_stats=round(basic_stats,2)
print(basic_stats)

#plots for numeric variabes
hist(df$Age_in_Months,
     main = "Distribution of Age",
     xlab = "Age (Months)",
     col = "lightblue", border = "black"
     )
boxplot(df$Average_Score,
        main = "Boxplot of Average Score",
        ylab = "Average Score",
        col = "orange")
boxplot(df$Total_Time_Spent_in_Minutes,
        main = "Boxplot of Total Time Spent in Minutes",
        ylab = "Minutes",
        col = "orange")
plot(density(df$Total_Time_Spent_in_Minutes, na.rm = TRUE),
     main = "Density of Time Spent",
     xlab = "Time Spent (Minutes)")
hist(df$Days_Completed_Activity,
     main = "Distribution of Days Completed",
     xlab = "Days",
     col = "lightblue", border = "black")

#skewness and kurtosis
install.packages("moments")
library(moments)
cols <- c("Total_Time_Spent_in_Minutes", "Days_Completed_Activity", "Points_Earned")
skew_kurt <- sapply(df[, cols], function(x) c(
  Skewness = skewness(x, na.rm = TRUE),
  Kurtosis = kurtosis(x, na.rm = TRUE)
))
skew_kurt <- round(skew_kurt, 2)
print(skew_kurt)

plot(density(df$Points_Earned, na.rm = TRUE),
     main = "Points earned",
     xlab = "TPoints earned")
#______________________________________________________

#categorical variable analysis
gender_counts <- table(df$Gender)
gender_perc <- round(100 * gender_counts / sum(gender_counts), 1)
labels <- paste(names(gender_counts), gender_perc, "%")
pie(gender_counts,
    labels = labels,
    main = "Gender Distribution",
    col = rainbow(length(gender_counts)))

course_counts <- table(df$Course_Category)
course_perc <- round(100 * course_counts / sum(course_counts), 1)
labels <- paste(names(course_counts), course_perc, "%")
pie(course_counts,
    labels = labels,
    main = "Course Category Distribution",
    col = rainbow(length(course_counts)))
#______________________________________________________

#ordinal variable analysis
barplot(table(df$Grade),
        main = "Grade Distribution",
        xlab = "Grade",
        ylab = "Count",
        col = "lightgreen")

barplot(table(df$Academic_Grade),
        main = "Academic Grade Distribution",
        xlab = "Academic Grade",
        ylab = "Count",
        col = "lightgreen")
#______________________________________________________
