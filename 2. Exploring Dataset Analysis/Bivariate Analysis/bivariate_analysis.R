getwd()
setwd("E:/Others/assignments/Honours AIML/Assignment2")
getwd()
df<-read.csv("diabetes.csv")
head(df)

str(df)
summary(df)

#select numeric variables and compute correlation matrix
numeric_vars <- df[, sapply(df, is.numeric)]
cor_matrix <- cor(numeric_vars, use = "complete.obs")

#print pairwise correlations
for(i in 1:(ncol(cor_matrix)-1)) {
  for(j in (i+1):ncol(cor_matrix)) {
    cat(colnames(cor_matrix)[i], "-", colnames(cor_matrix)[j], ":",
        round(cor_matrix[i, j], 2), "\n")
  }
}

#print correlation of each predictor with Outcome
for(i in 1:(ncol(cor_matrix)-1)) {
    cat("Outcome", "-", colnames(cor_matrix)[i], ":",
        round(cor_matrix[i, ncol(cor_matrix)], 3), "\n")
}

strong <- c("Glucose", "BMI", "DiabetesPedigreeFunction")
weak <- c("BloodPressure", "SkinThickness")

# Loop through all combinations of strong vs weak
for (s in strong) {
  for (w in weak) {
    # Scatter plot
    plot(df[[s]], df[[w]],
         main = paste(s, "vs", w),
         xlab = s,
         ylab = w,
         pch = 19, col = "lightgreen")
    
    # Linear regression
    model <- lm(df[[w]] ~ df[[s]])
    abline(model, col = "blue", lwd = 2)
  }
}

#scatter plots and regression lines: strong vs weak
for (s in strong) {
  for (w in weak) {
    model <- lm(df[[w]] ~ df[[s]])
    summary_model <- summary(model)
    cat(s, "->", w, 
        "Slope:", round(summary_model$coefficients[2,1], 3),
        "R²:", round(summary_model$r.squared, 3), "\n")
  }
}


