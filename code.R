# Importing the "mtcars"
data("mtcars")

# Displaying the structure of the dataset.
str(mtcars)
# Displaying the summary of each column of the dataset.
summary(mtcars)
# Printing the whole dataset.
mtcars
# Printing the head.
head(mtcars)

# Printing the head.
head(mtcars2)

# The head of automatic cars.
head(mtcars[which(mtcars$am == 0),])  # 0 for automatic
# The head of manual cars.
head(mtcars[which(mtcars$am == 1),])  # 1 for manual

############################################

# Redefining "mtcars" to get more information.
mtcars2 = within(mtcars,
                 {
                   cyl <- ordered(cyl)
                   gear <- ordered(gear)
                   carb <- ordered(gear)
                   vs <- factor(vs, labels = c("V_shaped", "Straight"))
                   am <- factor(am, labels = c("auto", "manual"))
                 })
str(mtcars2)
summary(mtcars2)

# The head of automatic cars.
head(mtcars2[which(mtcars2$am == "auto"),])  # 0 for automatic
# The head of manual cars.
head(mtcars2[which(mtcars2$am == "manual"),])  # 1 for manual

# Top 10 cars based on Displacement.
mtcars2[order(-mtcars2$disp),][0:10,]  # First Method.
head(mtcars2[order(-mtcars2$disp),], 10)  # Second Method.

# Top 10 cars based on hp (Horse Power)
mtcars2[order(-mtcars2$hp),][0:10,]  # First Method.
head(mtcars2[order(-mtcars2$hp),], 10)  # Second Method.

# Top 10 cars based on drat (Rear axle ratio)
mtcars2[order(-mtcars2$drat),][0:10,]  # First Method.
head(mtcars2[order(-mtcars2$drat),], 10)  # Second Method.

# Cars with mpg above the mean mpg
average.mpg <- mean(mtcars2$mpg)
mtcars2[which(mtcars2$mpg > average.mpg),]


# Box Plot for numeric variables.
mt.num.cols <- mtcars2[sapply(mtcars2, is.numeric)]  # Getting only the numeric columns.
plotting.boxPlot <- function(df)
{
  for (column in colnames(df))
  {
    v <- df[column]
    
    boxplot(v,
            main = paste("Normal Q-Q plot for", column),
            col = "blue")
  }
}

plotting.boxPlot(mt.num.cols)


non_numeric_cols <- mtcars[setdiff(colnames(mtcars), colnames(mt.num.cols))]
plotting.histogram <- function(df)
{
  for (column in colnames(df))
  {
    
    hist(df[[column]],
         main = paste(column, "Frequencies"),
         col = "chocolate3",
         xlab = column,
         ylab = "Frequency")
  }
}

plotting.histogram(non_numeric_cols)

# Histogram for mpg.
hist(mtcars$mpg,
     col = "chocolate3",
     xlab = "MPG",
     ylab = "Frequency",
     main = "MPG Frequencies")

# Standard deviation for Displacement
disp.mean <- mean(mtcars2$disp)
disp.sd <- sd(mtcars2$disp)
x <- sort(mtcars2$disp)
y <- dnorm(x, disp.mean, disp.sd)
plot(x, y,
     type = "o",
     xlab = "", ylab = "",
     main = "Displacement")


# box plot for mpg variable.
boxplot(mtcars2$disp,
        col = "chocolate3",
        ylab = "disp",
        main = "Distribution of disp")
summary(mtcars$disp)


# box plot for hp variable.
boxplot(mtcars2$hp,
        col = "chocolate3",
        ylab = "hp",
        main = "Distribution of hp")
summary(mtcars$hp)


# box plot for qsec variable.
boxplot(mtcars2$qsec,
        col = "chocolate3",
        ylab = "qsec",
        main = "Distribution of qsec")
summary(mtcars$qsec)

# Percentage of cars having 3.4 lbs or more.
wt.mean <- mean(mtcars2$wt)
wt.sd <- sd(mtcars2$wt)
R.V <- sort(mtcars2$wt)
prob_less_than_3.4 <- pnorm(3.4, mean = wt.mean, sd = wt.sd)
prob_more_than_3.4 <- 1 - prob_less_than_3.4

print(prob_more_than_3.4)


# Probability of getting 18 or less manual cars
manual_cars <- sum(mtcars2$am == "manual") / length(mtcars2$am)
prob_18_manual_cars <- pbinom(18, 32, manual_cars)

print(prob_18_manual_cars)


# Probability of having four or less spots
prob_4_less <- pbinom(4, 12, 1/5)

print(prob_4_less)


# Number of permutations for 3 digit ternary number.
print(3 * 3 * 3)  # Method 1
perm <- choose(3, 1) * factorial(1)  # Method 2
print(perm ** 3)

# Getting the permutations.
d1 <- rep(c(0, 1, 2), times=3)
d2 <- rep(c(0, 10, 20), each = 3)
d3 <- rep(c(0, 100, 200), each = 9)
prem <- d1 + d2 + d3
print(prem)
print(length(prem))

# Method 2
num <- 0
while(num <= 222)
{
  cat(num, " ")

  num <- num + 1
  if (num %% 10 > 2)
  {
    num <- num - (num %% 10)
    num <- num + 10
  }
  if (as.integer((num %% 100)/10) > 2)
  {
    num <- num - (num %% 100)
    num <- num + 100
  }
}

# Probability that you get 3 numbers where the minimum number is 2 and the maximum is 5
# Method 1
numerator <- (choose(3, 1) * 1/9) * (choose(2, 1) * 1/8) * (choose(1, 1) * 2/7) 
denominator <- choose(9, 3)
result <- numerator / denominator
print(result)

numerator <- (choose(4, 1) ) * (choose(3, 1) ) * (choose(2, 1) ) 
denominator <- choose(9, 3)
result <- numerator / denominator
print(result)


# Probability that you get 3 numbers where the minimum number is 2 and the maximum is 5
# Method 1
numerator <- choose(4, 3)
denominator <- choose(9, 3)
result <- numerator / denominator

print(result)


# Q-Q plot for mtcars dataset
mt.num.cols <- mtcars2[sapply(mtcars2, is.numeric)]  # Getting only the numeric columns.
plotting.Q.Q <- function(df)
{
  for (column in colnames(df))
  {
    v <- unlist(df[column])
    
    qqnorm(v,
           main = paste("Normal Q-Q plot for", column),
           col = "blue")
    qqline(v, col = "red", lwd = 2, lty = 2)
  }
}

plotting.Q.Q(mt.num.cols)
