data <- read.csv("data.csv")
View(data)


data <- data[-c(1:8), -c(1:18)]



nrow(data)
ncol(data)


colnames(data) <- c("age", "gender", "group1_logo_creativity", "group1_logo_improvement", "group2_logo_creativity", "group2_logo_improvement", "group3_logo_creativity", "group3_logo_improvement", "group2_password_creativity", "group2_password_improvement", "group3_password_creativity", "group3_password_improvement", "group1_password_creativity", "group1_password_improvement", "group3_space_creativity", "group3_space_improvement", "group1_space_creativity", "group1_space_improvement", "group2_space_creativity", "group2_space_improvement")


head(data)




ncol(data)

data[, 3:20] <- lapply(data[, 3:20], as.numeric)


sum(is.na(data))


View(data)


head(data)



data$group1_innovation <- rowMeans(data[, c(
    "group1_logo_creativity", "group1_logo_improvement",
    "group1_password_creativity", "group1_password_improvement",
    "group1_space_creativity", "group1_space_improvement"
)])

data$group2_innovation <- rowMeans(data[, c(
    "group2_logo_creativity", "group2_logo_improvement",
    "group2_password_creativity", "group2_password_improvement",
    "group2_space_creativity", "group2_space_improvement"
)])

data$group3_innovation <- rowMeans(data[, c(
    "group3_logo_creativity", "group3_logo_improvement",
    "group3_password_creativity", "group3_password_improvement",
    "group3_space_creativity", "group3_space_improvement"
)])


round(data$group1_innovation, 3)
round(data$group2_innovation, 3)
round(data$group3_innovation, 3)





t_test_group1_group2 <- t.test(data$group1_innovation, data$group2_innovation)


t_test_group1_group3 <- t.test(data$group1_innovation, data$group3_innovation)


t_test_group2_group3 <- t.test(data$group2_innovation, data$group3_innovation)

# Group 1 – Group 2
# The t-statistic is negative, indicating that the mean innovation score for Group 1 is slightly lower than Group 2.
# The p-value (0.6478) is greater than 0.05, suggesting that there is no statistically significant difference between the mean innovation scores of Group 1 and Group 2. Therefore, we do not reject the null hypothesis.

# Group 1 - Group 3
# The t-statistic is negative, indicating that the mean innovation score for Group 1 is slightly lower than Group 3. The p-value (0.4099) is greater than 0.05, indicating no statistically significant difference between the mean innovation scores of Group 1 and Group 3. Therefore, we do not reject the null hypothesis.
# Group 2 - Group 3
# The t-statistic is negative, indicating that the mean innovation score for Group 2 is slightly lower than Group 3. The p-value (0.6801) is greater than 0.05, suggesting no statistically significant difference between the mean innovation scores of Group 2 and Group 3. Therefore, you do not reject the null hypothesis.



print(t_test_group1_group2)
print(t_test_group1_group3)
print(t_test_group2_group3)


mean(data$group1_innovation)
sd(data$group1_innovation)

mean(data$group2_innovation)
sd(data$group2_innovation)


mean(data$group3_innovation)
sd(data$group3_innovation)





innovation_data <- data.frame(
    innovation = c(data$group1_innovation, data$group2_innovation, data$group3_innovation),
    group = rep(c("Group1", "Group2", "Group3"), each = nrow(data))
)

# Perform one-way ANOVA
anova_result <- aov(innovation ~ group, data = innovation_data)

# Print the ANOVA summary
print(summary(anova_result))


head(data)

lm <- lm(innovation ~ group, data = data)


# Descriptive Analysis


innovation_levels <- list(
    data$group1_innovation,
    data$group2_innovation,
    data$group3_innovation
)


boxplot(innovation_levels,
    names = c("Group 1", "Group 2", "Group 3"),
    main = "Innovation Levels by Group",
    xlab = "Group",
    ylab = "Innovation Level",
    col = c("skyblue", "lightgreen", "lightcoral")
)
legend("bottomleft", legend = c("Female Heavy", "Balanced", "Male Heavy"), fill = c("skyblue", "lightgreen", "lightcoral"))


shapiro.test(data$group1_innovation)
shapiro.test(data$group2_innovation)
shapiro.test(data$group3_innovation)



hist_data <- hist(data$group1_innovation, probability = TRUE, main = "Histogram of group 1 Innovation Level (PDF)", xlab = "Group 1 Innovation level")

mean_value <- mean(data$group1_innovation)
sd_value <- sd(data$group1_innovation)

x <- seq(min(data$group1_innovation), max(data$group1_innovation), length = 100)

y <- dnorm(x, mean = mean_value, sd = sd_value)

lines(x, y, col = "darkorange", lwd = 2, lty = 2)

legend("topright", legend = "Normal Distribution", col = "darkorange", lwd = 2, lty = 2)


# Group 2
hist(data$group2_innovation, probability = TRUE, main = "Histogram of group 2 Innovation Level (PDF)", xlab = "Group 2 Innovation level")
mv <- mean(data$group2_innovation)
sd <- sd(data$group2_innovation)

x <- seq(min(data$group2_innovation), max(data$group2_innovation), length = 100)

y <- dnorm(x, mv, sd)

lines(x, y, col = "darkorange", lwd = 2, lty = 2)
legend("topright", legend = "Normal Distribution", col = "darkorange", lwd = 2, lty = 2)


# Group 3
hist(data$group3_innovation, probability = TRUE, main = "Histogram of group 3 Innovation Level (PDF)", xlab = "Group 3 Innovation level")
mv <- mean(data$group3_innovation)
sd <- sd(data$group3_innovation)
x <- seq(min(data$group3_innovation), max(data$group3_innovation), length = 100)
y <- dnorm(x, mv, sd)

lines(x, y, col = "darkorange", lwd = 2, lty = 2)
legend("topright", legend = "Normal Distribution", col = "darkorange", lwd = 2, lty = 2)



mean(data$group1_innovation)
#  5.576812
sd(data$group1_innovation)

# 1.697597

mean(data$group2_innovation)
# 5.802899

sd(data$group2_innovation)
# 1.635594

mean(data$group3_innovation)
# 6.021014

sd(data$group3_innovation)
# 1.916477





taskone <- read.csv("taskone_review.csv")


taskone <- taskone[-c(1:4), -c(1:18)]
View(taskone)



taskone <- taskone[-c(20:23), ]

taskone$group <- as.numeric(ifelse(taskone$group == "", 2, taskone$group))
print(taskone$group)


taskone[taskone == ""] <- "F3"

taskone$group[taskone$label == "F3"] <- 1

no <- data.frame(
    group = 2,
    label = "M4",
    password_communi_1 = 6,
    password_innovation_1 = 5
)

taskone <- rbind(taskone, no)

View(taskone)



taskone$password_communi_1 <- as.numeric(taskone$password_communi_1)
taskone$password_innovation_1 <- as.numeric(taskone$password_innovation_1)

taskone$group <- factor(taskone$group)



boxplot(taskone$password_innovation_1 ~ taskone$group,
    main = "Innovation Scores by Group", xlab = "Group", ylab = "Innovation Score", col = c("slateblue", "green", "orange")
)


# Convert 'password_innovation_1' to numeric
taskone$password_innovation_1 <- as.numeric(taskone$password_innovation_1)

# Convert 'group' to a factor
taskone$group <- factor(taskone$group)

# Boxplot for innovation scores by group
boxplot(password_communi_1 ~ group,
    data = taskone,
    main = "Innovation Scores by Group 1", xlab = "Group", ylab = "Innovation Score", col = c("slateblue", "green", "orange")
)



library(dplyr)

taskone <- taskone %>%
    mutate(
        overall_satisfaction = rowMeans(select(., c(password_communi_1, password_innovation_1)))
    )



View(taskone)

plot(overall_satisfaction ~ group, data = taskone, main = "Overall Satisfaction Scores by Group 1", xlab = "Group", ylab = "Innovation Score", col = c("slateblue", "green", "orange"))
legend("bottomright", legend = c("female dominated group", "balanced group", "male dominated group"), fill = c("slateblue", "green", "orange"))



taskone$overall_satisfaction <- as.numeric(taskone$overall_satisfaction)


t_test_task1_task2 <- t.test(
    taskone$overall_satisfaction[taskone$group == 1],
    taskone$overall_satisfaction[taskone$group == 2]
)

t_test_task1_task3 <- t.test(
    taskone$overall_satisfaction[taskone$group == 1],
    taskone$overall_satisfaction[taskone$group == 3]
)

t_test_task2_task3 <- t.test(
    taskone$overall_satisfaction[taskone$group == 2],
    taskone$overall_satisfaction[taskone$group == 3]
)


print(t_test_task1_task2)
print(t_test_task1_task3)
print(t_test_task2_task3)

lm_taskone <- lm(overall_satisfaction ~ group, data = taskone)
summary(lm_taskone)


group1_taskone <- subset(taskone, group == 1)
group2_taskone <- subset(taskone, group == 2)
group3_taskone <- subset(taskone, group == 3)


perform_t_test <- function(variable, group1, group2) {
    t_test_result <- t.test(group1[[variable]], group2[[variable]])
    return(t_test_result)
}


t_test_communication_1 <- perform_t_test("password_communi_1", group1_taskone, group2_taskone)
t_test_innovation_1 <- perform_t_test("password_innovation_1", group1_taskone, group2_taskone)
t_test_overall_satisfaction <- perform_t_test("overall_satisfaction", group1_taskone, group2_taskone)


print(t_test_communication_1)
print(t_test_innovation_1)
print(t_test_overall_satisfaction)



summary_tone <- taskone %>%
    group_by(group) %>%
    summarise(
        mean_overallsatisfaction = mean(overall_satisfaction),
        mean_communication = mean(password_communi_1),
        mean_innovation = mean(password_innovation_1)
    )


summary_tone


library(ggplot2)

# Grouped bar plot
ggplot(summary_tone_long, aes(x = group, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
        title = "Mean Values by Group",
        y = "Mean Value",
        x = "Group"
    ) +
    theme_minimal()




histogram_overall_satisfaction <- hist(taskone$overall_satisfaction, breaks = seq(1, 7, 1), main = "Histogram of overall satisfaction for task one", xlab = "Satisfaction score", ylab = "Frequency of each score")


histogram_communication <- hist(taskone$password_communi_1, breaks = seq(1, 7, 1), main = "Histogram of participant communication for task one", xlab = "Satisfaction score", ylab = "Frequency of each score")


histogram_innovation <- hist(taskone$password_innovation_1, breaks = seq(1, 7, 1), main = "Histogram of participants' innovation level for task one", xlab = "Satisfaction score", ylab = "Frequency of each score")







# TASK TWO
tasktwo <- read.csv("tasktwo_review.csv")

tasktwo <- tasktwo[-c(1:4), -c(1:18)]

tasktwo$group[tasktwo$group == ""] <- "GRP 3"

tasktwo <- tasktwo[-c(15), ]


boxplot(as.numeric(agora_communication_1) ~ group,
    data = tasktwo,
    main = "Innovation Scores by Group", xlab = "Group", ylab = "Innovation Score", col = c("slateblue", "green", "orange")
)


boxplot(as.numeric(agora_innovation_1) ~ group,
    data = tasktwo,
    main = "Innovation Scores by Group", xlab = "Group", ylab = "Innovation Score", col = c("slateblue", "green", "orange")
)


tasktwo$agora_communication_1 <- as.numeric(tasktwo$agora_communication_1)
tasktwo$agora_innovation_1 <- as.numeric(tasktwo$agora_innovation_1)

tasktwo <- tasktwo %>%
    mutate(
        overall_satisfaction = rowMeans(select(., c(agora_communication_1, agora_innovation_1)))
    )


boxplot(overall_satisfaction ~ group, data = tasktwo, main = "Overall Satisfaction Scores by Group Task 2", xlab = "Group", ylab = "Satisfaction Score", col = c("slateblue", "green", "orange"))

legend("bottomright", legend = c("female dominated group", "balanced group", "male dominated group"), fill = c("slateblue", "green", "orange"))


histogram_overall_satisfaction <- hist(tasktwo$overall_satisfaction, breaks = seq(1, 7, 1), main = "Histogram of overall satisfaction for task two", xlab = "Satisfaction score", ylab = "Frequency of each score")


histogram_communication <- hist(tasktwo$agora_communication_1, breaks = seq(1, 7, 1), main = "Histogram of participant communication for task two", xlab = "Satisfaction score", ylab = "Frequency of each score")


histogram_innovation <- hist(tasktwo$agora_innovation_1, breaks = seq(1, 7, 1), main = "Histogram of participants' innovation level for task two", xlab = "Satisfaction score", ylab = "Frequency of each score")



summary_ttwo <- tasktwo %>%
    group_by(group) %>%
    summarise(
        mean_overallsatisfaction = mean(overall_satisfaction),
        mean_communication = mean(agora_communication_1),
        mean_innovation = mean(agora_innovation_1)
    )


summary_ttwo




nrow(tasktwo)
View(tasktwo)





# TASK THREE

taskthree <- read.csv("taskthree_review.csv")
taskthree <- taskthree[-c(1:4), -c(1:18)]

boxplot(password_innovation_1 ~ group,
    data = taskthree,
    main = "Innovation Scores by Group", xlab = "Group", ylab = "Innovation Score", col = taskone$group
)


colnames(taskthree) <- c("label", "group", "logo_communication", "logo_innovation")


taskthree$logo_innovation[taskthree$logo_innovation == ""] <- "7"

taskthree$logo_communication <- as.numeric(taskthree$logo_communication)
taskthree$logo_innovation <- as.numeric(taskthree$logo_innovation)


taskthree$overall_satisfaction <- rowMeans(taskthree[, c("logo_communication", "logo_innovation")], na.rm = TRUE)


histogram_overall_satisfaction <- hist(taskthree$overall_satisfaction, breaks = seq(1, 7, 1), main = "Histogram of overall satisfaction for task three", xlab = "Satisfaction score", ylab = "Frequency of each score", col = "honeydew")


histogram_communication <- hist(taskthree$logo_communication, breaks = seq(1, 7, 1), main = "Histogram of participant communication for task three", xlab = "Satisfaction score", ylab = "Frequency of each score", col = "honeydew")


histogram_innovation <- hist(taskthree$logo_innovation, breaks = seq(1, 7, 1), main = "Histogram of participants' innovation level for task three", xlab = "Satisfaction score", ylab = "Frequency of each score", col = "honeydew")

View(taskthree)


summary_tthree <- taskthree %>%
    group_by(group) %>%
    summarise(
        mean_overallsatisfaction = mean(overall_satisfaction),
        mean_communication = mean(logo_communication),
        mean_innovation = mean(logo_innovation)
    )


summary_tthree

boxplot(overall_satisfaction ~ group, data = taskthree, main = "Overall Satisfaction Scores by Group Task 3", xlab = "Group", ylab = "Satisfaction Score", col = c("slateblue", "green", "orange"))

legend("bottomright", legend = c("female dominated group", "balanced group", "male dominated group"), fill = c("slateblue", "green", "orange"))
