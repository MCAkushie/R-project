library(tidyverse)
Melanoma <- read_csv("C:/Users/HP/Downloads/melanoma.csv")
Melanoma

Melanoma <- Melanoma %>% select(-1)
head(Melanoma)

# Mutate columns to factors
df <- Melanoma
df <- df %>%
  mutate(sex = factor(sex, levels = c(0, 1), labels = c("female", "male")),
         ulcer = factor(ulcer, levels = c(0, 1), labels = c("absent", "present")),
         status = factor(status, levels = c(1, 2, 3), labels = c("died_melanoma", "alive", "died_unrelated")))

df
view(df)
summary(df)

par(mfrow=c(2,2))

hist(df$year)
hist(df$time)
hist(df$age)
hist(df$thickness)

attach(df)
cor(time, thickness, method="pearson")
cor(time, age, method="pearson")
cor(thickness, age, method="pearson")

plot(time, thickness)
plot(time, age)
plot(thickness, age)

#Now let's build our regression model (note that it's y∼x)
my_model=lm(formula = thickness~time)
my_model=lm(formula = age~time)
my_model=lm(formula = age~thickness)

my_model
summary(my_model)
abline(my_model, col = "green", lwd = 2)

par(mfrow=c(2,2))

t_test_time_sex <- t.test(time ~ sex, data = df)
t_test_thickness_sex <- t.test(thickness ~ sex, data = df)
t_test_age_sex <- t.test(age ~ sex, data = df)

# Print the results
print(t_test_time_sex)
print(t_test_thickness_sex)
print(t_test_age_sex)


p_time <- ggplot(data = df, aes(sample = time))
p_time + stat_qq() + stat_qq_line()

p_thick <- ggplot(data = df, aes(sample = thickness))
p_thick + stat_qq() + stat_qq_line()

p_age <- ggplot(data = df, aes(sample = age))
p_age + stat_qq() + stat_qq_line()

p_time + stat_qq() + stat_qq_line() + facet_grid(. ~ sex)
p_thick + stat_qq() + stat_qq_line() + facet_grid(. ~ sex)
p_age + stat_qq() + stat_qq_line() + facet_grid(. ~ sex)
