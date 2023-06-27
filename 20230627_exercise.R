install.packages("tidyverse")
install.packages("emmeans")
install.packages("car")
install.packages("mice")

library(tidyverse)
library(emmeans)
library(car)

#exercise 1

setwd("C:\\Users\\gckc1\\Google Drive\\Post doc\\Presentations\\2023 Griffith R workshop")

cambodia_data <- read.csv("KHH2013_Public_Use.csv")

cambodia_small <- cambodia_data %>%
  select(Q1, Q2, Q6, Q20, Q22, Q23, Q24, Q26, Q27, Q29, Q35, Q49, Q50)

summary(cambodia_small)

#exercise 2

cambodia_no_age_sex <- cambodia_small %>%
  filter(is.na(Q1) | is.na(Q2))

summary(cambodia_no_age_sex)

cambodia_1217 <- cambodia_small %>%
  filter(Q1 >= 2 & Q1 <= 7)

summary(cambodia_1217)

table(cambodia_1217$Q2)

prop.table(table(cambodia_1217$Q24))

prop.table(xtabs(~ Q2 + Q24, data = cambodia_1217), margin = 1)

cambodia_1217 %>%
  group_by(Q2) %>%
  summarize(mean_loneliness = mean(Q22, na.rm = TRUE),
            sd_loneliness = sd(Q22, na.rm = TRUE))

cambodia_1217$age <- cambodia_1217$Q1 + 10

#Exercise 3

cambodia_1217 <- cambodia_1217 %>%
  mutate(sex = case_when(
    Q2 == 1 ~ 1,
    Q2 == 2 ~ 0, 
    Q2 == NA ~ NA
  ))

cambodia_1217$sex <- factor(cambodia_1217$sex, levels =  c("0","1"),
                            labels = c("Female","Male"))

cam.res.1 <- lm(Q26 ~ sex, data = cambodia_1217)
summary(cam.res.1)

cambodia_1217 <- cambodia_1217 %>%
  mutate(bullied = case_when(
    Q20 == 1 ~ 0,
    Q20 == 2 ~ 1,
    Q20 > 2 ~ 2,
    Q20 == NA ~ NA
  ))

cambodia_1217$bullied <- factor(cambodia_1217$bullied, levels = c("0","1","2"),
                                labels = c("0 days", "1 or 2 days", "3 or more days"))

cam.res.2 <- lm(Q26 ~ bullied, data = cambodia_1217)
summary(cam.res.2)

emm_cam.res.2 <- emmeans(cam.res.2, pairwise ~ bullied, level = 0.95)
summary(emm_cam.res.2)

cam.res.3 <- lm(Q26 ~ age + sex + bullied + Q35, data = cambodia_1217)
summary(cam.res.3)

cam.res.3.std <- data.frame("residuals" = rstandard(cam.res.3))
cam.res.3.std %>%
  ggplot(aes(x = residuals)) +
  geom_histogram(color = "white")

cam.res.3.std %>%
  ggplot(aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line()

outlierTest(cam.res.3)
infIndexPlot(cam.res.3)
residualPlots(cam.res.3)

cam.res.4 <- lm(Q26 ~ age + bullied + sex*Q35, data = cambodia_1217)
summary(cam.res.4)

m_Q35 <- mean(cambodia_1217$Q35, na.rm = TRUE)
sd_Q35 <- sd(cambodia_1217$Q35, na.rm = TRUE)

emm_cam.res.4 <- emmeans(cam.res.4, pairwise ~ Q35*sex,
                         cov.keep = 3,
                         at = list(Q35 = c(m_Q35-sd_Q35,
                                           m_Q35,
                                           m_Q35+sd_Q35)), level = 0.95)

summary(emm_cam.res.4)

cam.4.mod <- emtrends(cam.res.4, pairwise ~ sex, var = "Q35", level = 0.95)
summary(cam.4.mod)

emmip(cam.res.4, sex ~ Q35,
      cov.keep = 3, at = list(
        Q35 = c(m_Q35-sd_Q35,
                m_Q35,
                m_Q35+sd_Q35)),
      CIs = TRUE, level = 0.95, position = "jitter")

#Exercise 4
cambodia_1217 <- cambodia_1217 %>%
  mutate(suicide_attempt = case_when(
    Q26 == 1 ~ 0,
    Q26 > 1 ~ 1,
    Q26 == NA ~ NA
  ))

cambodia_1217 <- cambodia_1217 %>%
  mutate(alcohol = case_when(
    Q35 == 1 ~ 0,
    Q35 > 1 ~ 1,
    Q35 == NA ~ NA
  ))

cambodia_1217$suicide_attempt <- factor(cambodia_1217$suicide_attempt, levels =  c("0","1"),
                            labels = c("no suicide attempt","one or more attempt"))

cambodia_1217$alcohol <- factor(cambodia_1217$alcohol, levels =  c("0","1"),
                                        labels = c("no alcohol use","one or more attempt"))

cam.res.5 <- glm(suicide_attempt ~ age + sex + bullied  + alcohol, family = binomial, data = cambodia_1217)
summary(cam.res.5)

cbind(coef(cam.res.5), confint(cam.res.5, level = 0.95))

exp(cbind(coef(cam.res.5), confint(cam.res.5, level = 0.95)))

cam.res.6 <- glm(suicide_attempt ~ age + bullied + sex*alcohol, family = binomial, data = cambodia_1217)
summary(cam.res.6)