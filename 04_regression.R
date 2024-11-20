## ------------------------------------------------------------------------
##
## Script name: 04_regression.R
## Purpose: Multinomial and ordinary regression
## Author: Yanwen Wang
## Date Created: 2024-11-20
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# 1 Multinomial logistic regression ---------------------------------------

# 1.1 Fit models ----------------------------------------------------------

mods <- list(
  mod1 <- multinom(
    cluster5 ~ gender + age + I(age^2) + edu + hukou + province,
    data = childless_df
  ),
  mod2 <- multinom(
    cluster5 ~ gender * age + gender * I(age^2) + edu + hukou + province,
    data = childless_df
  ),
  mod3 <- multinom(
    cluster5 ~ gender * edu + age + I(age^2) + hukou + province,
    data = childless_df
  ),
  mod4 <- multinom(
    cluster5 ~ gender * hukou + age + I(age^2) + edu + province,
    data = childless_df
  )
)

# 1.2 Marginal effects ----------------------------------------------------

# Gender
predictions <- ggemmeans(mod1, terms = c("gender"))

gender_plt <- as.data.frame(predictions) %>%
  mutate(
    x = case_when(
      x == 0 ~ "female",
      x == 1 ~ "male"
    )
  ) %>%
  ggplot(aes(x = x, y = predicted, group = 1)) +
  geom_line() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(response.level ~ ., scales = "free") +
  labs(
    title = "",
    y = "Probability",
    x = ""
  ) +
  theme_bw()

# Age or cohort
predictions <- ggemmeans(mod2, terms = c("age[all]", "gender"))

cohort_plt <- as.data.frame(predictions) %>%
  mutate(
    Gender = case_when(
      group == 0 ~ "female",
      group == 1 ~ "male"
    )
  ) %>%
  mutate(x = 2018 - x) %>%
  ggplot(aes(x = x, y = predicted, group = Gender, color = Gender)) +
  geom_line() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1) +
  scale_x_continuous(breaks = seq(1920, 1980, 5)) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(response.level ~ ., scales = "free") +
  labs(
    title = "",
    y = "Probability",
    x = ""
  ) +
  theme_bw() +
  theme(legend.position = "none")

# Education
predictions <- ggemmeans(mod3, terms = c("edu[all]", "gender"))

edu_plt <- as.data.frame(predictions) %>%
  mutate(
    Gender = case_when(
      group == 0 ~ "female",
      group == 1 ~ "male"
    ),
    x = case_when(
      x == 0 ~ "low edu",
      x == 1 ~ "high edu"
    ),
    x = factor(x, levels = c("low edu", "high edu"))
  ) %>%
  ggplot(aes(x = x, y = predicted, group = Gender, color = Gender)) +
  geom_line() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(response.level ~ ., scales = "free") +
  labs(
    title = "",
    y = "Probability",
    x = ""
  ) +
  theme_bw()

# Hukou
predictions <- ggemmeans(mod4, terms = c("hukou[all]", "gender"))

hukou_plt <- as.data.frame(predictions) %>%
  mutate(Gender = case_when(
    group == 0 ~ "female",
    group == 1 ~ "male"
  )) %>%
  mutate(x = case_when(
    x == 0 ~ "rural",
    x == 1 ~ "urban"
  )) %>%
  ggplot(aes(x = x, y = predicted, group = Gender, color = Gender)) +
  geom_line() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(response.level ~ ., scales = "free") +
  labs(
    title = "",
    y = "Probability",
    x = ""
  ) +
  theme_bw()

# 2 Ordinary least squares regression -------------------------------------

mod <- lm(
  complexity ~ gender + birthy + I(birthy^2) + edu + hukou + province,
  data = childless_df
)