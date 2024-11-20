## ------------------------------------------------------------------------
##
## Script name: 03_describe.R
## Purpose: Descriptive statistics
## Author: Yanwen Wang
## Date Created: 2024-11-20
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# 1 Descriptive statistics --------------------------------------------------

# Select and construct variables of interest
childless_df <- seq_childless_1540 %>%
  select(ID, complexity, 303:ncol(seq_childless_1540)) %>%
  select(-pedu, -sibling, -migrant) %>%
  # Get province code from family roster in 2018
  left_join(
    cfps2018_familyroster %>% select(pid, fid_provcd18),
    by = c("ID" = "pid")
  ) %>%
  rename(province = fid_provcd18) %>%
  mutate(
    region = case_when(
      province >= 11 & province <= 15 ~ "HuaBei",
      province >= 21 & province <= 23 ~ "DongBei",
      province >= 31 & province <= 37 ~ "HuaDong",
      province >= 41 & province <= 46 ~ "ZhongNan",
      province >= 50 & province <= 54 ~ "XiNan",
      province >= 61 & province <= 65 ~ "XiBei"
    ),
    region = factor(region)
  ) %>%
  mutate(gender = factor(gender)) %>%
  mutate(
    edu = case_when(
      edu <= 3 ~ 0,
      edu >= 4 ~ 1
    ),
    edu = factor(edu)
  ) %>%
  mutate(
    hukou = case_when(
      hukou == 1 ~ 0,
      hukou == 3 ~ 1
    ),
    hukou = factor(hukou)
  ) %>%
  mutate(
    n_cohort = case_when(
      birthy < 1949 ~ "<1949",
      birthy >= 1949 & birthy < 1966 ~ "1949-1966",
      birthy >= 1966 & birthy < 1978 ~ "1966-1978",
      birthy >= 1978 ~ ">=1978"
    )
  ) %>%
  mutate(
    n_cohort = factor(
      n_cohort,
      levels = c("<1949", "1949-1966", "1966-1978", ">=1978")
    )
  ) %>%
  mutate(
    age = 2018 - birthy,
    agesq = age * age,
    birthysq = birthy * birthy
  ) %>%
  na.omit()

# Distribution of clusters
tabyl(childless_df, cluster5) %>% adorn_pct_formatting(2)

# Categorical variables
categorical <- bind_rows(
  ctab(childless_df, gender, cluster5),
  ctab(childless_df, edu, cluster5),
  ctab(childless_df, hukou, cluster5)
) %>%
  select(
    variable,
    `never married`,
    `married late`,
    `married early`,
    `married ontime`,
    `unpartnered`
  )

# Continuous variables
continuous <- childless_df %>%
  select(
    cluster5, age, n_marriage, complexity
  ) %>%
  group_by(cluster5) %>%
  summarise_if(is.numeric, list(mean, sd), na.rm = TRUE) %>%
  pivot_longer(
    cols = -cluster5,
    names_to = c("variable", "fn"),
    names_sep = -4,
    values_to = "value"
  ) %>%
  mutate(
    fn = case_when(
      fn == "_fn1" ~ "mean",
      fn == "_fn2" ~ "sd"
    )
  )  %>%
  pivot_wider(
    id_cols = c(cluster5, variable),
    names_from = fn,
    values_from = value
  ) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(m_sd = paste0(mean, " (", sd, ")")) %>%
  pivot_wider(
    id_cols = variable,
    names_from = cluster5,
    values_from = m_sd
  ) %>%
  select(
    variable,
    `never married`,
    `married late`,
    `married early`,
    `married ontime`,
    `unpartnered`
  )

bind_rows(categorical, continuous)

# Average distance withiin each cluster
dist_df <- as.data.frame(omdist)

id_cluster_df <- seq_childless_1540 %>%
  select(ID, cluster5, birthy) %>%
  mutate(n = seq(1, 797))

id_cluster1 <- id_cluster_df %>%
  filter(cluster5 == "never married") %>%
  pull(n)

id_cluster2 <- id_cluster_df %>%
  filter(cluster5 == "married late") %>%
  pull(n)

id_cluster3 <- id_cluster_df %>%
  filter(cluster5 == "married early") %>%
  pull(n)

id_cluster4 <- id_cluster_df %>%
  filter(cluster5 == "married ontime") %>%
  pull(n)

id_cluster5 <- id_cluster_df %>%
  filter(cluster5 == "unpartnered") %>%
  pull(n)

dist_df %>%
  select(id_cluster1) %>% # Change to id_cluster1, 2, 3, 4, 5
  rowid_to_column() %>%
  filter(rowid %in% id_cluster1) %>%
  unlist() %>%
  mean()

# Average age into first marriage by cluster
seq_childless_1540 %>%
  # Change to other clusters to get the average age into first marriage
  filter(cluster5 == "married late") %>%
  select(ID, 2:302) %>%
  pivot_longer(
    cols = -ID,
    names_to = "month",
    values_to = "event"
  ) %>%
  group_by(ID) %>%
  filter(event == "first marriage") %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(age = as.numeric(month) / 12 + 15) %>%
  pull(age) %>%
  mean()

# Ever-cohabited by cluster
id_ever_cohabited <- seq_childless_1540 %>%
  select(ID, `0`:`300`) %>%
  pivot_longer(
    cols = -ID,
    names_to = "month",
    values_to = "status"
  ) %>%
  group_by(ID) %>%
  filter(any(status == "cohabit")) %>%
  ungroup() %>%
  pull(ID) %>%
  unique()

seq_childless_1540 %>%
  select(ID, cluster5) %>%
  mutate(cohabit = ifelse(ID %in% id_ever_cohabited, 1, 0)) %>%
  group_by(cluster5) %>%
  summarise(cohabit_percent = round(mean(cohabit) * 100, 2),
            cohabit_sd = round(sd(cohabit) * 100, 2)) %>%
  ungroup()

# Ever divorced by cluster
id_ever_divorced <- seq_childless_1540 %>%
  select(ID, `0`:`300`) %>%
  pivot_longer(
    cols = -ID,
    names_to = "month",
    values_to = "status"
  ) %>%
  group_by(ID) %>%
  filter(any(status == "unpartnered")) %>%
  ungroup() %>%
  pull(ID) %>%
  unique()

seq_childless_1540 %>%
  select(ID, cluster5) %>%
  mutate(divorce = ifelse(ID %in% id_ever_divorced, 1, 0)) %>%
  group_by(cluster5) %>%
  summarise(divorce_percent = round(mean(divorce) * 100, 2),
            divorce_sd = round(sd(divorce) * 100, 2)) %>%
  ungroup()
