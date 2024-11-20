## ------------------------------------------------------------------------
##
## Script name: 05_distance.R
## Purpose: Average pairwise distance by cohort
## Author: Yanwen Wang
## Date Created: 2024-11-20
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

birthy_list <- id_cluster_df %>%
  arrange(birthy) %>%
  pull(birthy) %>%
  unique()

dist_list <- c()

for (i in birthy_list) {
  n <- id_cluster_df %>%
    filter(birthy == i) %>%
    pull(n)
  m <- dist_df %>%
    select(n) %>%
    rowid_to_column() %>%
    filter(rowid %in% n) %>%
    unlist() %>%
    mean()
  dist_list <- c(dist_list, m)
}

distance_plt <- data.frame(
  cohort = birthy_list,
  dist = dist_list
) %>%
  ggplot(aes(x = cohort, y = dist)) +
  geom_point() +
  geom_smooth(span = 2) +
  scale_x_continuous(breaks = seq(1920, 1980, 5)) +
  labs(
    title = "",
    x = "",
    y = "Average Distance"
  ) +
  theme_classic()
