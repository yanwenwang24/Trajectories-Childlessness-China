## ------------------------------------------------------------------------
##
## Script name: 02_sequence.R
## Purpose: Sequence analysis
## Author: Yanwen Wang
## Date Created: 2024-11-20
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# 1 Prepare data ----------------------------------------------------------

# Select childless respondents
seq_childless_1540 <- seq_marital_1540 %>%
  filter(n_children == 0) %>%
  mutate(across(2:302, ~ replace(., . == "married", "remarried")))

# Create labels for marital status
lab <- c(
  "cohabit",
  "first marriage",
  "remarried",
  "never_married",
  "unpartnered"
)

# Define sequence objects
seq_childless <- seqdef(seq_childless_1540[, 2:302],
  labels = lab
)

# Define color palettes
cpal(seq_childless) <- c("#7eb0d5", "#bd7ebe", "#ffb55a", "#b2e061", "#fd7f6f")

# Calculate entropy, turbulence, and complexity
# Calculate within-individual longitudinal entropy, turbulence, and complexity
seq_childless_1540$entropy <- as.data.frame(seqient(seq_childless))$Entropy
seq_childless_1540$turbulence <- as.data.frame(seqST(seq_childless))$Turbulence
seq_childless_1540$complexity <- as.data.frame(seqici(seq_childless))$C

# 2 Identify clusters ------------------------------------------------------

# Calculate pairwise distance by optimal matching with constant cost
omdist <- seqdist(
  seq_childless,
  method = "OMspell", sm = "CONSTANT", with.missing = TRUE
)

# Hierarchical clustering
hc_ward <- hclust(as.dist(omdist), method = "ward.D")

# Retrieve cluster membership and fit measures
clust <- as.clustrange(
  hc_ward,
  diss = omdist,
  ncluster = 10
)

# The optimal number of clusters is 5.

# 3 Plot sequence profiles -------------------------------------------------

# Name the five clusters
clust_labels <- c(
  "never married",
  "married late",
  "married early",
  "married ontime",
  "unpartnered"
)

seq_childless_1540$cluster5 <- factor(
  clust$clustering$cluster5,
  levels = c(1, 2, 3, 4, 5),
  labels = clust_labels
)

# Plot individual sequences and distribution of status by cluster
plot_individual <- ggseqiplot(
  seq_childless,
  group = seq_childless$cluster5
) +
  labs(title = "Individual Sequences by Cluster") +
  theme(legend.position = "none")

plot_distribution <- ggseqdplot(
  seq_childless,
  group = seq_childless$cluster5
) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Distribution of Status by Cluster")

# Combine plots
plot_individual / plot_distribution
