# Cross tab
ctab <- function(df, col1, col2) {
  df <- df %>%
    tabyl({{col1}}, {{col2}}) %>%
    adorn_percentages("col") %>%
    adorn_pct_formatting(digits = 2) %>%
    adorn_ns() %>%
    as.data.frame()
  df[1] <- paste(deparse(substitute(col1)), df[[1]], sep = "")
  df %>%
    rename(variable = {{col1}})
}
