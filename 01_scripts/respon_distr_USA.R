# Response Distribution Analysis, Team USA
# Dataset: ds_main (TISP survey data), filtered for United States (COUNTRY_CODE == "USA")

# Load libraries
library(tidyverse)
library(haven)
library(resquin)
library(visdat)

# Load & filter data
ds <- read_rds("__00_data/ds_main.rds")

# Filter for USA respondents only
ds_usa <- ds |>
  filter(COUNTRY_CODE == "USA")

#Number of USA respondents
nrow(ds_usa)

# Extract Likert-scale item batteries
# TISP questionnaire contains several multi-item Likert batteries
# Examined separately so the intra-individual statistics are interpretable within a coherent scale

# Trust in Scientists
trust_sci_usa <- ds_usa |>
  select(starts_with("TRUST_SCI_"))

# Climate Government Trust
clim_gov_usa <- ds_usa |>
  select(starts_with("CLIM_GOV_"))

# Climate Emotions
clim_emo_usa <- ds_usa |>
  select(starts_with("CLIM_EMO_"))

# Science Populism
scipop_usa <- ds_usa |>
  select(starts_with("SCIPOP_"))

# Science Information Sources
sciinfo_usa <- ds_usa |>
  select(starts_with("SCIINFO_"))

# Response distribution indicators
# resp_distributions() computes intra-individual (within-person) location and spread indicators
# including ii_mean, ii_median, ii_sd, mahal, and NA counts

# Preview output before assigning
resp_distributions(trust_sci_usa)

rd_trust_sci <- resp_distributions(trust_sci_usa)
rd_clim_gov  <- resp_distributions(clim_gov_usa)
rd_clim_emo  <- resp_distributions(clim_emo_usa)
rd_scipop    <- resp_distributions(scipop_usa)
rd_sciinfo   <- resp_distributions(sciinfo_usa)

# Descriptive summary
# Trust in Scientists (USA)
summary(rd_trust_sci)
# Climate Gov. Trust (USA)
summary(rd_clim_gov)
# Climate Emotions (USA)
summary(rd_clim_emo)
# Science Populism (USA)
summary(rd_scipop)
# Science Info Sources (USA)
summary(rd_sciinfo)

# univariate distribution plot
# histogram of each indicator, excl. NA-count columns

plot_univariate <- function(rd, title) {
  rd |>
    select(-contains("na")) |>
    pivot_longer(-id) |>
    ggplot(aes(value)) +
    geom_histogram(bins = 30) +
    facet_wrap(vars(name), scales = "free_x") +
    labs(title = title,
         subtitle = "US respondents",
         x = NULL, y = "Count") +
    theme_minimal()
}

p1 <- plot_univariate(rd_trust_sci, "Response Distribution, Trust in Scientists")
p2 <- plot_univariate(rd_clim_gov,  "Response Distribution, Climate Gov. Trust")
p3 <- plot_univariate(rd_clim_emo,  "Response Distribution, Climate Emotions")
p4 <- plot_univariate(rd_scipop,    "Response Distribution, Science Populism")
p5 <- plot_univariate(rd_sciinfo,   "Response Distribution, Science Info Sources")

out_dir <- "__02_output"

ggsave(file.path(out_dir, "p1_trust_sci.png"),    p1, width = 8, height = 5)
ggsave(file.path(out_dir, "p2_clim_gov.png"),     p2, width = 8, height = 5)
ggsave(file.path(out_dir, "p3_clim_emo.png"),     p3, width = 8, height = 5)
ggsave(file.path(out_dir, "p4_scipop.png"),       p4, width = 8, height = 5)
ggsave(file.path(out_dir, "p5_sciinfo.png"),      p5, width = 8, height = 5)

print(p1); print(p2); print(p3); print(p4); print(p5)

# bivariate scatter-plot matrix
# correlations between indicators within each battery

# Bivariate plot, Trust in Scientists
png(file.path(out_dir, "biv_trust_sci.png"), width = 800, height = 800)
rd_trust_sci |> select(-contains("na")) |> plot(main = "Trust in Scientists, USA")
dev.off()

# Biv plot, Climate Gov. Trust
png(file.path(out_dir, "biv_clim_gov.png"), width = 800, height = 800)
rd_clim_gov |> select(-contains("na")) |> plot(main = "Climate Gov. Trust, USA")
dev.off()

# in R-viewer
rd_trust_sci |> select(-contains("na")) |> plot(main = "Trust in Scientists, USA")
rd_clim_gov  |> select(-contains("na")) |> plot(main = "Climate Gov. Trust, USA")

# Straightlining check
# straightliners are respondents with ii_sd == 0 (always chose the same response)

n_straight_trust <- sum(rd_trust_sci$ii_sd == 0, na.rm = TRUE)
n_straight_clim  <- sum(rd_clim_gov$ii_sd  == 0, na.rm = TRUE)

# straightl. Trust in Scientists:
n_straight_trust
nrow(rd_trust_sci)
# straightl. climate Gov. Trust: 
n_straight_clim
nrow(rd_clim_gov)

# Mahalanobis distance, i.e. bottom 10% vs top 10%
p10 <- quantile(rd_trust_sci$mahal, 0.10, na.rm = TRUE)
p90 <- quantile(rd_trust_sci$mahal, 0.90, na.rm = TRUE)

low_mahal  <- rd_trust_sci |> filter(mahal <= p10)
high_mahal <- rd_trust_sci |> filter(mahal >= p90)

#Bottom 10% Mahalanobis distance
nrow(low_mahal)
print(summary(low_mahal))

#Top 10% Mahalanobis distance

nrow(high_mahal)
print(summary(high_mahal))

# missing values & min_valid_responses
# clim_emo_usa used because it contains missing data
visdat::vis_miss(clim_emo_usa)
clim_emo_usa |> resp_distributions() |> summary()

# lowering threshold includes more respondents but accepts more missings.
clim_emo_usa |> resp_distributions(min_valid_responses = 0.9) |> summary()
clim_emo_usa |> resp_distributions(min_valid_responses = 0.5) |> summary()
clim_emo_usa |> resp_distributions(min_valid_responses = 0.1) |> summary()
# ii_mean, ii_median are robust across thresholds, ii_sd and mahal become less stable with more tolerated missings

