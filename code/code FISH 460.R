library(readr)
library(dplyr)
library(ggplot2)


Raw_Crab_Data <- read_csv("~/Raw Crab Data.csv")


slopes <- Raw_Crab_Data %>%
  group_by(Treatment, CrabID, Week) %>%
  summarise(
    slope = coef(lm(RFU_wt ~ Time_min))[2]
  ) %>%
  ungroup()


summary_stats <- slopes %>%
  group_by(Treatment) %>%
  summarise(
    mean_slope = mean(slope),
    sd_slope = sd(slope),
    n = n()
  )

print(summary_stats)
ggplot() +
  geom_bar(data = summary_stats, aes(x = Treatment, y = mean_slope), stat = "identity", fill = "lightblue", alpha = 0.7) +
  geom_errorbar(data = summary_stats, aes(x = Treatment, ymin = mean_slope - sd_slope, ymax = mean_slope + sd_slope), width = 0.2) +
  geom_jitter(data = slopes, aes(x = Treatment, y = slope), width = 0.1, color = "navy", size = 3) +
  labs(
    y = "Respiration slope [(RFU/g)/min]",
    title = "Crab Respiration Rates by Treatment"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))


#salinity
salinity_data <- data.frame(
  Time = rep(c(0, 1), 4),  # 0 = start of week 1, 1 = end of week 1 or week 2
  Treatment = rep(c("Crowded Week 1", "Crowded Week 2", "Uncrowded", "Uncrowded + Crowded Water"), each = 2),
  Salinity = c(
    34, 40,    # Crowded Week 1: start 34 → 40 at week 1
    34, 41,    # Crowded Week 2: start 34 → 41 at week 2
    34, 34,    # Uncrowded: flat at 34
    36, 36     # Uncrowded + Crowded Water: flat at 36 (starts at 36)
  )
)



