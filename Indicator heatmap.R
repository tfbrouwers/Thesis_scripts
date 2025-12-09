library(tidyverse)
library(readxl)
setwd("E:/Msc/Thesis/R")

df3 <- read_excel("indicator species used.xlsx")

df_long <- df3 %>%
  pivot_longer(
    cols = c(Moehne, `Wester 1`, `Wester 2`),
    names_to = "site",
    values_to = "status"
  )

status_cols <- c(
  "confident" = "green4",
  "not_confident" = "#ffb700",
  "absent" = "grey30"
)

df_long <- df_long %>%
  mutate(site = fct_relevel(site, "W2", "W1", "M"))

plot_3 <- ggplot(df_long, aes(x = site, y = `Indicator species`, fill = status)) +
  geom_tile(color = "white") +
  facet_grid(
    rows = vars(community),
    scales = "free_y",
    space = "free_y"
  ) +
  scale_fill_manual(values = status_cols, na.value = "grey30") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(face = "italic"),
    panel.grid = element_blank()
  ) +
  labs(
    x = "Study site",
    y = "Indicator species",
    fill = " "
  ) 

plot_3
ggsave("E:/Msc/Thesis/Data/Indicator species/indicators.jpg", plot = plot_3, units = "cm", dpi = 300)
