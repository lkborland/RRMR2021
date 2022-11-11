# KUD plotting - spatial stuff
library(rgdal)
library(rgeos)
library(raster)

library(tidyverse)
library(cowplot)
library(showtext)
library(sysfonts)
showtext_auto()

library(plotly)
library(hrbrthemes)

# Add fonts from Google.
font_add_google("Roboto Mono", "Roboto Mono")
font_add_google("Open Sans", "Open Sans")
font_add_google("Special Elite", "Special Elite")
# Set ggplot theme
theme_set(theme_minimal(base_family = "Roboto Mono"))




violin_BR_KUD <- ggplot(BR_ANOVA, aes(x=period, y=area, fill=period)) + 
  geom_violin(trim=TRUE)  + 
  scale_x_discrete(limits=periods.lvl, labels = KUD_labels) +
  labs(x = "Period of survey", y = "Area", title = "Black Rockfish Usage Density", fill="Period") +
  scale_fill_manual(values = noise_colors_repeat, breaks = periods.lvl, labels = KUD_labels, name = "Period")
violin_BR_KUD

violin_CR_KUD <- ggplot(CR_ANOVA, aes(x=period, y=area, fill=period)) + 
  geom_violin(trim=TRUE)  + 
  scale_x_discrete(limits=periods.lvl, labels = KUD_labels) +
  labs(x = "Period of survey", y = "Area", title = "China Rockfish Usage Density", fill="Period") +
  scale_fill_manual(values = noise_colors_repeat, breaks = periods.lvl, labels = KUD_labels, name = "Period")
violin_CR_KUD

violin_LC_KUD <- ggplot(LC_ANOVA2, aes(x=period, y=area, fill=period)) + 
  geom_violin(trim=TRUE)  + 
  scale_x_discrete(limits=periods.lvl, labels = KUD_labels) +
  labs(x = "Period of survey", y = "Area", title = "Lingcod Usage Density", fill="Period") +
  scale_fill_manual(values = noise_colors_repeat, breaks = periods.lvl, labels = KUD_labels, name = "Period")
violin_LC_KUD