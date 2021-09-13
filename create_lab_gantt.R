#! Based on https://www.r-bloggers.com/2020/03/using-r-simple-gantt-chart-with-ggplot2/
#! Inspiation Janet Hill https://research-groups.usask.ca/hilllab/index.php

library(tidyverse)
library(lubridate)

# Read in people information
LabGantt <- read_csv("LabGantt.csv")

LabGantt <- LabGantt |> 
  mutate(
    # When no end date, change to today's date
    # End = if_else(is.na(End), Sys.Date(), End),
    End = if_else(is.na(End), today(), End),
    # Calculate total number of days spent in lab
    Days = End - Start,
    # Set factor level to order the Position on the plot
    Positon = factor(Position),
    # Convert names to factors and order wrt start date
    Name = fct_reorder(Name, Start, min, .desc= TRUE),
    ) |> 
  arrange(Start, desc(Days))

# Plot----
LabGantt |> 
  ggplot() + 
  geom_linerange(aes(xmin = Start,
                     xmax = End,
                     y = Name,
                     colour = Position),
                 size = 5) +
  scale_colour_manual(values = c("#2D708EFF", "#DCE319FF", "#808080", 
                                 "#20A387FF", "#482677FF")) +
  theme_bw() +
  scale_x_date() +
  theme(panel.grid = element_blank()) +
  geom_vline(xintercept=as.numeric(ymd("2020-01-01", "2021-01-01", "2022-01-01")), linetype="dotted") +
  xlab("") + ylab("") +
  ggtitle("Dimension Reduction-UManitoba")

ggsave("LabGantt.png", width = 6, height = 3)
