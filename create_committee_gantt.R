#! Based on https://www.r-bloggers.com/2020/03/using-r-simple-gantt-chart-with-ggplot2/
#! Inspiation Janet Hill https://research-groups.usask.ca/hilllab/index.php

library(tidyverse)
library(lubridate)

CommitteeGantt <- read_csv("CommitteeGantt.csv")
CommitteeGantt$End[is.na(CommitteeGantt$End)] <- Sys.Date()
CommitteeGantt$Days <- CommitteeGantt$End - CommitteeGantt$Start

CommitteeGantt <- arrange(CommitteeGantt, Start, desc(Days))
CommitteeGantt$Name <- factor(CommitteeGantt$Name, levels = rev(unique(CommitteeGantt$Name)))

CommitteeGantt$Department <- as.factor(CommitteeGantt$Department)

plot_gantt_ctee <- qplot(xmin = Start,
                    xmax = End,
                    y = Name,
                    colour = Department,
                    geom = "linerange",
                    data = CommitteeGantt,
                    size = I(5)) +
  #scale_colour_viridis_d() +
  scale_colour_manual(values = c("#042333df", "#403891ff", "#a65c85ff", "#f68f46ff", "#efe350ff")) +
  theme_bw() +
  scale_x_date() +
  theme(panel.grid = element_blank()) +
  geom_vline(xintercept=as.numeric(ymd("2019-01-01","2020-01-01", "2021-01-01")), linetype="dotted") +
  xlab("") +
  ylab("") +
  ggtitle("Committees")

