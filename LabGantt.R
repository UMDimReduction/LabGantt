library(tidyverse)
library(lubridate)
library(here)
# read in people information
LabGantt <- read_csv(here("LabGantt.csv"))
CommitteeGantt <- read_csv(here("CommitteeGantt.csv"))

# When no end date, change to today's date
LabGantt$End[is.na(LabGantt$End)] <- Sys.Date()

# Calculate total number of days spent in lab
LabGantt$Days <- LabGantt$End - LabGantt$Start

# Arrange based on start date, followed by total number of days
LabGantt <- arrange(LabGantt, Start, desc(Days))
LabGantt$Name <- factor(LabGantt$Name, levels = rev(unique(LabGantt$Name)))

# Set factor level to order the Position on the plot
LabGantt$Position <- as.factor(LabGantt$Position)

# Plot
plot_gantt <- qplot(xmin = Start,
                    xmax = End,
                    y = Name,
                    colour = Position,
                    geom = "linerange",
                    data = LabGantt,
                    size = I(5)) +
  #scale_colour_viridis_d() +
  scale_colour_manual(values = c("#2D708EFF" ,  "#DCE319FF",  "#808080", "#20A387FF", "#482677FF")) +
  theme_bw() +
  scale_x_date() +
  theme(panel.grid = element_blank()) +
    geom_vline(xintercept=as.numeric(ymd("2019-01-01","2020-01-01", "2021-01-01")), linetype="dotted") +
  xlab("") +
  ylab("") +
  ggtitle("MicroStats Lab")

# Repate above for committees
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

