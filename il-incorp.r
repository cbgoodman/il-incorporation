library(tidyverse)
library(lubridate)

# read in data
incorp <- read.csv("il-incorporation.csv")
name <- incorp$name
date0 <- ymd(incorp$earliest_date)
date <- ymd(incorp$incorp_pre18720701)
date1 <- ymd(incorp$incorp_sos)

dates <- data.frame(name, date0, date, date1)
dates <- mutate(dates, reincorp = ifelse(date1>date, "Yes", "No"),
  reincorp = replace(reincorp, is.na(date), "No"))

# plot -- Earliest date of Incorporation
early <- ggplot(dates) +
  aes(x = year(date0)) +
  geom_bar(fill="#ca5268", alpha=0.85) +
  scale_x_continuous(breaks=seq(1815, 2010, by=10)) +
  # Theming
  labs(
    title="Illinois Municipal Incorporation",
    subtitle="Earliest Date of Municipal Incorporation",
    caption="Author: Chris Goodman (@cbgoodman), Data: IL Secretary of State.",
    y="Count",
    x="Year") +
  theme_minimal(base_family="Open Sans Condensed Light") +
  # light, dotted major y-grid lines only
  theme(panel.grid=element_line())+
  theme(panel.grid.major.y=element_line(color="#2b2b2b", linetype="dotted", size=0.15))+
  theme(panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  # light x-axis line only
  theme(axis.line=element_line())+
  theme(axis.line.y=element_blank())+
  theme(axis.line.x=element_blank())+
  # tick styling
  theme(axis.ticks=element_line())+
  theme(axis.ticks.x=element_blank())+
  theme(axis.ticks.y=element_blank())+
  theme(axis.ticks.length=unit(5, "pt"))+
  # x-axis labels
  theme(axis.text.x=element_text(size=10, hjust=0.95,vjust=0.2))+
  # breathing room for the plot
  theme(plot.margin=unit(rep(0.5, 4), "cm"))+
  # move the y-axis tick labels over a bit
  theme(axis.text.y=element_text(margin=margin(r=-5)))+
  theme(axis.text.x=element_text(margin=margin(r=-5)))+
  # make the plot title bold and modify the bottom margin a bit
  theme(plot.title=element_text(family="Open Sans Condensed Bold", margin=margin(b=15)))+
  # make the subtitle italic
  theme(plot.subtitle=element_text(family="Open Sans Condensed Light Italic"))+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(t=15)))
ggsave(plot=early, "incorp.png", width=10, height=6, units="in", dpi="retina")

# plot -- SOS date of Incorporation
sos <- ggplot(dates) +
  aes(x = year(date1), fill = reincorp) +
  geom_histogram(position = "stack", stat = "bin", binwidth=1, col="white", alpha=0.85, na.rm = TRUE) +
  scale_x_continuous(breaks=seq(1872, 2012, by=10)) +
  scale_fill_manual(values = c("#617A89", "#ca5268"),
    labels=c("No", "Yes")) +
  guides(fill = guide_legend(
                  title = "Is the certificate \na reincorporation?",
                  title.position = "left",
                  direction = "horizontal"
                  )) +
  # Theming
  labs(
    title="Post Cities and Villages Act of 1872 Incorporations",
    subtitle="IL Secretary of State Certificate of Incorporation Date",
    caption="Author: Chris Goodman (@cbgoodman), Data: IL Secretary of State.",
    y="Count",
    x="Year") +
  theme_minimal(base_family="Open Sans Condensed Light") +
  theme(
    legend.position = "bottom",
    legend.title.align = 0,
    legend.text.align = 0
    #legend.title = "Is the certificate a reincorporation?"
  ) +
  # light, dotted major y-grid lines only
  theme(panel.grid=element_line())+
  theme(panel.grid.major.y=element_line(color="#2b2b2b", linetype="dotted", size=0.15))+
  theme(panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  # light x-axis line only
  theme(axis.line=element_line())+
  theme(axis.line.y=element_blank())+
  theme(axis.line.x=element_blank())+
  # tick styling
  theme(axis.ticks=element_line())+
  theme(axis.ticks.x=element_blank())+
  theme(axis.ticks.y=element_blank())+
  theme(axis.ticks.length=unit(5, "pt"))+
  # x-axis labels
  theme(axis.text.x=element_text(size=10, hjust=0.95,vjust=0.2))+
  # breathing room for the plot
  theme(plot.margin=unit(rep(0.5, 4), "cm"))+
  # move the y-axis tick labels over a bit
  theme(axis.text.y=element_text(margin=margin(r=-5)))+
  theme(axis.text.x=element_text(margin=margin(r=-5)))+
  # make the plot title bold and modify the bottom margin a bit
  theme(plot.title=element_text(family="Open Sans Condensed Bold", margin=margin(b=15)))+
  # make the subtitle italic
  theme(plot.subtitle=element_text(family="Open Sans Condensed Light Italic"))+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(t=15)))
ggsave(plot=sos, "incorp_sos.png", width=10, height=6, units="in", dpi="retina")
