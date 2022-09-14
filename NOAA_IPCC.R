library(openair)
library(ggplot2)
library(dplyr)
library(reshape2)

Annual_inc <- read.table("ftp://ftp.cmdl.noaa.gov/ccg/co2/trends/co2_gr_mlo.txt", stringsAsFactors = FALSE, skip = 59)
names(Annual_inc) <- c("year", "annual_increase", "unc")


Annual_mean <- read.table("ftp://ftp.cmdl.noaa.gov/ccg/co2/trends/co2_annmean_mlo.txt", stringsAsFactors = FALSE, skip = 56)
names(Annual_mean) <- c("year", "annual_mean", "unc")

RCP3 <- read.table("dat/iiasa/RCP3PD/RCP3PD_MIDYR_CONC.DAT", stringsAsFactors = FALSE, skip = 38, header = TRUE)
RCP3$RCP <- "RCP3"
RCP3 <- select(RCP3, YEARS, CO2, RCP)
RCP45 <- read.table("dat/iiasa/RCP45/RCP45_MIDYR_CONC.DAT", stringsAsFactors = FALSE, skip = 38, header = TRUE)
RCP45$RCP <- "RCP45"
RCP45 <- select(RCP45, YEARS, CO2, RCP)
RCP6 <- read.table("dat/iiasa/RCP6/RCP6_MIDYR_CONC.DAT", stringsAsFactors = FALSE, skip = 38, header = TRUE)
RCP6$RCP <- "RCP6"
RCP6 <- select(RCP6, YEARS, CO2, RCP)
RCP85 <- read.table("dat/iiasa/RCP85/RCP85_MIDYR_CONC.DAT", stringsAsFactors = FALSE, skip = 38, header = TRUE)
RCP85$RCP <- "RCP85"
RCP85 <- select(RCP85, YEARS, CO2, RCP)

ALL_RCPs_R <- data.frame(date = RCP3$YEARS, RCP3 = RCP3$CO2, RCP45 = RCP45$CO2, RCP6 = RCP6$CO2, RCP85 = RCP85$CO2)

ALL_RCPs <- ALL_RCPs_R %>% 
  filter(date < "2121") %>% 
  melt("date") %>% 
  select(date, RCP = variable, CO2 = value)

## plots without grey background
theme_set(theme_bw())

p1 <- ggplot(ALL_RCPs, aes(x=date, y=CO2, colour=RCP, group=RCP)) +
  geom_line() +
  ggtitle("IIASA Representative Concentration Pathways (RCP)")

ggsave(p1, filename = "plots/All_RCPs.png", width = 300, height = 200, units = 'mm', dpi = 500, limitsize = FALSE)

Both <- left_join(Annual_inc, Annual_mean, by = "year")

p2 <- ggplot(Both, aes(x=year, y=annual_increase)) + 
  geom_point(aes(col=annual_increase, size=annual_increase)) + 
  geom_smooth(method = "lm", se=T, lwd = 1, color = "red") +
  theme(legend.position="none") +
  labs(subtitle="Year on Year increase in parts per million (ppm) of CO2", 
       y="Annual Increase (ppm)", 
       x="Year", 
       title="Annual Increase in CO2", 
       caption = "Source: NOAA")

ggsave(p2, filename = "plots/Annual_increase.png", width = 300, height = 200, units = 'mm', dpi = 500, limitsize = FALSE)
