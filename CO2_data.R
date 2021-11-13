library(openair)
library(lubridate)
library(ggplot2)
library(dplyr)
library(Cairo)
library(stringr)

## downloaded from https://meta.icos-cp.eu/collections/yZecOZ-jPa8nw8JVOTHtlaYN

##ICOS
df <- read.csv("../CO2/CBW_207m_air.hdf.all.COMBI_Drought2018_20190522.co2", skip = 30, sep = ';')
dat <- df %>% 
  mutate(date = ymd_hm(paste(Year, Month, Day, Hour, Minute)),
         annual_mean = co2) %>% 
  select(date, annual_mean) %>% 
  filter(!annual_mean < -999)

dat_yr <- dat %>% 
  timeAverage('year') %>% 
  transmute(year = year(date),
            annual_mean)

dat_month <- dat %>% 
  timeAverage('month')

dat_inc <- data.frame(annual_increase = diff(as.matrix(dat_yr$annual_mean)))
dat_inc$year <- dat_yr$year[2:NROW(dat_yr)]

Both <- left_join(dat_inc, dat_yr, by = "year")
Both <- na.omit(Both)

scatterPlot(Both, x = "year", y = "annual_increase", smooth = TRUE, ci = TRUE)
scatterPlot(dat, x = "date", y = "annual_mean", smooth = TRUE, ci = TRUE)

theme_set(theme_bw())

p2 <- ggplot(dat_inc, aes(x=year, y=annual_increase)) + 
  geom_point(aes(col=factor(annual_increase), size=annual_increase)) + 
  geom_smooth(method = "lm", se=T, lwd = 2, color = "tomato2", alpha = 0.4) +
  theme_dark() +
  theme(legend.position="none") +
  labs(subtitle=expression("Year on Year increase in parts per million (ppm) of CO"[2]), 
       y=expression("Annual Increase CO"[2]*"(ppm)"), 
       x="Year", 
       title=expression("Annual Increase in CO"[2]), 
       caption = "Data source: ICOS")


p3 <- ggplot(dat, aes(x=date, y=annual_mean)) + 
  geom_smooth(method = "lm", se=T, lwd = 3) +
  theme(legend.position="none") +
  labs(subtitle="Year on Year increase in parts per million (ppm) of CO2", 
       y="Annual mean (ppm)", 
       x="Year", 
       title="Annual Increase in CO2", 
       caption = "Source: ICOS")



## NOAA
Annual_inc <- read.table("ftp://ftp.cmdl.noaa.gov/ccg/co2/trends/co2_gr_mlo.txt", stringsAsFactors = FALSE, skip = 59)
names(Annual_inc) <- c("year", "annual_increase", "unc")

Annual_mean <- read.table("ftp://ftp.cmdl.noaa.gov/ccg/co2/trends/co2_annmean_mlo.txt", stringsAsFactors = FALSE, skip = 56)
names(Annual_mean) <- c("year", "annual_mean", "unc")

co2_month <- read.table('https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_mm_mlo.txt', stringsAsFactors = FALSE)
names(co2_month) <- c('year', 'month', 'decimal date', 'monthly average', 'de-seasonalized', '#days', 'st.dev of days', 'unc. of mon mean')
co2_month <- co2_month %>% 
  mutate(date = date_decimal(`decimal date`)) %>% 
  timeAverage('month')

noaa_icos <- dat_month %>% 
  left_join(co2_month, by = 'date') %>% 
  select(date, icos = annual_mean, noaa = 'monthly average') %>% 
  tidyr::pivot_longer(cols = c(icos, noaa))

p2 <- ggplot(noaa_icos, aes(x=date, y=value, group = name))
  geom_line()
  # theme_dark() +
  # theme(legend.position="none") +
  # labs(subtitle=expression("Year on Year increase in parts per million (ppm) of CO"[2]), 
  #      y=expression("Annual Increase CO"[2]*"(ppm)"), 
  #      x="Year", 
  #      title=expression("Annual Increase in CO"[2]), 
  #      caption = "Data source: NOAA")

timeVariation(noaa_icos, 'value', group = 'name')

## data from https://tntcat.iiasa.ac.at/RcpDb/dsd?Action=htmlpage&page=download
## download links (might be neccesary to provide name and email first. If doesn't work try the link in a browser and then try again)
RCPs <- c('https://tntcat.iiasa.ac.at/RcpDb/download/CMIP5RECOMMENDATIONS/RCP3PD_MIDYR_CONC.zip',
          'https://tntcat.iiasa.ac.at/RcpDb/download/CMIP5RECOMMENDATIONS/RCP45_MIDYR_CONC.zip',
          'https://tntcat.iiasa.ac.at/RcpDb/download/CMIP5RECOMMENDATIONS/RCP6_MIDYR_CONC.zip',
          'https://tntcat.iiasa.ac.at/RcpDb/download/CMIP5RECOMMENDATIONS/RCP85_MIDYR_CONC.zip')

all_rcp <- list()
for (r in RCPs){
  rcp_nam <- str_sub(r, 64, -16)
download.file(r, destfile = 'file.zip')
unzip('file.zip', exdir = paste0('dat/iiasa/', rcp_nam), overwrite = TRUE)
unlink('file.zip')

rcp_file <- list.files(paste0('dat/iiasa/', rcp_nam), pattern = rcp_nam, full.names = TRUE)

df <- read.table(rcp_file, stringsAsFactors = FALSE, skip = 38, header = TRUE)
df$RCP <- rcp_nam
df <- select(df, YEARS, CO2, RCP)

all_rcp[[rcp_nam]] <- df

assign(rcp_nam, df)

}


ALL_RCPs_R <- do.call(rbind, all_rcp)
## filter out after 2160
ALL_RCPs_R <- filter(ALL_RCPs_R, YEARS < "2160")


p1 <- ggplot(ALL_RCPs_R, aes(x=YEARS, y=CO2, colour=RCP, group=RCP)) +
  geom_line(lwd = 2) +
  theme_dark() +
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey41")) +
  theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "grey41")) +
  scale_y_continuous(breaks = seq(200, 1400, by = 200)) +
  scale_x_continuous(breaks = seq(1750, 2150, by = 50)) +
  labs(subtitle=expression("Past CO"[2]*" Concentrations and future RCP Scenarios"), 
       y=expression("Annual Average CO"[2]*"Concentrations (ppm)"), 
       x="Year", 
       title=expression("CO"[2]*" Trend 1765 to 2160"), 
       caption = "Data source: IIASA.ac.at")


png("plots/RCPs.png",
    type="cairo",
    units="mm", 
    width=400, 
    height=300, 
    pointsize=50, 
    res=2000)
print(p1)
dev.off()

Both <- left_join(Annual_inc, Annual_mean, by = "year")

scatterPlot(Both, x = "year", y = "annual_mean", smooth = TRUE, ci = TRUE)
summary(Annual_inc)

theme_set(theme_bw())

p2 <- ggplot(Both, aes(x=year, y=annual_increase)) + 
  geom_point(aes(col=factor(annual_increase), size=annual_increase)) + 
  geom_smooth(method = "lm", se=T, lwd = 3, color = "tomato2", alpha = 0.4) +
  theme_dark() +
  theme(legend.position="none") +
  labs(subtitle=expression("Year on Year increase in parts per million (ppm) of CO"[2]), 
       y=expression("Annual Increase CO"[2]*"(ppm)"), 
       x="Year", 
       title=expression("Annual Increase in CO"[2]), 
       caption = "Data source: NOAA")


png("CO2_Increase.png",
    type="cairo",
    units="mm", 
    width=355, 
    height=200, 
    pointsize=40, 
    res=2000)

print(p2)
dev.off()

p3 <- ggplot(Both, aes(x=year, y=annual_mean)) + 
  geom_smooth(method = "lm", se=T, lwd = 3) +
  theme(legend.position="none") +
  labs(subtitle="Year on Year increase in parts per million (ppm) of CO2", 
       y="Global annual mean (ppm)", 
       x="Year", 
       title="Annual Increase in CO2", 
       caption = "Source: NOAA")
print(p3)

png("RCPs.png",
    type="cairo",
    units="in", 
    width=14, 
    height=9, 
    pointsize=50, 
    res=2000)
print(p1)
dev.off()
