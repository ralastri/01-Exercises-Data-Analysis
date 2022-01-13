## ### ## ### ## ### ## ### ## ### ## ### ## ### ## ### ## ### ## ### ## ##
# 
# Exercise 01 - Datamanagement, Collection and Storage - Michael Stölzle
#
#
# Raffaello Lastrico, 11.01.2022
#
## ### ## ### ## ### ## ### ## ### ## ### ## ### ## ### ## ### ## ### ## ##


# Setup  ------------------------------------------------------------------

# alte R-Objekte loeschen
rm(list=ls())


# benoetigte Pakete laden -------------------------------------------------
library(tidyverse)



# Datenimport und -aufbereitung -------------------------------------------------------------
# read in data: skip 1 line, sep: ",", 
hobo_raw <- read_csv(file = "0_data_raw/10347366.csv",
                     skip = 1, col_types = cols("i","c","d","d","c","c","c"))

# Rename columns for easy examination
hobo_raw <- hobo_raw %>% 
  rename(id = '#', 
         dttm = `Date Time, GMT+01:00`, 
         temp = `Temp, °C (LGR S/N: 10347366, SEN S/N: 10347366)`, 
         lux = `Intensity, Lux (LGR S/N: 10347366, SEN S/N: 10347366)`,
         attached = `Coupler Attached (LGR S/N: 10347366)`,
         connected = `Host Connected (LGR S/N: 10347366)`,
         stopped = `Stopped (LGR S/N: 10347366)`,
         eof = `End Of File (LGR S/N: 10347366)`
         )

# format dttm as PosixCT
hobo_raw$dttm <- hobo_raw$dttm %>% 
  lubridate::dmy_hms(tz = "Etc/GMT+1")

# examine meta columns
hobo_raw %>% 
  filter(!is.na(attached) | !is.na(connected) | !is.na(stopped) | !is.na(eof))

  ## the Meta columns dont contain any data besides at the end of file

# remove last columns
hobo_raw <- hobo_raw %>% 
  select(id:lux)

# are there NA values?
hobo_raw %>% 
  filter(is.na(temp) | is.na(lux))
  
  ## yes. at the end, where eof was logged!

# remove these two lines
hobo_raw <- hobo_raw[1:(nrow(hobo_raw)-2),]



# Prüfung auf Plausibilität -----------------------------------------------
  
par(mfrow = c(2,1), mar = c(4,4,2,1), oma = c(2,1,0,1))
plot(hobo_raw$id, hobo_raw$temp, type = "l", ylab = "Temp [°C]")
plot(hobo_raw$id, hobo_raw$lux, type = "l", ylab="Lux", xlab = "Index")

  ## Data looks good, temperature rises to 30 C when Lightintensity peaks -> propable sun influence
  ## other than that no obvious problems
  ## Light measurement within range (max. 320000)

par(mfrow = c(1,1))
plot(hobo_raw$temp, sqrt(hobo_raw$lux))

# Datenexport -------------------------------------------------------------
hobo_export <- hobo_raw

# filter desired time range (by Prof)
hobo_export <- hobo_export %>%
  filter(dttm >= as.POSIXct("2021-12-13 00:00", tz = "Etc/GMT+1"), dttm <= as.POSIXct("2022-01-09 23:50", tz = "Etc/GMT+1"))

# format date for export
hobo_export$dttm <- format(hobo_export$dttm, format = "%Y-%m-%d %H:%M")

# rewrite id column:
hobo_export$id <- 1:nrow(hobo_export)

# write dataframe to csv:
write.csv(hobo_export, 
          file = "0_data_raw/10347366_processed.csv",
          row.names = F,
          quote = F)


# Getting to know the data ------------------------------------------------

# spielerei
library(ggplot2)
hobo_raw$time <- format(hobo_raw$dttm, format = "%H:%M")

ggplot(data = hobo_raw, aes(x = time, y = temp))+
  geom_point()

hobo_raw %>% 
  filter(lux > 0, temp > 15) %>% 
  ggplot(aes(x = temp, y = lux))+
    geom_point()+
    scale_y_sqrt()


