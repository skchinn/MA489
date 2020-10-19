library(tidyverse)
library(ggmap)
library(ggplot2)
library(tidyr)

#arrest_data_orig = read.csv("C:/Users/Samantha Chinn/OneDrive - West Point/AY21-1/MA489/MA489 - Spatial Temporal Statistics/Project/NYPD_Arrests_Data__Historic_.csv")
arrest_data = read.csv("C:/Users/Samantha Chinn/OneDrive - West Point/AY21-1/MA489/MA489 - Spatial Temporal Statistics/Project/arrest_data.csv")


rape_data = arrest_data %>% filter(OFNS_DESC == "RAPE")
## ggmap API key
rape_data_white = arrest_data %>% filter(OFNS_DESC == "RAPE",
                                   PERP_RACE == "WHITE",
                                   AGE_GROUP == "25-44",
                                   PERP_SEX == "M")

rape_data_black = arrest_data %>% filter(OFNS_DESC == "RAPE",
                                         PERP_RACE == "BLACK",
                                         AGE_GROUP == "25-44",
                                         PERP_SEX == "M")

rape_data_whitehispanic = arrest_data %>% filter(OFNS_DESC == "RAPE",
                                         PERP_RACE == "WHITE HISPANIC",
                                         AGE_GROUP == "25-44",
                                         PERP_SEX == "M")

rape_data_blackhispanic = arrest_data %>% filter(OFNS_DESC == "RAPE",
                                                 PERP_RACE == "BLACK HISPANIC",
                                                 AGE_GROUP == "25-44",
                                                 PERP_SEX == "M")

rape_data_asianPI = arrest_data %>% filter(OFNS_DESC == "RAPE",
                                                 PERP_RACE == "ASIAN / PACIFIC ISLANDER",
                                                 AGE_GROUP == "25-44",
                                                 PERP_SEX == "M")

rape_data_AIAN = arrest_data %>% filter(OFNS_DESC == "RAPE",
                                           PERP_RACE == "AMERICAN INDIAN/ALASKAN NATIVE",
                                           AGE_GROUP == "25-44",
                                           PERP_SEX == "M")

rape_aggregated = rape_data %>% 
  count(PERP_RACE,Longitude, Latitude)

qmplot(Longitude,Latitude,data = rape_aggregated, maptype = "toner-lite", zoom = 14, color = I("red"), size = n)


qmplot(Longitude,Latitude,data = rape_aggregated, maptype = "toner-lite", zoom = 14, color = I("red"), size = n) +
  facet_wrap(~ PERP_RACE)

arrest_data %>% count(ARREST_BORO)

# qmplot
# https://cran.r-project.org/web/packages/ggmap/readme/README.html
# plotted points
qmplot(Longitude,Latitude,data = rape_data, maptype = "toner-lite", zoom = 14, color = I("red"))
# density plot
rape_plot_white = qmplot(Longitude,Latitude,data = rape_data_white, maptype = "toner-background",darken = 0.7, color = I("red"),
                         legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("White Rape \nPropensity", low = "white", high = "blue",midpoint = 100)

rape_plot_black = qmplot(Longitude,Latitude,data = rape_data_black, maptype = "toner-background",darken = 0.7, color = I("red"),
                         legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Black Rape \nPropensity", low = "white", high = "blue",midpoint = 100)

rape_plot_whitehispanic = qmplot(Longitude,Latitude,data = rape_data_whitehispanic, maptype = "toner-background",darken = 0.7, color = I("red"),
                                 legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("White Hispanic \nRape Propensity", low = "white", high = "blue",midpoint = 100)

rape_plot_blackhispanic = qmplot(Longitude,Latitude,data = rape_data_blackhispanic, maptype = "toner-background",darken = 0.7, color = I("red"),
                                 legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Black Hispanic \nRape Propensity", low = "white", high = "blue",midpoint = 100)

rape_plot_asianPI = qmplot(Longitude,Latitude,data = rape_data_asianPI, maptype = "toner-background",darken = 0.7, color = I("red"),
                                 legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Asian/Pacific Islander \nRape Propensity", low = "white", high = "blue",midpoint = 100)

rape_plot_AIAN = qmplot(Longitude,Latitude,data = rape_data_AIAN, maptype = "toner-background",darken = 0.7, color = I("red"),
                           legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("American Indian/ \nAlaskan Native \nRape Propensity", low = "white", high = "blue",midpoint = 100)

races = rape_data %>% select(PERP_RACE) %>% unique()

# plot side by side
# https://stackoverflow.com/questions/1249548/side-by-side-plots-with-ggplot2
require(gridExtra)
grid.arrange(rape_plot_white,rape_plot_whitehispanic,
             rape_plot_black,rape_plot_blackhispanic,
             ncol = 2)


# create STFDF object
library("sp")
library("spacetime")
arrest_data_ST = arrest_data %>% na.omit()
arrest_data_ST = separate(arrest_data_ST, "ARREST_DATE", c("month", "day", "year"), sep = "/", remove = FALSE)
arrest_data_ST = arrest_data_ST %>% arrange(ARREST_DATE)

spat_part <- unique(arrest_data_ST[, c("Longitude", "Latitude")])
spat_part <- SpatialPoints(coords = spat_part)
temp_part <- with(arrest_data_ST,
                  paste(year, month, day, sep = "-"))
temp_part <- as.Date(temp_part)
temp_part <- unique(temp_part)

#rape_data_ST <- gather(arrest_data$PERP_RACE, arrest_data$ARREST_KEY)
#pivot_longer()



STObj <- STFDF(sp = spat_part,
               time = temp_part,
               data = arrest_data_ST)


arrest_by_month = arrest_data_ST %>% 
  count(month, year, Latitude, Longitude)

View(arrest_by_month)
