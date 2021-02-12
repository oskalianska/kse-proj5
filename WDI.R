library(ggplot2)
library(maps)
library(readr)

# Update these to plot different indicators
indicatorName <- "Life expectancy at birth, total (years)"
indicatorYear <- 2013

indicators <- read_csv("/Users/olena.skalianska/Downloads/world-development-indicators/Indicators.csv")

filtered <- indicators[indicators$IndicatorName==indicatorName & indicators$Year==indicatorYear,]
# write_csv(filtered, "filtered.csv")

correction <- c("Antigua and Barbuda"="Antigua", "Bahamas, The"="Bahamas", "Brunei Darussalam"="Brunei", "Cabo Verde"="Cape Verde", "Congo, Dem. Rep."="Democratic Republic of the Congo", "Congo, Rep."="Republic of Congo", "Cote d'Ivoire"="Ivory Coast", "Egypt, Arab Rep."="Egypt", "Faeroe Islands"="Faroe Islands", "Gambia, The"="Gambia", "Iran, Islamic Rep."="Iran", "Korea, Dem. Rep."="North Korea", "Korea, Rep."="South Korea", "Kyrgyz Republic"="Kyrgyzstan", "Lao PDR"="Laos", "Macedonia, FYR"="Macedonia", "Micronesia, Fed. Sts."="Micronesia", "Russian Federation"="Russia", "Slovak Republic"="Slovakia", "St. Lucia"="Saint Lucia", "St. Martin (French part)"="Saint Martin", "St. Vincent and the Grenadines"="Saint Vincent", "Syrian Arab Republic"="Syria", "Trinidad and Tobago"="Trinidad", "United Kingdom"="UK", "United States"="USA", "Venezuela, RB"="Venezuela", "Virgin Islands (U.S.)"="Virgin Islands", "Yemen, Rep."="Yemen")

for (c in names((correction))) {
  filtered[filtered$CountryName==c,"CountryName"] = correction[c]
}

map.world <- merge(x=map_data(map="world"),
                   y=filtered[,c("CountryName","Value")],
                   by.x="region",
                   by.y="CountryName",
                   all.x=TRUE)
map.world <- map.world[order(map.world$order),]

p <- ggplot(map.world) +
  geom_map(map=map.world, aes(map_id=region, x=long, y=lat, fill=Value)) + 
  scale_fill_gradient(low = "brown3", high = "green", guide = "colourbar") +
  coord_equal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.title=element_blank(),
        legend.position="bottom") +
  ggtitle(paste0(indicatorName, " in ", indicatorYear))

plot(p)
ggsave("map.png", p, width=7, height=4, units="in")

#___________________________________________

indicatorName <- "Life expectancy at birth, total (years)"
indicatorYear <- 1960

# Update these to plot different indicators
filtered <- indicators[indicators$IndicatorName==indicatorName & indicators$Year==indicatorYear,]
# write_csv(filtered, "filtered.csv")

correction <- c("Antigua and Barbuda"="Antigua", "Bahamas, The"="Bahamas", "Brunei Darussalam"="Brunei", "Cabo Verde"="Cape Verde", "Congo, Dem. Rep."="Democratic Republic of the Congo", "Congo, Rep."="Republic of Congo", "Cote d'Ivoire"="Ivory Coast", "Egypt, Arab Rep."="Egypt", "Faeroe Islands"="Faroe Islands", "Gambia, The"="Gambia", "Iran, Islamic Rep."="Iran", "Korea, Dem. Rep."="North Korea", "Korea, Rep."="South Korea", "Kyrgyz Republic"="Kyrgyzstan", "Lao PDR"="Laos", "Macedonia, FYR"="Macedonia", "Micronesia, Fed. Sts."="Micronesia", "Russian Federation"="Russia", "Slovak Republic"="Slovakia", "St. Lucia"="Saint Lucia", "St. Martin (French part)"="Saint Martin", "St. Vincent and the Grenadines"="Saint Vincent", "Syrian Arab Republic"="Syria", "Trinidad and Tobago"="Trinidad", "United Kingdom"="UK", "United States"="USA", "Venezuela, RB"="Venezuela", "Virgin Islands (U.S.)"="Virgin Islands", "Yemen, Rep."="Yemen")

for (c in names((correction))) {
  filtered[filtered$CountryName==c,"CountryName"] = correction[c]
}

map.world <- merge(x=map_data(map="world"),
                   y=filtered[,c("CountryName","Value")],
                   by.x="region",
                   by.y="CountryName",
                   all.x=TRUE)
map.world <- map.world[order(map.world$order),]

p1 <- ggplot(map.world) +
  geom_map(map=map.world, aes(map_id=region, x=long, y=lat, fill=Value)) + 
  scale_fill_gradient(low = "brown3", high = "green", guide = "colourbar") +
  coord_equal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.title=element_blank(),
        legend.position="bottom") +
  ggtitle(paste0(indicatorName, " in ", indicatorYear))

plot(p1)
ggsave("map1.png", p, width=7, height=4, units="in")

library(ggpubr)
theme_set(theme_pubr())

figure <- ggarrange(p, p1,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)
figure
