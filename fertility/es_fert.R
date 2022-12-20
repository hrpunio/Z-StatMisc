##
## title: "HICP"
##
library("ggplot2")
library("dplyr")
library("scales")
library("ggthemes")
library("ggpubr")
library("tidyr")
library("readr")
library("stringr")
library("ISOweek") ###
library("knitr")
library("eurostat")

##
ex <- c('CH', 'LU', 'ME', 'MK', 'MT', 'RS', 'CY')

fert_0 <- read.csv("tgs00100_tabular.csv", sep = ';',  header=T, na.string="NA") %>%
  select (geo, year, value) %>%
  filter (year == 2020) %>%
  mutate (member = substr(geo, 1, 2)) %>%
  filter (! member %in% ex )


fert_0$member

##fert.pl <- fert_0 %>% filter (geo == 'PL') 
p0 <- ggplot(fert_0, aes(x=member, y=value )) + 
    ggtitle("Fertility rate by NUTS2 regions (2020)", subtitle="Eurostat: tgs00100 table") +
  geom_hline(yintercept = 2.15, color="navyblue", alpha=.25, size=1.2) +
  geom_boxplot(color='deeppink') + ylab("") + xlab("");

ggsave(p0, file="eurofert.png", width=10)
 