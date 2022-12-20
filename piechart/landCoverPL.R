# -- we are r --
require("ggplot2")
library("dplyr")
library("RColorBrewer")
library("ggpubr")
library("tidyverse")
library("tidyr")

# kolorki definicje
mycols <- c( '#e6550d', '#636363', '#31a354', '#e5f5e0', 
             '#3182bd', '#feb24c', '#ffffb3', '#005a32', '#543005')

## Gminy wg typów (za BDL/GUS)
g <- read.csv("gminy-typy.csv", sep = ';', 
              dec = ".",  header=T, na.string="NA") %>%
  select (Nazwa, type=RodzajeGmin, value=Wartosc) 

## P1: wykres słupkowy
p.1 <- g %>% filter (Nazwa == "POLSKA") %>% 
  ggplot(aes(x = reorder(type, value), y=value )) +
  geom_bar(stat="identity", fill='forestgreen') +
  xlab(label="") + ylab(label="% ") +
  coord_flip()+ ggtitle("Gminy w Polsce według typów (2021)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=sprintf ("%i", value)), hjust=1.2, color="black", size=3.0) +
  theme(legend.position="none")
p.1

## PC.1: wykres kołowy
pc.1 <- g %>% filter (Nazwa == "POLSKA") %>% 
  #arrange(desc(type)) %>%
  #mutate(yy.value = cumsum(value) - 0.5*value) %>%
  mutate(yy.value = value/sum(value)*100) %>%
  ggplot(aes(x = "", y = value, fill = type)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  #geom_text(aes(y = yy.value, label = value), color = "black")+
  geom_text(aes(label = value), position = position_stack(vjust=0.5)) +
  scale_fill_manual(values = mycols) +
  ggtitle("Gminy w Polsce według typów (2021)") +
  ##labs(caption="Źródło: BDL", fill = "Typ") +
  theme_void()

pc.1

### Porówanie ## 
### 3 województwa

p.2 <- g %>% filter (Nazwa %in% c("POMORSKIE", "MAZOWIECKIE", "ŚLĄSKIE")) %>%
  ggplot(aes(x = reorder(type, value), y=value, fill=Nazwa, group=Nazwa )) +
  geom_bar(stat="identity",  position="dodge") +
  xlab(label="") + ylab(label="% ") +
  coord_flip()+ 
  ggtitle("Gminy w wybranych województwach wg typów") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = mycols) +
  geom_text(aes(
    x = reorder(type, value), y=value, group=Nazwa,
    label=sprintf ("%i", value)), 
    ### https://stackoverflow.com/questions/6017460/position-geom-text-on-dodged-barplot
    position = position_dodge(width = .9),
    hjust=1.2, color="black", size=3.0) +
  ###scale_color_discrete(guide=F) +
  ## theme(legend.position="none")
  theme(legend.position="top")
p.2

p.3 <- g %>% filter (Nazwa %in% c("POMORSKIE", "MAZOWIECKIE", "ŚLĄSKIE")) %>%
  ggplot(aes(x = type, y=value, fill=Nazwa, group=Nazwa )) +
  geom_bar(stat="identity",  position="stack") +
  xlab(label="") + ylab(label="% ") +
  coord_flip()+ 
  ggtitle("Gminy w wybranych województwach wg typów") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = mycols) +
  geom_text(aes(
    x = type, y=value, group=Nazwa,
    label=sprintf ("%i", value)), 
    ### https://stackoverflow.com/questions/6017460/position-geom-text-on-dodged-barplot
    position = position_stack(),
    hjust=1.2, color="black", size=3.0) +
  ###scale_color_discrete(guide=F) +
  ## theme(legend.position="none")
  theme(legend.position="top")
p.3

## (Multi) Pie chart
## Faceted pie-chart with text
pc.3.x <- g %>% filter (Nazwa %in% c("POMORSKIE", "MAZOWIECKIE", "ŚLĄSKIE")) %>%
  group_by(Nazwa) %>%
  #arrange(desc(type)) %>%
  ##mutate(yy.value = cumsum(value) - 0.5*value) %>%
  mutate(yy.value = value /sum (value) * 100)  %>%
  ungroup() %>%
  ggplot(aes(x = "", y = yy.value, fill = type)) +
  geom_bar(width = 1, stat = "identity") +
  #coord_polar("y", start = 0)+
  geom_text(aes(label = value), color = "black", position = position_stack(vjust=0.5))+
  #scale_fill_manual(values = mycols) +
  ggtitle("Total nights spent by country") +
  theme_void() +
  facet_wrap(~Nazwa)

pc.3.x

## pie
pc.3 <- pc.3.x + coord_polar("y", start = 0) +
  theme(legend.position = "bottom")
pc.3

######################## PRZYKŁAD 2 ######################################

d <- read.csv("PODZ_2780_CREL_20221218071042.csv", sep = ';', 
              dec = ".",  header=T, na.string="NA") %>%
  select (Nazwa, type=Kierunki.wykorzystania.powierzchni, unit=Jednostka.miary,
          value=Wartosc) %>%
  mutate(value=str_replace(value, ',', '.'),
      value = as.numeric(value)) %>%
  filter (! grepl('ogółem', type))


p.1 <- d %>% filter (Nazwa == "POLSKA") %>% 
  #arrange(desc(value)) %>%
  ggplot(aes(x = reorder(type, value), y=value )) +
  geom_bar(stat="identity", fill='forestgreen') +
  xlab(label="") + ylab(label="% ") +
  ##coord_cartesian(ylim = c(0, 60)) +
  scale_y_continuous(name="?", limits=c(0, 60)) +
  coord_flip()+ ggtitle(sprintf ("Share of land use %s", "PL")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ##scale_fill_manual(values = mycolors) +
  geom_text(aes(label=sprintf ("%.1f", value)), hjust=-.08, color="black", size=3.0) +
  ###scale_color_discrete(guide=F) +
  theme(legend.position="none")
p.1

pc.1 <- d %>% filter (Nazwa == "POLSKA") %>% 
  arrange(desc(type)) %>%
  ##mutate(yy.value = cumsum(value) - 0.5*value) %>%
  mutate(yy.value = value/sum(value)*100) %>%
  ggplot(aes(x = "", y = value, fill = type)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  #geom_text(aes(y = yy.value, label = value), color = "black")+
  scale_fill_manual(values = mycols) +
  ggtitle("Total nights spent by country") +
  geom_text(aes(label = value), position = position_stack(vjust=0.5)) +
  theme_void()

pc.1



p.2 <- d %>% filter (Nazwa %in% c("POMORSKIE", "MAZOWIECKIE", "ŚLĄSKIE")) %>%
  ggplot(aes(x = reorder(type, value), y=value, fill=Nazwa )) +
  geom_bar(stat="identity",  position="dodge") +
  xlab(label="") + ylab(label="% ") +
  ##coord_cartesian(ylim = c(0, 60)) +
  ##scale_y_continuous(name="?", limits=c(0, 60)) +
  coord_flip()+ ggtitle(sprintf ("Share of land use %s", "PL")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ##scale_fill_manual(values = mycolors) +
  ##geom_text(aes(label=sprintf ("%.1f", value)), hjust=-.08, color="black", size=3.0) +
  ###scale_color_discrete(guide=F) +
  theme(legend.position="none")
  
p.2


p.3 <- d %>% filter (Nazwa %in% c("POMORSKIE", "MAZOWIECKIE", "ŚLĄSKIE")) %>%
  ggplot(aes(x = reorder(Nazwa, value), y=value, fill=type )) +
  geom_bar(stat="identity",  position="stack") +
  xlab(label="") + ylab(label="% ") +
  ##coord_cartesian(ylim = c(0, 60)) +
  ##scale_y_continuous(name="?", limits=c(0, 60)) +
  coord_flip()+ ggtitle(sprintf ("Share of land use %s", "PL")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ##scale_fill_manual(values = mycolors) +
  ##geom_text(aes(label=sprintf ("%.1f", value)), hjust=-.08, color="black", size=3.0) +
  ###scale_color_discrete(guide=F) +
  theme(legend.position="bottom")

p.3


pc.3.x <- d %>% filter (Nazwa %in% c("POMORSKIE", "MAZOWIECKIE", "ŚLĄSKIE")) %>%
  group_by(Nazwa) %>%
  #arrange(desc(type)) %>%
  ##mutate(yy.value = cumsum(value) - 0.5*value) %>%
  mutate(yy.value = value /sum (value) * 100)  %>%
  ungroup() %>%
  ggplot(aes(x = "", y = yy.value, fill = type)) +
  geom_bar(width = 1, stat = "identity") +
  #coord_polar("y", start = 0)+
  geom_text(aes(label = value), color = "black", position = position_stack(vjust=0.5))+
  #scale_fill_manual(values = mycols) +
  ggtitle("Total nights spent by country") +
  theme_void() +
  facet_wrap(~Nazwa)

pc.3.x

## pie
pc.3 <- pc.3.x + coord_polar("y", start = 0) +
  theme(legend.position = "bottom")
pc.3

## koniec


