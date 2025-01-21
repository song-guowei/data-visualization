if (!require(ggplot2)) install.packages('ggplot2')
if (!require(showtext)) install.packages('showtext')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(readxl)) install.packages('readxl')
if (!require(lubridate)) install.packages('lubridate')
if (!require(raster)) install.packages('raster')
if (!require(tmap)) install.packages('tmap')
if (!require(tmaptools)) install.packages('tmaptools')
if (!require(shinyjs)) install.packages('shinyjs')
library(ggplot2)
library(showtext)
library(tidyverse)
library(readxl)
library(lubridate)
library(raster)
library(tmap)
library(tmaptools)
library(shinyjs)

rm(list=ls())
path <- "/home/dashaa/wildfire_politics/data"

#load cute fonts
font_add_google("IBM Plex Sans", "ibm plex sans")
font_add_google("IBM Plex Serif", "ibm plex serif")
the_font <- "ibm plex sans"


###visualizing data on two 2020 wildfire complexes in North California
###the script produces graphs average_aerial_comparison.png (area plot) and news_citations.png (line plot)

##define common colors first
orange_color <- "#FF7F50"
purple_color <- "#C9A0DC"
etal_color <- "#008080"

##start with average_aerial_comparison.png
#load the data with ICS-209 reports
sitreps <- read.csv(paste0(path,
                           "/build_wildfire_reports/input/raw/new_ics209plus/ics209plus-wildfire/ics209-plus-wf_sitreps_1999to2020.csv"))
complexes <- filter(sitreps,INCIDENT_NAME %in% c("North Complex","LNU LIGHTNING COMPLEX"))

#add average values for aerial resources
complexes <- complexes %>%
  mutate(
    REPORT_FROM_DATE = ymd_hms(REPORT_FROM_DATE),
    REPORT_TO_DATE = ymd_hms(REPORT_TO_DATE),
    REPORT_MID_DATE = date(REPORT_FROM_DATE + (REPORT_TO_DATE - REPORT_FROM_DATE) / 2)
  ) %>%
  group_by(REPORT_MID_DATE) %>%
  summarize(
    AVERAGE_AERIAL = mean(TOTAL_AERIAL, na.rm = TRUE),
  ) %>%
  rename(Complex = INCIDENT_NAME)

setwd("/home/dashaa/documents/GitHub/data-visualization/output")

#a function to use with other variables as needed
set_common_theme <- function(data,mapping,linewidth) {
  ggplot(data,mapping) +
    geom_area(alpha = 0.6, position = "identity") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white", color="white"),
          axis.text = element_text(size = 14, family = the_font),
          axis.title.x = element_text(size = 16, family = the_font, margin = margin(t = 10)),
          axis.title.y = element_text(size = 16, family = the_font, margin = margin(r = 10)),
          legend.title = element_text(size = 16, family = the_font),
          legend.text = element_text(size=14, family = the_font)) +
    geom_vline(xintercept = as.numeric(as.Date("2020-09-09")), linetype = "dashed", 
               color = orange_color,size=1.2)
}

#aerial data
aerial_graph <- set_common_theme(combined_data, 
                                 mapping = aes(x = REPORT_MID_DATE, y = AVERAGE_AERIAL, color = Complex, fill = Complex),
                                 linewidth=1.35)
aerial_graph <- aerial_graph + 
  scale_color_manual(values = c(purple_color,teal_color)) +
  scale_fill_manual(values = c(purple_color,teal_color)) +
  labs(x = "Date",
       y = "Average Daily Aerial",
       color = "Complex") +
  annotate("text", x = as.Date("2020-09-10"), y = max(combined_data$AVERAGE_AERIAL, na.rm = TRUE)*1.1, 
           label = "Orange Sky", vjust = 0, hjust = 0, color = orange_color, size=5)
ggsave("average_aerial_comparison.png", aerial_graph, width = 12, height = 6, dpi = 600)

##now news_citations.png
#load news data
news <- read_excel(paste0(path,"/data_analysis/input/built/fire_citations.xlsx"))
news <- news%>%
  mutate(Date=mdy(Date)) %>%
  dplyr::select(-c("Claremont Fire","Meyers Fire", "Walbridge Fire"))
news <- melt(news, id.vars = "Date") %>%
  rename(Fire=variable)

news_graph <- ggplot(news, aes(x = Date, y = value, color = Fire, linetype = Fire)) +
  geom_line(linewidth=1.35) +
  scale_color_manual(values = c(teal_color, teal_color,purple_color,purple_color)) +
  scale_linetype_manual(values = c(1, 3, 1, 3)) +
  labs(x = "Date", y = "#articles citing fires") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = "white", color="white"),
        axis.text = element_text(size = 14, family = the_font),
        axis.title.x = element_text(size = 16, family = the_font, margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, family = the_font, margin = margin(r = 10)),
        legend.title = element_text(size = 16, family = the_font),
        legend.text = element_text(size=14, family = the_font)) +
  geom_vline(xintercept = as.numeric(as.Date("2020-09-09")), linetype = "dashed", 
             color = orange_color,size=1.25) +
  annotate("text", x = as.Date("2020-09-09")+0.25, y = max(news$value, na.rm = TRUE)*1.1, 
           label = "Orange Sky", vjust = 0, hjust = 0, color = orange_color, size=5, family = the_font)
ggsave("news_citations.png", news_graph, width = 12, height = 5)




###visualizing the number of times significant events were published on the front page of the New York Times
bignewsdays <- readRDS(paste0(path,"/build_big_news/output/big_news_days.rds"))
bignewsdays <- mutate(bignewsdays,year=substr(day,1,4))

summary_data <- bignewsdays %>%
  group_by(year) %>%
  summarise(
    events_1 = length(unique(unlist(all_articles))),
    !!!setNames(lapply(1:10, function(i) {
      length(unique(unlist(get(paste0("news", i, "days")))))
    }), paste0("events_", 1:10))
  )

df_long <- summary_data %>%
  pivot_longer(cols = starts_with("events_"),
               names_to = "days_published",
               values_to = "event_counts") %>%
  mutate(days_published = as.numeric(str_replace(days_published, "events_", "")))

stats <- ggplot(df_long, aes(x = factor(days_published), y = event_counts, fill = factor(year))) +
  geom_bar(stat = "identity", position = "stack") +  
  scale_fill_viridis_d() +
  labs(x = "X", y = "Number of Wikipedia's Events with Publications",
       fill = "Year",
       title = "Wikipedia's Events published in NYT for at least X Days, coloured by Year") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "white", color="white"),
        plot.title = element_text(size=90,hjust=0.5, margin = margin(b = 40), family = "ibm plex serif"),
        axis.text.x = element_text(size = 80, margin = margin(t = -40), family = the_font),
        axis.text.y = element_text(size = 80, family = the_font),
        axis.title.y = element_text(size = 85, margin = margin(r = 20), family = the_font),
        axis.title.x = element_text(size = 85, margin = margin(t = 20), family = the_font),
        legend.text = element_text(size = 65, family = the_font),
        legend.title = element_text(size = 85, family = the_font),
        legend.key.height = unit(1, "cm"),
        plot.margin = margin(t = 50, r = 30, b = 40, l = 30)
  ) 
ggsave("bignewsdays_stats.png", stats, width = 16, height = 16, units = "in",dpi=300)




### Visualizing types of forest owners across the U.S.

path <- "/home/dashaa/carb_offsets/data/build_forest_ownership"

forest_own <- raster(paste0(path,"/input/raw/USDA/Data/forest_own1/forest_own1.tif"))
f <- freq(forest_own)
usa <- getData("GADM", country="USA", level=1)
usa <- usa[usa$NAME_1!="Alaska",]
usa <- usa[usa$NAME_1!="Hawaii",]
usa <- spTransform(usa,crs(forest_own))

usa_map <- tm_shape(usa) +
  tm_borders()

#tmaptools::palette_explorer() - for selecting colors
#- in front of the palette name is for reversing color order
#names(pdfFonts()) - a list of fonts
forest_own_map <- tm_shape(forest_own) +
  tm_raster(style="cat",palette="-Set1",alpha=0.8,
            legend.show=F) 

legend <- tm_add_legend('symbol',col=get_brewer_pal("-Set1", n = 8),alpha=0.8,
                        shape=22,border.col="grey40",
                        labels=c("Family","Corporate","Investment Organizations","Other Private","Federal","State","Local","Tribal"),
                        title="Forest Ownership Type") +
  tm_layout(legend.outside=T,fontfamily = "NimbusRom",fontface=1.5,)# + 

all_map <- forest_own_map + 
  usa_map + 
  legend +
  tm_layout(frame=FALSE)

png(file = "/output/forest_ownership_map.png",units="in", width=7, height=5, res=300)
all_map
dev.off()
