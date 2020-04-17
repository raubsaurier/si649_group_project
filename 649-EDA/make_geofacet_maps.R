## load small multiples data

rm(list=ls())
library(ggplot2)
library(tidyverse)
library(dplyr)
library(geofacet)
library(plotly)
library(htmlwidgets)
library(tibble)


wd  <- "~/repos/si649_group_project/649-EDA/"

smData <- read.csv(paste0(wd,"small_multiples.csv"), stringsAsFactors = FALSE)

graphData = subset(smData, level_ind=="State")

natData = subset(smData, level_ind=="National")

natData = natData %>%
  group_by(year) %>%
  summarise(nat_average = mean(pct_value))


graphData = merge(graphData, natData, by="year")

graphData$perform_ind = ifelse(graphData $pct_diff_from_mean<0, "Below Nat. Average", "Above Nat. Average")


colorScale =  c("#9400D3","#228B22")
names(colorScale) = c("Below Nat. Average", "Above Nat. Average")

## plot small multiples 
small_multiples = ggplot(data=graphData,
                         aes(x = year, y = pct_diff_from_mean, fill=perform_ind
                                           ,text = paste(
                                            "Households w/ Access (%):", pct_value, "\n",
                                            "National Average (%):", nat_average, "\n"
                                          #   sep = ""
                                          # )
                                          ))) +
  geom_bar(stat="identity") +
  facet_geo(~state_abbr) + 
  scale_fill_manual(values=colorScale, name="") +
    labs(title = "% of U.S. Households w/ Computer Access",
       subtitle = "2015-2017",
       x = "",
       y = "") +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_continuous(breaks = c(2015,2016,2017)) + 
  theme(axis.text.x = element_text(angle = 45),
        strip.background = element_blank())

sm_plotly = ggplotly(small_multiples, tooltip = c("text"))


htmlwidgets::saveWidget(frameableWidget(sm_plotly), "sm_charts_update.html")
  
saveWidget(sm_plotly,paste0(wd,"sm_multiples.html"))





plotly_json(sm_plotly)

