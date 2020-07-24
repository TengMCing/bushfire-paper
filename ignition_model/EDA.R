# Load library
library(sf)
library(tidyverse)
library(rnaturalearth)
library(GGally)
library(tourr)
library(RColorBrewer)
library(naniar)

au_map <- ne_states(country = 'Australia', returnclass = 'sf')
vic_map <- au_map[7,]

set.seed(357)

# Read in training data
training <- read_csv("training.csv")
training <- select(training, -c(EVENTID:CAUSE), -id, -FOREST)
training <- mutate(training, 
                   log_dist_cfa = log(dist_cfa),
                   log_dist_camp = log(dist_camp),
                   log_dist_road = log(dist_road),
                   COVER = factor(COVER),
                   HEIGHT = factor(HEIGHT),
                   FOR_CODE = factor(FOR_CODE))

p <- vis_miss(training)
ggsave("plots/vis_miss.jpeg", plot = p, width = 14)

training <- na.omit(training)

for (each_variable in names(training)){
  if (each_variable == "lon") next
  if (each_variable == "lat"){
    p <- ggplot(training) +
      geom_sf(data = vic_map) + 
      geom_density_2d(aes(lon, lat)) +
      facet_wrap(~new_cause) +
      ggtitle("2D density plot of hotspots ignition")
    
    ggsave(paste0("plots/", "lon_lat", ".jpeg"), plot = p, width = 14)
  }
  
  if (each_variable == 'new_cause'){
    p <- ggplot(training) +
      geom_bar(aes(new_cause)) +
      ggtitle("Histogram of new_cause")
    ggsave(paste0("plots/", "new_cause", ".jpeg"), plot = p, width = 8)
    
  } else {
    if (is.numeric(training[[each_variable]])){
      p <- ggplot(training) + 
        geom_density(aes_string(each_variable, col = "new_cause"), size = 1)+
        scale_colour_brewer(palette = "Set1") +
        ggtitle(paste0("Density of ", each_variable))
        
      ggsave(paste0("plots/", each_variable, ".jpeg"), plot = p, width = 8)
      
    } else {
      p <- ggplot(training) +
        geom_bar(aes_string(each_variable, fill = "new_cause"), position = "fill") +
        coord_flip() +
        scale_fill_brewer(palette = "Set1") +
        ggtitle(paste0("Segmented bar chart of ", each_variable))
      
      ggsave(paste0("plots/", each_variable, ".jpeg"), plot = p, width = 8)
    }
  }
}

p <- training %>%
  select_if(is.numeric) %>%
  select(-dist_camp, -dist_road, -dist_cfa) %>%
  ggcorr(nbreaks = 7)

p <- p + ggtitle("Correlation Matrix")

ggsave(paste0("plots/", "corr", ".jpeg"), plot = p, width = 12, height = 8)

