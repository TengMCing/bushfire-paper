# Load library
library(sf)
library(tidyverse)
library(rnaturalearth)
library(GGally)
library(tourr)
library(RColorBrewer)
library(naniar)
library(lubridate)
library(ggridges)

au_map <- ne_states(country = 'Australia', returnclass = 'sf')
vic_map <- au_map[7,]

set.seed(357)

# Read in training data
training <- read_csv("training.csv")
training <- training %>%
  filter(!CAUSE %in% c("BURNING BUILDING", 
                       "WASTE DISPOSAL, INDUSTRIAL, SAWMILL, TIP", 
                       "WASTE DISPOSAL, DOMESTIC", 
                       "BURNING VEHICLE, MACHINE",
                       "BURNING BUILDING"))

training <- select(training, -EVENTID, -FIRE_NAME, -FIRE_DIST, -FIRE_STAT, -FIRE_NUM, -CAUSE, -id, -FOREST)
training <- mutate(training, 
                   log_dist_cfa = log(dist_cfa),
                   log_dist_camp = log(dist_camp),
                   log_dist_road = log(dist_road),
                   COVER = factor(COVER),
                   HEIGHT = factor(HEIGHT),
                   FOR_CODE = factor(FOR_CODE),
                   new_cause = ifelse(new_cause == "relight", "other", new_cause),
                   new_cause = ifelse(new_cause == "acidental_human", "accident", new_cause),
                   new_cause = ifelse(new_cause == "burning_off_human", "burning_off", new_cause))

training <- mutate(training, year = factor(year(FIRE_START)),
                   month = factor(month(FIRE_START)),
                   day = factor(day(FIRE_START)),
                   dow = factor(wday(FIRE_START)))

training <- rename(training, cause = new_cause)
training <- mutate(training, 
                   cause = factor(cause, levels = c("lightning", 
                                                    "accident", 
                                                    "arson", 
                                                    "burning_off", 
                                                    "other")))



p <- vis_miss(training, warn_large_data = FALSE)
ggsave("plots/vis_miss.jpeg", plot = p, width = 14, height = 7)

training <- na.omit(training)

for (each_variable in names(training)){
  if (each_variable == "lon") next
  if (each_variable == "lat"){
    p <- ggplot(training) + 
      geom_density_2d_filled(aes(lon, lat), contour_var = "ndensity") +
      geom_sf(data = vic_map, fill = NA, col = "white") +
      facet_wrap(~cause) +
      theme_bw(base_size = 16) + 
      ggtitle("2D density plot of hotspots ignition")
    
    ggsave(paste0("plots/", "lon_lat", ".jpeg"), plot = p, width = 14, height = 7)
  }
  
  if (each_variable == 'cause'){
    p <- ggplot(training) +
      geom_bar(aes(cause)) +
      theme_bw(base_size = 16) +
      ggtitle("Histogram of cause")
    ggsave(paste0("plots/", "cause", ".jpeg"), plot = p, width = 8, height = 7)
    
  } else {
    if (is.numeric(training[[each_variable]])){
      p <- ggplot(training) + 
        geom_density(aes_string(each_variable, col = "cause"), size = 1)+
        scale_colour_brewer(palette = "Set1") +
        theme_bw(base_size = 16) +
        theme(legend.position = "bottom") + 
        ggtitle(paste0("Density of ", each_variable))
        
      ggsave(paste0("plots/", each_variable, ".jpeg"), plot = p, width = 8, height = 7)
      
      p <- ggplot(training, aes_string(each_variable, "cause")) + 
        geom_density_ridges_gradient(aes(fill = stat(x))) +
        scale_fill_viridis_c(option = "C") +
        theme_bw(base_size = 16) + 
        theme(legend.position = "none") +
        ggtitle(paste0("Ridgeline chart of ", each_variable))
      
      ggsave(paste0("plots/ridges_", each_variable, ".jpeg"), plot = p, width = 8, height = 7)

      
    } else {
      p <- ggplot(training) +
        geom_bar(aes_string(each_variable, fill = "cause"), position = "fill") +
        coord_flip() +
        scale_fill_brewer(palette = "Set1") +
        theme_bw(base_size = 16) +
        theme(legend.position = "bottom") + 
        ggtitle(paste0("Segmented bar chart of ", each_variable))
      
      ggsave(paste0("plots/", each_variable, ".jpeg"), plot = p, width = 8, height = 7)
      
      if (length(unique(training[[each_variable]])) <= 7){
        p <- training %>%
          group_by(!!as.symbol(each_variable), cause) %>%
          summarise(n = n()) %>%
          mutate(freq = n/sum(n)) %>%
          ggplot() + 
          geom_smooth(aes_string(each_variable, "freq", group = "cause", col = "cause"), 
                      size = 1, 
                      se = FALSE) +
          scale_color_brewer(palette = "Set1") +
          theme_bw(base_size = 16) +
          theme(legend.position = "bottom") +
          ggtitle(paste0("Line chart of ", each_variable)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggsave(paste0("plots/line_", each_variable, ".jpeg"), plot = p, width = 8, height = 7)
      } else {
        
        temp_span <- list("day" = 0.3, 
                          "year" = 0.35, 
                          "month" = 0.75, 
                          "FIRE_START" = 0.35, 
                          "FOR_CODE" = 0.35,
                          "FOR_TYPE" = 0.35)[[each_variable]]
        
        p <- training %>%
          group_by(!!as.symbol(each_variable), cause) %>%
          summarise(n = n()) %>%
          mutate(freq = n/sum(n)) %>%
          ggplot() +
          geom_smooth(aes_string(each_variable, "freq", group = "cause", col = "cause"), 
                      size = 1, 
                      se = FALSE,
                      span = temp_span) +
          scale_color_brewer(palette = "Set1") +
          theme_bw(base_size = 16) +
          theme(legend.position = "bottom") +
          ggtitle(paste0("Line chart of ", each_variable)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggsave(paste0("plots/line_", each_variable, ".jpeg"), plot = p, width = 8, height = 7)
      }
      
      
      
      p <- ggplot(training) +
        geom_bar(aes_string(each_variable, fill = "cause")) +
        coord_flip() +
        scale_fill_brewer(palette = "Set1") + 
        theme_bw(base_size = 16) +
        theme(legend.position = "bottom") + 
        ggtitle(paste0("Segmented bar chart of ", each_variable))
      
      ggsave(paste0("plots/", each_variable, "_normal.jpeg"), plot = p, width = 8, height = 7)
    }
  }
}

p <- training %>%
  select_if(is.numeric) %>%
  select(-dist_camp, -dist_road, -dist_cfa) %>%
  ggcorr(nbreaks = 7)

p <- p + ggtitle("Correlation Matrix")

ggsave(paste0("plots/", "corr", ".jpeg"), plot = p, width = 12, height = 8)


# read_VIC_road_map = function(directory = 'data/road_geometry'){
#   fls = list.files(directory, full.names = TRUE)
#   all = map(fls, read_csv, quote="'")
#   for (i in seq(length(all))){
#     all[[i]] <- all[[i]] %>%
#       mutate(DIRECTION = as.character(DIRECTION))
#   }
#   d = bind_rows(all)
#   return(d)
# }
# 
# VIC_road_map = read_VIC_road_map()
# 
# VIC_road_map = VIC_road_map[sample(1:nrow(VIC_road_map), nrow(VIC_road_map)*0.4),]
# 
# p <- ggplot(training) +
#   geom_sf(data = au_map, fill = "grey", colour = "black")+
#   coord_sf(ylim = c(-40,-34), xlim = c(140,150)) +
#   geom_segment(aes(x = S_LONGITUDE, y = S_LATITUDE, xend = E_LONGITUDE, yend = E_LATITUDE), 
#                color = 'blue', 
#                data = VIC_road_map) +
#   geom_point(aes(lon, lat), alpha = 0.3) +
#   facet_wrap(~cause) +
#   theme_bw(base_size = 16) +
#   ggtitle('VIC road map')
# 
# ggsave(paste0("plots/", "road_cause", ".jpeg"), plot = p, width = 12, height = 8)



  
  

  
  
  

