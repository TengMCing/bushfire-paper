# Load library
library(sf)
library(tidyverse)
library(rnaturalearth)
library(GGally)
# library(tourr)
# library(RColorBrewer)
library(naniar)
library(lubridate)
library(ggridges)
library(ggthemes)

au_map <- ne_states(country = 'Australia', returnclass = 'sf')
vic_map <- au_map[7,]

set.seed(357)

# Read in training data
training <- read_csv("data/training.csv")

training <- training %>%
  filter(!CAUSE %in% c("BURNING BUILDING",
                       "WASTE DISPOSAL, INDUSTRIAL, SAWMILL, TIP",
                       "WASTE DISPOSAL, DOMESTIC",
                       "BURNING VEHICLE, MACHINE",
                       "BURNING BUILDING")) %>%
  filter(new_cause != "other") %>%
  filter(new_cause != "relight")


training <- select(training, -c(EVENTID:FIRE_NUM), -id, -CAUSE, -FOREST, -FOR_CODE, -FOR_CAT)

training <- mutate(training,
                   year = factor(year(FIRE_START)),
                   month = factor(month(FIRE_START), levels = c(10,11,12,1,2,3)),
                   day = factor(day(FIRE_START), levels = c(1:31)),
                   wod = factor(wday(FIRE_START), levels = c(1:7)))

training <- filter(training, month %in% c(10,11,12,1,2,3))

p <- vis_miss(training, warn_large_data = FALSE)
ggsave("figures/vis_miss.jpeg", plot = p, width = 14, height = 7)

training <- na.omit(training)

training <- mutate(training, new_cause = ifelse(new_cause == "accidental_human", "accident", new_cause)) %>%
  mutate(new_cause = ifelse(new_cause == "burning_off_human", "burning_off", new_cause)) %>%
  mutate(new_cause = factor(new_cause)) %>%
  mutate(FOR_TYPE = factor(FOR_TYPE))

training <- na.omit(training)

training <- mutate(training,
                   log_dist_cfa = log(dist_cfa),
                   log_dist_camp = log(dist_camp),
                   log_dist_road = log(dist_road),
                   COVER = factor(COVER),
                   HEIGHT = factor(HEIGHT))

training <- rename(training, cause = new_cause)
training <- mutate(training,
                   cause = fct_relevel(cause,
                                       "lightning",
                                       "accident",
                                       "arson",
                                       "burning_off"))

training <- na.omit(training)

for (each_variable in names(training)){
  if (each_variable == "lon") next
  if (each_variable == "lat"){
    p <- ggplot(training) +
      geom_density_2d_filled(aes(lon, lat), contour_var = "ndensity") +
      geom_sf(data = vic_map, fill = NA, col = "white") +
      facet_wrap(~cause) +
      theme_map(base_size = 20)

    ggsave(paste0("figures/", "lon_lat", ".jpeg"), plot = p, width = 16, height = 12)
  }

  if (each_variable == 'cause'){
    p <- ggplot(training) +
      geom_bar(aes(cause)) +
      theme_bw(base_size = 16) +
      ggtitle("Histogram of cause")
    ggsave(paste0("figures/", "cause", ".jpeg"), plot = p, width = 8, height = 7)

  } else {
    if (is.numeric(training[[each_variable]])){
      p <- ggplot(training) +
        geom_density(aes_string(each_variable, col = "cause"), size = 1)+
        scale_colour_brewer(palette = "Set1") +
        theme_bw(base_size = 16) +
        theme(legend.position = "bottom") +
        ggtitle(paste0("Density of ", each_variable))

      ggsave(paste0("figures/", each_variable, ".jpeg"), plot = p, width = 8, height = 7)

      p <- ggplot(training, aes_string(each_variable, "cause")) +
        geom_density_ridges_gradient(aes(fill = stat(x))) +
        scale_fill_viridis_c(option = "C") +
        theme_bw(base_size = 16) +
        theme(legend.position = "none") +
        ggtitle(paste0("Ridgeline chart of ", each_variable))

      ggsave(paste0("figures/ridges_", each_variable, ".jpeg"), plot = p, width = 8, height = 7)


    } else {
      p <- ggplot(training) +
        geom_bar(aes_string(each_variable, fill = "cause"), position = "fill") +
        coord_flip() +
        scale_fill_brewer(palette = "Set1") +
        theme_bw(base_size = 16) +
        theme(legend.position = "bottom") +
        ggtitle(paste0("Segmented bar chart of ", each_variable))

      ggsave(paste0("figures/", each_variable, ".jpeg"), plot = p, width = 8, height = 7)

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

        ggsave(paste0("figures/line_", each_variable, ".jpeg"), plot = p, width = 8, height = 7)
      } else {

        temp_span <- list("day" = 0.35,
                          "year" = 0.5,
                          "month" = 0.75,
                          "FIRE_START" = 0.2,
                          "FOR_CODE" = 0.2,
                          "FOR_TYPE" = 0.2)[[each_variable]]

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

        ggsave(paste0("figures/line_", each_variable, ".jpeg"), plot = p, width = 8, height = 7)
      }



      p <- ggplot(training) +
        geom_bar(aes_string(each_variable, fill = "cause")) +
        coord_flip() +
        scale_fill_brewer(palette = "Set1") +
        theme_bw(base_size = 16) +
        theme(legend.position = "bottom") +
        ggtitle(paste0("Segmented bar chart of ", each_variable))

      ggsave(paste0("figures/", each_variable, "_normal.jpeg"), plot = p, width = 8, height = 7)
    }
  }
}

p <- training %>%
  select_if(is.numeric) %>%
  select(-dist_camp, -dist_road, -dist_cfa) %>%
  ggcorr(nbreaks = 7)

p <- p + ggtitle("Correlation Matrix")

ggsave(paste0("figures/", "corr", ".jpeg"), plot = p, width = 12, height = 8)

hotspots <- read_csv("data/VIC_hotspots_raw.csv")
memberships <- read_csv("data/VIC_hotspots_after_clustering.csv")
hotspots$fire_id <- memberships$fire_id


plotdata <- function(obs){
  temp <- filter(hotspots, fire_id == obs) %>%
    group_by(hour_id) %>%
    summarise(lon = mean(lon), lat = mean(lat)) %>%
    ungroup()

  temp3 <- NULL

  for (i in seq(2137, max(temp$hour_id), 6)){
    temp4 <- filter(hotspots, fire_id == obs, hour_id <= i, hour_id > i-6)
    if (nrow(temp4) == 0){
      temp4 <- filter(hotspots, fire_id == obs, hour_id <= i-6, hour_id > i-12) %>%
        mutate(firepower = NA)
    }
    temp4$label <- i
    if (is.null(temp3)){
      temp3 <- temp4
    } else {
      temp3 <- bind_rows(temp3, temp4)
    }
  }

  temp3 <- mutate(temp3, label = ymd_h("2019-10-01 00") + hours(label))
  temp3$label <- as.character(temp3$label)

  return(temp3)

}

obs <- 395
data1 <- plotdata(obs)

obs <- 390
data2 <- plotdata(obs)

temp2 <- filter(hotspots, fire_id %in% c(390, 395))

data1$firepower <- data1$firepower * -1

temp2$lon <- jitter(temp2$lon)
temp2$lat <- jitter(temp2$lat)

p1 <- bind_rows(data1, data2) %>%
  ggplot() +
  geom_point(data = temp2, aes(lon, lat), col = "grey") +
  geom_point(aes(lon, lat, col = firepower), position = "jitter") +
  scale_color_distiller(palette = "RdYlBu", na.value = "grey") +
  # scale_color_viridis_c(option = "B", direction = -1, na.value = "grey") +
  facet_wrap(~label) +
  theme_map() +
  theme(legend.position = "none")


p2 <- ggplot() +
  geom_sf(data = vic_map) +
  geom_point(data = filter(hotspots, fire_id %in% c(390, 395)),
             aes(lon, lat, col = factor(fire_id))) +
  labs(color = "fire_id") +
  scale_color_manual(values = c("390" = "#a50026", "395" = "#313695")) +
  theme_map()


p3 <- gridExtra::grid.arrange(p2, p1, ncol = 1)

ggsave(paste0("figures/", "fire_mov", ".jpeg"), plot = p3, width = 14, height = 14)


library(patchwork)

p1 <- training %>%
  mutate(cause = factor(tools::toTitleCase(as.character(cause)), levels = c("Lightning", "Accident", "Arson", "Burning_off"))) %>%
  ggplot() +
  geom_histogram(aes(cause, fill = cause), stat = "count") +
  theme_minimal(base_size = 20) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "RdBu") +
  xlab("Cause") +
  ylab("Count")

p2 <- training %>%
  mutate(cause = factor(tools::toTitleCase(as.character(cause)), levels = c("Lightning", "Accident", "Arson", "Burning_off"))) %>%
  ggplot() +
  geom_bar(data = select(training, -cause), aes(as.Date(paste0(as.character(year), "-01-01")))) +
  geom_bar(aes(as.Date(paste0(as.character(year), "-01-01")), fill = cause)) +
  facet_wrap(~cause) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "RdBu") +
  xlab("Year") +
  ylab("Count") +
  scale_x_date(date_labels = "%y")

p3 <- training %>%
  mutate(cause = factor(tools::toTitleCase(as.character(cause)), levels = c("Lightning", "Accident", "Arson", "Burning_off"))) %>%
  ggplot() +
  geom_bar(data = select(training, -cause), aes(month)) +
  geom_bar(aes(month, fill = cause)) +
  facet_wrap(~cause) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "RdBu") +
  xlab("Month") +
  ylab("Count")

p4 <- training %>%
  mutate(cause = factor(tools::toTitleCase(as.character(cause)), levels = c("Lightning", "Accident", "Arson", "Burning_off"))) %>%
  ggplot() +
  geom_bar(data = select(training, -cause), aes(wod)) +
  geom_bar(aes(wod, fill = cause)) +
  facet_wrap(~cause) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "RdBu") +
  xlab("Day of the week") +
  ylab("Count")


p5 <- (p1 + p2) / (p3 + p4)

p5 <- p5 + plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 20))

ggsave(paste0("figures/", "overall_summary", ".png"), plot = p5, width = 16, height = 12, dpi = 600)

p1 <- training %>%
  mutate(cause = factor(tools::toTitleCase(as.character(cause)), levels = c("Lightning", "Accident", "Arson", "Burning_off"))) %>%
  gather(key = "metric", value = "distance", log_dist_cfa, log_dist_road, log_dist_camp, aws_m12) %>%
  mutate(metric = case_when(metric =="log_dist_cfa"~"Log distance to the nearest CFA station (m)",
                            metric =="log_dist_road"~"Log distance to the nearest road (m)",
                            metric =="log_dist_camp"~"Log distance to the nearest recreation site (m)",
                            metric =="aws_m12"~"1-year average wind speed (m/s)")) %>%
  ggplot() +
  geom_density(aes(distance, col = cause, fill = cause), alpha = 0.3) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "bottom") +
  xlab("") +
  scale_color_brewer(palette = "RdBu") +
  facet_wrap(~metric, scales = "free", nrow = 2)


ggsave(paste0("figures/", "overall_density", ".png"), plot = p1, width = 16, height = 12, dpi = 600)

training %>%
  mutate(cause = factor(tools::toTitleCase(as.character(cause)), levels = c("Lightning", "Accident", "Arson", "Burning_off"))) %>%
  ggplot() +
  geom_density_2d_filled(aes(lon, lat), contour_var = "ndensity") +
  geom_sf(data = vic_map, fill = NA, col = "white") +
  facet_wrap(~cause)+
  theme_map(base_size = 20) +
  theme(legend.position = "right",
        plot.margin = unit(c(0, 0, 0, 0), "pt")) +
  guides(fill = guide_legend(reverse = T)) -> p

ggsave(paste0("figures/", "lon_lat", ".png"), plot = p, width = 16, height = 12, dpi = 600)


# training <- mutate(training,
#                    month = as.numeric(month),
#                    day = as.numeric(day),
#                    wod = as.numeric(wod))
#
# set.seed(1086)
#
# training2 <- training[sample(1:nrow(training), 500),]
#
# training2 <- mutate(training2, cause = ifelse(cause == "lightning", "lightning", "other")) %>%
#   mutate(cause = factor(cause, levels = c("lightning", "other")))
#
# pal <- scales::hue_pal()(2)
# col <- pal[as.numeric(training2$cause)]
#
# X11()
# animate_xy(select(training2,
#                   -FIRE_START,
#                   -cause,
#                   -FOR_TYPE,
#                   -COVER,
#                   -HEIGHT,
#                   -year),
#            guided_tour(lda_pp(training2$cause)),
#            col = col,
#            half_range = 1,
#            rescale = TRUE,
#            axes = "off")
#
# set.seed(1086)
#
# training2 <- training[sample(1:nrow(training), 500),]
#
# training2 <- mutate(training2, cause = ifelse(cause == "arson", "arson", "other")) %>%
#   mutate(cause = factor(cause, levels = c("arson", "other")))
#
# pal <- scales::hue_pal()(2)
# col <- pal[as.numeric(training2$cause)]
#
# X11()
# animate_xy(select(training2,
#                   -FIRE_START,
#                   -cause,
#                   -FOR_TYPE,
#                   -COVER,
#                   -HEIGHT,
#                   -year),
#            guided_tour(lda_pp(training2$cause)),
#            col = col,
#            half_range = 1,
#            rescale = TRUE,
#            axes = "off")
#
# set.seed(1086)
#
# training2 <- training[sample(1:nrow(training), 500),]
#
# training2 <- mutate(training2, cause = ifelse(cause == "accident", "accident", "other")) %>%
#   mutate(cause = factor(cause, levels = c("accident", "other")))
#
# pal <- scales::hue_pal()(2)
# col <- pal[as.numeric(training2$cause)]
#
# X11()
# animate_xy(select(training2,
#                   -FIRE_START,
#                   -cause,
#                   -FOR_TYPE,
#                   -COVER,
#                   -HEIGHT,
#                   -year),
#            guided_tour(lda_pp(training2$cause)),
#            col = col,
#            half_range = 1,
#            rescale = TRUE,
#            axes = "off")
#
# set.seed(1086)
#
# training2 <- training[sample(1:nrow(training), 500),]
#
# training2 <- mutate(training2, cause = ifelse(cause == "burning_off", "burning_off", "other")) %>%
#   mutate(cause = factor(cause, levels = c("burning_off", "other")))
#
# pal <- scales::hue_pal()(2)
# col <- pal[as.numeric(training2$cause)]
#
# X11()
# animate_xy(select(training2,
#                   -FIRE_START,
#                   -cause,
#                   -FOR_TYPE,
#                   -COVER,
#                   -HEIGHT,
#                   -year),
#            guided_tour(lda_pp(training2$cause)),
#            col = col,
#            half_range = 1,
#            rescale = TRUE,
#            axes = "off")








