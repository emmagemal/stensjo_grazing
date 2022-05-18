### Figures for LE Project course
### Topic: effect of grazing in Stensjö's thinned forests on species richness
### Emma Gemal, Stockholm University 

## Library ---- 
library(tidyverse)

## Loading the data ----
grazing <- read.csv("Data/full_data.csv", header = T)

str(grazing)   # site and plot are integers
summary(grazing)

### Data manipulation ----
grazing <- grazing %>% 
              mutate(site = as.character(site),
                     plot = as.factor(plot))
str(grazing)


### Boxplots ----
themegz <- theme_bw() +
              theme(panel.grid = element_blank(),
                    axis.title.x = 
                      element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                    axis.title.y = 
                      element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                    legend.position = "none") +
              theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# changing names for site 
grazing <- grazing %>% 
              mutate(site = case_when(site == "1" ~ "Site A",
                                      site == "2" ~ "Site B"))      
str(grazing)

## Species richness vs. grazing ---
(rich_plot <- ggplot(grazing, aes(x = grazing, y = richness)) +
                geom_boxplot(aes(fill = grazing), alpha = 0.8) +
                facet_wrap(~site) +
                ylab("Species richness") +
                xlab("Grazing amount") +
                themegz +
                scale_fill_manual(values = c("#FFAD00", "#BA7BA1")))

ggsave("Figures/richness_grazing.png", plot = rich_plot, height = 5, width = 6, units = "in")

## Average height vs. grazing ---
(avg_plot <- ggplot(grazing, aes(x = grazing, y = avg_height)) +
                geom_boxplot(aes(fill = grazing), alpha = 0.8) +
                facet_wrap(~site) +
                ylab("Mean height (cm)") +
                xlab("Grazing amount") +
                themegz +
                scale_fill_manual(values = c("#FFAD00", "#BA7BA1")))

ggsave("Figures/avgheight_grazing.png", plot = avg_plot, height = 5, width = 6, units = "in")

## Maximum height vs. grazing ---
(tall_plot <- ggplot(grazing, aes(x = grazing, y = tallest)) +
                geom_boxplot(aes(fill = grazing), alpha = 0.8) +
                facet_wrap(~site) +
                ylab("Maximum height (cm)") +
                xlab("Grazing amount") +
                themegz +
                scale_fill_manual(values = c("#FFAD00", "#BA7BA1")))

ggsave("Figures/maxheight_grazing.png", plot = tall_plot, height = 5, width = 6, units = "in")

