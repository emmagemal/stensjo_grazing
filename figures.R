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
grazing2 <- grazing %>% 
              mutate(site = case_when(site == "2" ~ "Site A",
                                      site == "1" ~ "Site B"))      
str(grazing)

# making a summarized dataset for plotting 
means1 <- grazing %>% 
            group_by(grazing, site, transect) %>% 
            summarize(avg.rich = mean(richness),
                      avg.height = mean(avg_height),
                      avg.tallest = mean(tallest),
                      sd.rich = sd(richness),
                      sd.height = sd(avg_height),
                      sd.tallest = sd(tallest)) %>% 
            mutate(site = case_when(site == "2" ~ "1 year",
                                    site == "1" ~ "3 years"))
means2 <- means1 %>% 
            group_by(grazing, site) %>% 
            summarize(avg.rich = mean(avg.rich),
                      avg.height = mean(avg.height),
                      avg.tallest = mean(avg.tallest),
                      sd.rich = mean(sd.rich),
                      sd.height = mean(sd.height),
                      sd.tallest = mean(sd.tallest))

## Species richness vs. grazing ---
(rich_plot <- ggplot(grazing2, aes(x = grazing, y = richness)) +
                geom_boxplot(aes(fill = grazing), outlier.shape = 21, alpha = 0.8) +
                facet_wrap(~site) +
                ylab("Species richness") +
                xlab("Grazing presence") +
                themegz +
                scale_fill_manual(values = c("#FFAD00", "#BA7BA1")))

(rich_plot2 <- ggplot(grazing2, aes(x = grazing, y = richness)) +
                  geom_boxplot(aes(fill = grazing), outlier.shape = 21, alpha = 0.8) +
                  ylab("Species richness") +
                  xlab("Grazing presence") +
                  themegz +
                  scale_fill_manual(values = c("#FFAD00", "#F05C00")))

ggsave("Figures/richness_grazing.png", plot = rich_plot, height = 3.5, width = 3.5, units = "in")
ggsave("Figures/richness_overall.png", plot = rich_plot2, height = 3.5, width = 3.5, units = "in")


## Average height vs. grazing ---
(avg_plot <- ggplot(grazing2, aes(x = grazing, y = avg_height)) +
                geom_boxplot(aes(fill = grazing), outlier.shape = 21, alpha = 0.8) +
                facet_wrap(~site) +
                ylab("Mean height (cm)") +
                xlab("Grazing presence") +
                themegz +
                scale_fill_manual(values = c("#FFAD00", "#BA7BA1")))

(avg_plot2 <- ggplot(means2, aes(x = site, y = avg.height, fill = grazing, color = grazing)) +
                geom_bar(position = position_dodge(), stat = "identity", 
                         color = "black", alpha = 0.8) +
                geom_errorbar(aes(ymax = avg.height+sd.height, 
                                  ymin = avg.height-sd.height,
                                  width = 0.2),
                              position = position_dodge(0.9),
                              color = "black") +
                ylab("Mean height (cm)") +
                xlab("Numbers of years grazed") +
                themegz +
                theme(legend.position = "right") +
                scale_color_manual(values = c("#FFAD00", "#F05C00"),
                                   name = "Grazing presence",
                                   labels = c("Grazed", "Ungrazed")) +
                scale_fill_manual(values = c("#FFAD00", "#F05C00"),
                                  name = "Grazing presence",
                                  labels = c("Grazed", "Ungrazed")))

ggsave("Figures/avgheight_grazing.png", plot = avg_plot, height = 3.5, width = 3.5, units = "in")
ggsave("Figures/avgheight_barplot.png", plot = avg_plot2, height = 4, width = 5, units = "in")

## Maximum height vs. grazing ---
(tall_plot <- ggplot(grazing2, aes(x = grazing, y = tallest)) +
                geom_boxplot(aes(fill = grazing), outlier.shape = 21, alpha = 0.8) +
                facet_wrap(~site) +
                ylab("Maximum height (cm)") +
                xlab("Grazing presence") +
                themegz +
                scale_fill_manual(values = c("#FFAD00", "#BA7BA1")))

ggsave("Figures/maxheight_grazing.png", plot = tall_plot, height = 4, width = 4.5, units = "in")

