### Statistics for LE Project course
### Topic: effect of grazing in Stensjö's thinned forests on species richness
### Emma Gemal, Stockholm University 

## Library ----
library(tidyverse)
library(ggpubr)

## Loading the data ----
grazing <- read.csv("Data/full_data.csv", header = T)

str(grazing)   # site and plot are integers
summary(grazing)

### Data manipulation ----
grazing <- grazing %>% 
              mutate(site = as.character(site),
                     plot = as.factor(plot))
str(grazing)


### Preliminary visualization ---- 
## Species richness ---
# overall grazed vs. ungrazed 
ggplot(grazing, aes(x = grazing, y = richness)) +
  geom_boxplot()

# grazed vs. ungrazed per site (site 1 = more grazed, site 2 = less grazed)
ggplot(grazing, aes(x = grazing, y = richness)) +
  geom_boxplot() +
  facet_wrap(~site)

# per site and per transect
ggplot(grazing, aes(x = grazing, y = richness)) +
  geom_boxplot() +
  facet_wrap(~site + transect)

## Average vegetation height ---
# overall grazed vs. ungrazed 
ggplot(grazing, aes(x = grazing, y = avg_height)) +
  geom_boxplot()

# grazed vs. ungrazed per site (site 1 = more grazed, site 2 = less grazed)
ggplot(grazing, aes(x = grazing, y = avg_height)) +
  geom_boxplot() +
  facet_wrap(~site)

# per site and per transect 
ggplot(grazing, aes(x = grazing, y = avg_height)) +
  geom_boxplot() +
  facet_wrap(~site + transect)

## Tallest vegetation height ---
# overall grazed vs. ungrazed 
ggplot(grazing, aes(x = grazing, y = tallest)) +
  geom_boxplot()

# grazed vs. ungrazed per site (site 1 = more grazed, site 2 = less grazed)
ggplot(grazing, aes(x = grazing, y = tallest)) +
  geom_boxplot() +
  facet_wrap(~site)

# per site and per transect
ggplot(grazing, aes(x = grazing, y = tallest)) +
  geom_boxplot() +
  facet_wrap(~site + transect)

## Average height vs. richness ---
# grazed vs. ungrazed
ggplot(grazing, aes(x = avg_height, y = richness)) +
  geom_smooth(method = "lm") +
  geom_point() +
  facet_wrap(~grazing, scales = "free_x")

# per site
ggplot(grazing, aes(x = avg_height, y = richness)) +
  geom_smooth(method = "lm") +
  geom_point() +
  facet_wrap(~site)

# grazing response per site
ggplot(grazing, aes(x = avg_height, y = richness)) +
  geom_smooth(method = "lm") +
  geom_point() +
  facet_wrap(~site + grazing, scales = "free")


### Summary statistics ----
means <- grazing %>% 
            group_by(grazing, site, transect) %>% 
            summarize(avg.rich = mean(richness),
                      avg.height = mean(avg_height),
                      avg.tallest = mean(tallest),
                      sd.rich = sd(richness),
                      sd.height = sd(avg_height),
                      sd.tallest = sd(tallest),
                      median = median(richness, na.rm = TRUE),
                      IQR = IQR(richness, na.rm = TRUE))

n <- sqrt(20)  # 20 plots per site, 10 per transect
means_site <- means %>% 
                group_by(grazing, site) %>% 
                summarize(avg.rich = mean(avg.rich),
                          avg.height = mean(avg.height),
                          avg.tallest = mean(avg.tallest),
                          se.rich = mean((sd.rich/n)),
                          se.height = mean((sd.height/n)),
                          se.tallest = mean((sd.tallest/n)))
 
N <- sqrt(40)  # 40 plots in total per treatment (grazed vs. ungrazed)
means_total <- means %>% 
                  group_by(grazing) %>% 
                  summarize(avg.rich = mean(avg.rich),
                            avg.height = mean(avg.height),
                            avg.tallest = mean(avg.tallest),
                            se.rich = mean((sd.rich/N)),
                            se.height = mean((sd.height/N)),
                            se.tallest = mean((sd.tallest/N)))


### Statistics ----
## Species richness (total) ---
ggqqplot(grazing$richness)
shapiro.test(grazing$richness)   # p < 0.05 = not normal 

a <- grazing %>% 
        filter(grazing == "grazed")
b <- grazing %>% 
        filter(grazing == "ungrazed")

ggqqplot(a$richness)
shapiro.test(a$richness)   # p < 0.05 = not normal

ggqqplot(b$richness)
shapiro.test(b$richness)   # p < 0.05 = not normal 

# but will never be normal with this type of data = could just use t-test still 
  # is recommended by people on stack overflow at least (to stick with t-test)

# using a Wilcoxon test 
wilcox.test(richness ~ grazing, data = grazing)   # get a warning
  # p = 0.0045, W = 508

t.test(richness ~ grazing, data = grazing)
  # significant differences in richness (more species in ungrazed forest)
  # p = 0.005124, t = -2.8811, df = 77.402
  # grazed mean = 6.05, ungrazed mean = 7.60 species 


## Species richness (site) ---
t.test(richness ~ site, data = grazing)
  # significant differences between sites (fewer species in site 1 than site 2)
  # p = 1.317e-5, t = -4.7054, df = 66.851
  # site 1 mean = 5.65, site 2 mean = 8.00

# despite not being normal, still doing an ANOVA 
ano1 <- lm(richness ~ grazing*site, data = grazing)
summary(ano1)
anova(ano1)
  # no significant interaction = site does not affect grazing's effect on richness 
  # means that grazed vs ungrazed relationship doesn't differ between sites, it's still more
    # species in ungrazed than in grazed for both site 1 and 2

  # grazing: p = 0.001549 (significant effect of grazing)
  # site: p = 3.889e-6 (significant effect of site)
  # interaction: p = 0.4607 (not significant)


## Average vegetation height (total) ---
ggqqplot(grazing$avg_height)
shapiro.test(grazing$avg_height)   # p < 0.05 = not normal 

wilcox.test(avg_height ~ grazing, data = grazing)   # warning message
  # p = 4.858e-5, W = 379

t.test(avg_height ~ grazing, data = grazing)
  # significant difference in vegetation height with grazing (taller in ungrazed)
  # p = 1.822e-5, t = -4.6255, df = 65.243
  # grazed mean = 3.8125, ungrazed mean = 7.9750

## Average vegetation height (site) ---
t.test(avg_height ~ site, data = grazing)
  # no significant difference in vegetation height between sites 
  # p = 0.8733, t = -0.1599, df = 74.828
  # site 1 mean = 5.8125, site 2 mean = 5.9750 

ano2 <- lm(avg_height ~ grazing*site, data = grazing)
summary(ano2)
anova(ano2)
  # significant interaction, so the effect of grazing on height differs between sites 
  
  # grazing: p = 1.051e-5 (significant effect of grazing on vegetation height)
  # site: p = 0.85428 (no effect of site on vegetation height)
  # interaction: p = 0.02526 (grazing's effect on height differs significantly between sites)
    # can see from plots that site 1 = more of a difference between grazed v ungrazed, while
      # site 2 = closer in height between grazed and ungrazed sides 

# testing difference within sites
grazedA <- grazing %>%  
              filter(site == "1")
grazedB <- grazing %>% 
              filter(site == "2")

t.test(avg_height ~ grazing, data = grazedA)   # significant 
                                                  # p = 3.428e-5, t = -4.9597, df = 26.892
t.test(avg_height ~ grazing, data = grazedB)   # not significant 
                                                  # p = 0.09416, t = -1.7215, df = 34.352 


## Relationship between average height and richness ---
# not sure if it makes much ecological sense, but height may be taller for greater richness? 
lm1 <- lm(richness ~ avg_height, data = grazing)
summary(lm1)

lm2 <- lm(richness ~ avg_height*grazing, data = grazing)
summary(lm2)

lm3 <- lm(richness ~ avg_height*grazing*site, data = grazing)
summary(lm3)

lm4 <- lm(richness ~ avg_height*site, data = grazing)
summary(lm4)

AIC(lm1, lm2, lm3, lm4)  # lm4 = lowest AIC but more complex (lm2 is 2nd best I'd say)


## Tallest vegetation height (total) ---
ggqqplot(grazing$tallest)   # pretty good until the end...
shapiro.test(grazing$tallest)  # not normal 

wilcox.test(tallest ~ grazing, data = grazing)  # warning message
  # p = 0.0001941, W = 412.5

t.test(tallest ~ grazing, data = grazing)
  # significant difference in tallest vegetation with grazing (taller in ungrazed)
  # p = 0.000103, t = -4.1323, df = 66.223
  # grazed mean = 19.425, ungrazed mean = 32.575


## Tallest vegetation height (site) ---
t.test(tallest ~ site, data = grazing)
  # no significant difference in tallest plant between sites 

ano3 <- lm(tallest ~ grazing*site, data = grazing)
summary(ano3)
anova(ano3)
  # no significant interaction, so the effect of grazing on tallest vegetation does not 
    # differ between sites 
  # only grazing has a significant effect on maximum vegetation height 

  # grazing: p = 9.139e-5
  # site: p = 0.2748
  # interaction: p = 0.3733

