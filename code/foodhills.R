rm(list = ls())

# packages and libraries --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, conflicted,
               janitor, viridis, ggExtra, ggpubr) 

# conflicts: identical function names from different packages
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

# reading the data --------------------------------------------------------
getwd()

dta <- read.xlsx("Foodhills statistik 29 augusti 2023.xlsx")

dta <- 
  dta %>%
  as_tibble() %>%
  clean_names() %>%
  transmute(
    ca_mg = caal,
    disease_severity = index,
    pathogen = factor(anmarkning))

dta <- 
  dta %>%
  mutate(pathogen_releveled = fct_other(pathogen,
                                        c("Aphanomyces",
                                          "Pythium",
                                          "Pythium + Nematodes",
                                          
                                          "Rhizoctonia",
                                          "Rhizoctonia + Nematodes",
                                          "Rhizoctonia + Pythium",
                                          "Rhizoctonia + Pythium + Nematodes",
                                          "No pathogens found")
                                          ),
         aphanomyces = fct_other(pathogen, "Aphanomyces", other = "No or other pathogens")
         ) 

# t test
dta %>%
  
  ggplot(aes(x = ca_mg, y = disease_severity, group = aphanomyces)) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14, color = "black"),
        panel.border = element_rect(color = "black", fill=NA, size = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_viridis( discrete = T, option = "plasma", direction = 1) + 
  
  stat_cor(method = "pearson") + 
  stat_smooth(method = "lm") + 
  geom_point(aes(fill = aphanomyces ), shape = 21 , size = 3, alpha = 0.5) + 
  facet_grid(~aphanomyces, scales = "free")
  
model <- lm(disease_severity ~ aphanomyces*ca_mg, data = dta)
summary(model)

pred.values <- emmeans(model, specs = ~ aphanomyces*ca_mg, 
                       at=list(ca_mg=seq(min(dta$ca_mg), max(dta$ca_mg), by = 10))) %>%
  as_tibble()

pred.values %>%
  ggplot(aes(x = ca_mg, y = emmean, group = aphanomyces, fill = aphanomyces, color = aphanomyces)) + 
  #geom_point(aes(fill = aphanomyces ), shape = 21 , size = 3, alpha = 0.5) + 
  geom_point(data = dta, 
             aes(x = ca_mg , y = disease_severity, group = aphanomyces),
             shape = 21 , size = 2, alpha = 0.8, color = "black") + 
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, group = aphanomyces), alpha = 0.4, lwd = 0) + 
  geom_line(color = "blue", size = 1) +
  scale_fill_viridis( discrete = T, option = "plasma", direction = 1) + 
  scale_color_viridis( discrete = T, option = "plasma", direction = 1) +
  
  
  theme_bw() + 
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14, color = "black"),
        panel.border = element_rect(color = "black", fill=NA, size = 1.5)) +
  coord_cartesian(ylim = c(0,100))
  scale_y_continuous(limits = c(0,100))


TukeyHSD(model)

anova(model)  
emmeans(model, c("aphanomyces", "ca_mg"))

interaction.plot(dta, aphanomyces, ca_mg, disease_severity)

skimr::skim(dta)
levels(dta$pathogen)
# wrangling ---------------------------------------------------------------


# saving ------------------------------------------------------------------


# extra -------------------------------------------------------------------
