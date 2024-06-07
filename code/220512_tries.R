
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, conflicted, ggExtra, ggpubr) 

# conflicts: identical function names from different packages
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lmer", "lmerTest")


# create example dataset
set.seed(123)
df <- tibble(
  SoilID = rep(1:100, each = 3),
  Soil_PH = rnorm(300, 7.5, 0.5),
  soil_Ca = rnorm(300, 10, 2),
  soil_Mg = rnorm(300, 8, 1.5),
  Soil_K = rnorm(300, 20, 4),
  Soil_P = rnorm(300, 20, 5),
  region = rep(LETTERS[1:5], each = 60),
  crop = rep(letters[1:3], each = 20, times = 5),
  pathogen1 = sample(c("Present", "Absent"), 300, replace = TRUE, prob = c(0.3, 0.7)),
  pathogen2 = sample(c("Present", "Absent"), 300, replace = TRUE, prob = c(0.2, 0.8)),
  pathogen3 = sample(c("Present", "Absent"), 300, replace = TRUE, prob = c(0.4, 0.6)),
  disease_severity = rgamma(300, 10, 1)
)

df<- 
  df %>%
  janitor::clean_names() %>%
  mutate_if(is.character, as.factor)

# check the first few rows of the dataset
head(df)
skimr::skim(df)
# run regression analysis
model1 <- lm(disease_severity ~ soil_id + region + crop + pathogen1 + pathogen2 + pathogen3, data = df)

# check model summary
summary(model1)
anova(model1)

# run logistic regression for presence/absence of pathogen2, controlling for pathogen1 and pathogen3
model2 <- glm(pathogen2 ~ pathogen1 + pathogen3 , data = df, family = binomial(link = "logit"))

# check model summary
summary(model2)

df %>%
  pivot_longer(cols = c(pathogen1, pathogen2, pathogen3)) %>%
  filter(value == "Present") %>%
  ggplot(aes(y = disease_severity, x = soil_ca, color = name)) +
    theme_test() +
  stat_smooth(method = "lm", aes(fill= name)) +

    geom_point() + 
  stat_cor(method = "spearman") 
  
  
  