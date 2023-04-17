#### Load library ####
library(tidyverse)

#### Read and format data ####
setwd("~/lifewatch_speciescooccurrence/")
df = read_csv("csv/DPH_final.csv")

df_model = df %>% 
  select(
    location_col,
    Diurnal,
    Season,
    day_time,
    por_DPH,
    sb_DPH,
    dol_DPH,
    cod_DPH)

#### Inspect and clean data ####
summary(df_model)
# calculate co-occurrence: can't have NA values in DPH

# Visualize over time
df_model_seabass_porp = df_model %>% 
  filter(!is.na(por_DPH) & !is.na(sb_DPH)) %>% 
  select(-dol_DPH, -cod_DPH) %>% 
  mutate(cooc = case_when(
    (por_DPH == 1 & sb_DPH == 1) ~ "Cooc",
    (por_DPH == 1 & sb_DPH == 0) ~ "Porpoise",
    (por_DPH == 0 & sb_DPH == 1) ~ "Seabass",
    (por_DPH == 0 & sb_DPH == 0)~ "none"))


df_model_seabass_porp %>% 
  ggplot(aes(day_time, cooc)) +
  geom_point() +
  facet_wrap(~location_col, scales = "free_x")



# remove Buitenratel, Cpower, G-88, Grafton, Westhinder: < 10 seabass hours
location_sb10 = df_model_seabass_porp %>% 
  group_by(location_col) %>% 
  summarise(sb = sum(sb_DPH),
            por = sum(por_DPH)) %>% 
  filter(sb > 10) %>% 
  distinct(location_col) %>% 
  pull()

df_model_seabass_porp = df_model_seabass_porp %>% 
  filter(location_col %in% location_sb10)

df_model_seabass_porp %>% 
  ggplot(aes(day_time, cooc, colour = Season)) +
  geom_point(size = 0.1) +
  facet_wrap(~location_col, scales = "free_x", nrow = 5)


# Actual data set for modelling
df_model_seabass = df_model %>% 
  filter(sb_DPH == 1 & !is.na(por_DPH)) %>% 
  filter(location_col %in% location_sb10) %>% 
  mutate(cooc = ifelse(por_DPH == 1, 1, 0))

#PLOT

df_model_seabass %>% 
  ggplot(aes(day_time, as.factor(cooc))) +
  geom_point(size = 0.1) +
  facet_wrap(~location_col, scales = "free_x", nrow = 5)

df_model_seabass %>% 
  ggplot(aes(Diurnal, fill = as.factor(cooc) )) +
  geom_bar(position = "dodge") 
df_model_seabass %>% 
  ggplot(aes(Season, fill = as.factor(cooc) )) +
  geom_bar(position = "dodge")

df_model_seabass %>% 
  ggplot(aes(Diurnal, fill = as.factor(cooc) )) +
  geom_bar(position = "dodge") +
  facet_wrap(~location_col, nrow = 2)
df_model_seabass %>% 
  ggplot(aes(Season, fill = as.factor(cooc) )) +
  geom_bar(position = "dodge") +
  facet_wrap(~location_col, nrow = 2)

#### Model preparation ####
# df_model_seabass$hour_i = as.numeric(difftime(df_model_seabass$day_time, min(df_model_seabass$day_time), units = "hours") + 1)

df_model_seabass_short = df_model_seabass %>% 
  mutate(seasonf = factor(Season, levels = c("September Equinox", 'December Solstice', 'March Equinox', 'June Solstice')),
         dielf = factor(Diurnal),
         statf = factor(location_col)) %>% 
  select(cooc, seasonf, dielf, statf)

#### Model ####
glmmint = lme4::glmer(cooc ~ dielf * seasonf  + (1|statf/seasonf), family = "binomial", data = df_model_seabass_short)
summary(glmmint)
drop1(glmmint, test = "Chisq")
# remove interaction dielf * seasonf

glmmint = lme4::glmer(cooc ~ dielf + seasonf  + (1|statf/seasonf), family = "binomial", data = df_model_seabass_short)
summary(glmmint)
drop1(glmmint, test = "Chisq")
lme4::ranef(glmmint)
#singular fit issue

glmmint = lme4::glmer(cooc ~ dielf + seasonf  + (1|statf), family = "binomial", data = df_model_seabass_short)
summary(glmmint)

drop1(glmmint, test = "Chisq")
lme4::ranef(glmmint)

#all terms significant, but need to assess random affect by calculating ICC = (variance)/(variance+pi^2 / 3)

df_output = df_model_seabass_short %>% 
  group_by(seasonf, dielf, statf) %>% 
  summarise()

df_output$pred = predict(glmmint, df_output, type = "response")

df_output %>% 
  ggplot() +
  geom_boxplot(aes(seasonf, pred, colour = dielf)) +
  #geom_point(aes(seasonf, pred, colour = dielf)) +
  labs(x = '', y = 'Probability of co-occurrence')

####GLM: FINAL MODEL

glm_sb = glm(cooc ~ dielf+seasonf, family = "binomial", data = df_model_seabass_short)
summary(glm_sb)
drop1(glm_sb, test = "Chisq")

df_output = df_model_seabass_short %>% 
  group_by(dielf,seasonf) %>% 
  summarise(DPH = n())

#####GLM: BIAS-REDUCED (JUST A TRIAL)
library(brglm)
glm_sb = brglm(cooc ~ dielf+seasonf, family = binomial(logit), data = df_model_seabass_short, method = "brglm.fit")

# Get prediction and standard error in log scale
pred.int = predict(glm_sb, df_output, se.fit = T)

# Make function to transform logistic to probability
expfunction = function(x){exp(x)/(1+exp(x))}

# Get prediction + lower and upper confidence interval
df_output$fit = expfunction(pred.int$fit)
df_output$lwr = expfunction(pred.int$fit - (1.96*pred.int$se.fit))
df_output$upr = expfunction(pred.int$fit + (1.96*pred.int$se.fit))

#Relevel
df_output$seasonf  <- factor(df_output$seasonf , levels=c("September Equinox", "December Solstice", "March Equinox", "June Solstice"))
df_output$seasonf <- recode_factor(df_output$seasonf,"September Equinox"  = "Autumn", "December Solstice" = "Winter", "March Equinox" = "Spring", "June Solstice"= "Summer")

# Plot output

library(ggsignif)

df_output %>%
  ggplot() +
  geom_pointrange(size = 1.2, aes(seasonf, y = fit, ymin = lwr, ymax = upr, colour = dielf), position=position_dodge(width=0.9)) +
  scale_colour_manual(values = c("day" = "darkgoldenrod1", "night" = "steelblue")) +
  ggtitle("European seabass & Harbour Porpoise") +theme_classic()+
  theme(axis.text.y=element_text(size=12), axis.text.x = element_text(face='bold', size = 9),plot.title = element_text(size=15, face='bold')) +
  labs(x = '', y = 'Probability of co-occurrence', colour = "Diel Factor")+
  #geom_text(aes(label = DPH, y = fit, x=seasonf),position = position_dodge(width=0.05), vjust=0, size = 2) +
  geom_signif(data = df_output, aes(xmin = c('Winter'), xmax = c('Spring'), annotations = "*", y_position = 0.7),
    textsize = 4,vjust = 0.5, manual = TRUE) +
  geom_signif(data = df_output, aes(xmin = c('Winter'), xmax = c('Summer'), annotations = "**", y_position = 0.73),
              textsize = 4, vjust = 0.5, manual = TRUE) +
  geom_signif(data = df_output, aes(xmin = c('Autumn'), xmax = c('Spring'), annotations = "***", y_position = 0.77),
              textsize = 4, vjust = 0.5, manual = TRUE) +
  geom_signif(data = df_output, aes(xmin = c('Autumn'), xmax = c('Summer'), annotations = "**", y_position = 0.8),
              textsize = 4, vjust = 0.5, manual = TRUE) +
  scale_y_continuous(limits = c(0,1))

ggsave("plots/GLMM/sb_porp1.png", device='png', dpi=500, width=6, height=7)



