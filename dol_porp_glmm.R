#### Load library ####
library(tidyverse)

#### Read and format data ####
setwd("~/lifewatch_speciescooccurrence/csv/")
df_merged = read_csv("DPH_final.csv")

df_model = df_merged %>% 
  dplyr::select(
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
df_model_dol_porp = df_model %>% 
  filter(!is.na(por_DPH) & !is.na(dol_DPH)) %>%    
  select(-cod_DPH, -sb_DPH) %>% 
  mutate(cooc = case_when(
    (por_DPH == 1 & dol_DPH == 1) ~ "Cooc",
    (por_DPH == 1 & dol_DPH == 0) ~ "Porpoise",
    (por_DPH == 0 & dol_DPH == 1) ~ "Dolphin",
    (por_DPH == 0 & dol_DPH == 0)~ "none"))


df_model_dol_porp %>% 
  ggplot(aes(day_time, cooc)) +
  geom_point() +
  facet_wrap(~location_col, scales = "free_x")



# drop Belwind, Faulbaums, G88,Grafton,C-power, Nautica ena: < 10 dolphin hours
location_dol = df_model_dol_porp %>% 
  group_by(location_col) %>% 
  summarise(dol = sum(dol_DPH),
            por = sum(por_DPH)) %>% 
  filter(dol > 10) %>% 
  distinct(location_col) %>% 
  pull()

df_model_dol_porp = df_model_dol_porp %>% 
  filter(location_col %in% location_dol)

df_model_dol_porp %>% 
  ggplot(aes(day_time, cooc, colour = Season)) +
  geom_point(size = 0.1) +
  facet_wrap(~location_col, scales = "free_x", nrow = 5)


# Actual data set for modelling
df_model_dol = df_model %>% 
  filter(dol_DPH == 1 & !is.na(por_DPH)) %>% 
  filter(location_col %in% location_dol) %>% 
  mutate(cooc = ifelse(por_DPH == 1, 1, 0))

df_model_dol %>% 
  ggplot(aes(day_time, as.factor(cooc))) +
  geom_point(size = 0.1) +
  facet_wrap(~location_col, scales = "free_x", nrow = 5)

df_model_dol %>% 
  ggplot(aes(Diurnal, fill = as.factor(cooc) )) +
  geom_bar(position = "dodge") 
df_model_dol %>% 
  ggplot(aes(Season, fill = as.factor(cooc) )) +
  geom_bar(position = "dodge")

df_model_dol %>% 
  ggplot(aes(Diurnal, fill = as.factor(cooc) )) +
  geom_bar(position = "dodge") +
  facet_wrap(~location_col, nrow = 2)
df_model_dol %>% 
  ggplot(aes(Season, fill = as.factor(cooc) )) +
  geom_bar(position = "dodge") +
  facet_wrap(~location_col, nrow = 2)

#### Model preparation ####
# df_model_seabass$hour_i = as.numeric(difftime(df_model_seabass$day_time, min(df_model_seabass$day_time), units = "hours") + 1)

df_model_dol_short = df_model_dol %>% 
  mutate(seasonf = factor(Season),
         dielf = factor(Diurnal),
         statf = factor(location_col)) %>% 
  select(cooc, seasonf, dielf, statf)

#### Model ####
glmmint = lme4::glmer(cooc ~ dielf * seasonf  + (1|statf/seasonf), family = "binomial", data = df_model_dol_short)
summary(glmmint)
drop1(glmmint, test = "Chisq")
# remove interaction dielf * seasonf

glmmint = lme4::glmer(cooc ~ dielf + seasonf  + (1|statf/seasonf), family = "binomial", data = df_model_dol_short)
summary(glmmint)
drop1(glmmint, test = "Chisq")
lme4::ranef(glmmint)

glmmint = lme4::glmer(cooc ~ dielf + seasonf  + (1|statf), family = "binomial", data = df_model_dol_short)
summary(glmmint)
drop1(glmmint, test = "Chisq")
lme4::ranef(glmmint)

### GLM MODEL

glm_dol = glm(cooc ~ dielf*seasonf +seasonf+dielf, family = "binomial", data = df_model_dol_short)
summary(glm_dol)
drop1(glm_dol, test = "Chisq")
# remove dielf:seasonf

##FINAL MODEL
glm_dol = glm(cooc ~ seasonf+dielf, family = "binomial", data = df_model_dol_short)
summary(glm_dol)
drop1(glm_dol, test = "Chisq")
#diel and season significant 

#####GLM: BIAS-REDUCED
library(brglm)
glm_dol = brglm(cooc ~ dielf+seasonf, family = binomial(logit), data = df_model_dol_short, method = "brglm.fit")


lme4::ranef(glmmint)


df_output = df_model_dol_short %>% 
  group_by(seasonf,dielf) %>% 
  summarise(DPH = n())

# Get prediction and standard error in log scale
pred.int = predict(glm_dol, df_output, se.fit = T)

# Make function to transform logistic to probability
expfunction = function(x){exp(x)/(1+exp(x))}

# Get prediction + lower and upper confidence interval
df_output$fit = expfunction(pred.int$fit)
df_output$lwr = expfunction(pred.int$fit - (1.96*pred.int$se.fit))
df_output$upr = expfunction(pred.int$fit + (1.96*pred.int$se.fit))

#RELEVEL to see sig diff

df_model_dol_short$seasonf <- relevel(df_model_dol_short$seasonf, ref="September Equinox")
glm_dol = glm(cooc ~ seasonf+dielf, family = "binomial", data = df_model_dol_short)
summary(glm_dol)

df_output$seasonf  <- factor(df_output$seasonf , levels=c("September Equinox", "December Solstice", "March Equinox", "June Solstice"))
df_output$seasonf <- recode_factor(df_output$seasonf,"September Equinox"  = "Autumn", "December Solstice" = "Winter", "March Equinox" = "Spring", "June Solstice"= "Summer")

# Plot output

library(ggsignif)

df_output %>%
  ggplot() +
  geom_pointrange(size = 1.2, aes(seasonf, y = fit, ymin = lwr, ymax = upr, colour=dielf), position=position_dodge(width=0.5)) +
  scale_colour_manual(values = c("day" = "darkgoldenrod1", "night" = "steelblue")) +
  ggtitle("Dolphins & Harbour Porpoise")  +theme_classic()+
  theme(axis.text.y=element_text(size=12), axis.text.x = element_text(face='bold', size =9),plot.title = element_text(size=15, face='bold')) +
  labs(x = '', y = 'Probability of co-occurrence',colour = "Diel Factor")+
  #geom_text(aes(label = DPH, y = fit, x=seasonf),position = position_dodge(width=0.05), vjust=0, size=2) +
  geom_signif(data = df_output, aes(xmin = c('Winter'), xmax = c('Spring'), annotations = "*", y_position = 1),
              textsize = 4,vjust = 0.5, manual = TRUE)+
  scale_y_continuous(limits = c(0,1))

ggsave("plots/GLMM/dol_porp_glmm1.png", device='png', dpi=500, width=6, height=7)

