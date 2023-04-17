#### Load library ####
library(tidyverse)

#### Read and format data ####
setwd("~/lifewatch_speciescooccurrence/")
df_merged = read_csv("csv/DPH_final.csv")

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
df_model_cod_porp = df_model %>% 
  filter(!is.na(por_DPH) & !is.na(cod_DPH)) %>%    
  select(-dol_DPH, -sb_DPH) %>% 
  mutate(cooc = case_when(
    (por_DPH == 1 & cod_DPH == 1) ~ "Cooc",
    (por_DPH == 1 & cod_DPH == 0) ~ "Porpoise",
    (por_DPH == 0 & cod_DPH == 1) ~ "Cod",
    (por_DPH == 0 & cod_DPH == 0)~ "none"))


df_model_cod_porp %>% 
  ggplot(aes(day_time, cooc)) +
  geom_point() +
  facet_wrap(~location_col, scales = "free_x")


location_cod10 = df_model_cod_porp %>% 
  group_by(location_col) %>% 
  summarise(cod = sum(cod_DPH),
            por = sum(por_DPH)) %>% 
  filter(cod > 10) %>% 
  distinct(location_col) %>% 
  pull()

df_model_cod_porp = df_model_cod_porp %>% 
  filter(location_col %in% location_cod10)

df_model_cod_porp %>% 
  ggplot(aes(day_time, cooc, colour = Season)) +
  geom_point(size = 0.1) +
  facet_wrap(~location_col, scales = "free_x", nrow = 5)


# Actual data set for modelling
df_model_cod = df_model %>% 
  filter(cod_DPH == 1 & !is.na(por_DPH)) %>% 
  filter(location_col %in% location_cod10) %>% 
  mutate(cooc = ifelse(por_DPH == 1, 1, 0))

df_model_cod %>% 
  ggplot(aes(day_time, as.factor(cooc))) +
  geom_point(size = 0.1) +
  facet_wrap(~location_col, scales = "free_x", nrow = 5)

df_model_cod %>% 
  ggplot(aes(Diurnal, fill = as.factor(cooc) )) +
  geom_bar(position = "dodge") 
df_model_cod %>% 
  ggplot(aes(Season, fill = as.factor(cooc) )) +
  geom_bar(position = "dodge")

df_model_cod %>% 
  ggplot(aes(Diurnal, fill = as.factor(cooc) )) +
  geom_bar(position = "dodge") +
  facet_wrap(~location_col, nrow = 2)
df_model_cod %>% 
  ggplot(aes(Season, fill = as.factor(cooc) )) +
  geom_bar(position = "dodge") +
  facet_wrap(~location_col, nrow = 2)

#### Model preparation ####
# df_model_seabass$hour_i = as.numeric(difftime(df_model_seabass$day_time, min(df_model_seabass$day_time), units = "hours") + 1)

df_model_cod_short = df_model_cod %>% 
  mutate(seasonf = factor(Season),
         dielf = factor(Diurnal),
         statf = factor(location_col)) %>% 
  select(cooc, seasonf, dielf, statf)

#### Model ####
glm_cod = glm(cooc ~ dielf * seasonf, family = "binomial", data = df_model_cod_short)
summary(glm_cod)
drop1(glm_cod, test = "Chisq")

df_output = df_model_cod_short %>% 
  group_by(seasonf, dielf) %>% 
  summarise(DPH = n())

# Get prediction and standard error in log scale
pred.int = predict(glm_cod, df_output, se.fit = T)

# Make function to transform logistic to probability
expfunction = function(x){exp(x)/(1+exp(x))}

# Get prediction + lower and upper confidence interval
df_output$fit = expfunction(pred.int$fit)
df_output$lwr = expfunction(pred.int$fit - (1.96*pred.int$se.fit))
df_output$upr = expfunction(pred.int$fit + (1.96*pred.int$se.fit))

#RELEVEL

df_model_cod_short$seasonf <- relevel(df_model_cod_short$seasonf, ref="March Equinox")
glm_cod = glm(cooc ~ dielf * seasonf, family = "binomial", data = df_model_cod_short)
summary(glm_cod)

df_output$seasonf  <- factor(df_output$seasonf , levels=c("September Equinox", "December Solstice", "March Equinox", "June Solstice"))
df_output$seasonf <- recode_factor(df_output$seasonf,"September Equinox"  = "Autumn", "December Solstice" = "Winter", "March Equinox" = "Spring", "June Solstice"= "Summer")

# Plot output

library(ggsignif)

df_output %>%
  ggplot() +
  geom_pointrange(size = 1.2, aes(seasonf, y = fit, ymin = lwr, ymax = upr, colour = dielf), position=position_dodge(width=0.5)) +
  scale_colour_manual(values = c("day" = "darkgoldenrod1", "night" = "steelblue")) +
  ggtitle("Atlantic cod & Harbour Porpoise") + theme_classic()+
  theme(axis.text.y=element_text(size=12), axis.text.x = element_text(face='bold', size=10), plot.title = element_text(size=15, face='bold'),plot.subtitle = element_text(face='italic')) +
  labs(x = '', y = 'Probability of co-occurrence', colour = "Diel Factor")+
  #geom_text(aes(label = DPH, y = fit, x=seasonf),position = position_dodge(width=0.05), vjust=0, size =2) +
  geom_signif(data = df_output, aes(xmin = c('Autumn'), xmax = c('Spring'), annotations = "***", y_position = 0.78),
              textsize = 4, vjust = 0.5, manual = TRUE)+
  geom_signif(data = df_output, aes(xmin = c('Autumn'), xmax = c('Winter'), annotations = "***", y_position = 0.85),
              textsize = 4, vjust = 0.5, manual = TRUE)+
  scale_y_continuous(limits = c(0,1))

ggsave("plots/GLMM/cod_porp_glmm1.png", device='png', dpi=500, width=6, height=7)
