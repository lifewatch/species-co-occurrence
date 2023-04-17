#Pairwise Species Monthly Occupancy

install.packages("EcoSimR")

library("cooccur")
library("textshape")
library("tidyverse")

df_merged = read_csv("csv/DPH_final.csv")

#IF YOU WANT TO FILTER DATAFRAME, both species must be present (any row with NAs removed) & a station must have more than/equal to 10 hours of DPH of both species
location_sb10 = c("bpns-Belwindreefballs-CPOD","bpns-Birkenfels","bpns-Faulbaums","bpns-Gardencity","bpns-Nauticaena")
location_cod10 = c("bpns-Cpowerreefballs-CPOD") #therefore no cooccurrence table 
location_dol = c("bpns-Birkenfels","bpns-Buitenratel","bpns-Gardencity","bpns-Westhinder" )


#SUMMARISE DATA PER MONTH
x <- df_merged %>% #filter(location_col %in% location_dol) %>% 
  group_by(location_col, month) %>% summarise(dol=sum(na.omit((dol_DPH))), por=sum(na.omit((por_DPH))),  dol_por = sum(na.omit((cooccur_dol_por))))
x <- gather(x, species, DPH, 3:5)
x <- spread(x, month, DPH)

#run code for each month
df_cooccur <- df_merged %>% group_by(location_col, month) %>%    # %>% filter(location_col %in% location_cod10)
  summarise(Seabass = if_else(sum(na.omit(sb_DPH)) > 0, 1,0),
            Cod = if_else(sum(na.omit(cod_DPH)) > 0, 1,0),
            HarbourPorpoise = if_else(sum(na.omit(por_DPH)) > 0, 1,0),
            Dolphins = if_else(sum(na.omit(dol_DPH)) > 0, 1,0)) %>% filter(month==2)

df_cooccur <- column_to_rownames(df_cooccur, "location_col") %>% select(-month)
df_cooccur <- t(df_cooccur)

#---co-occur

cooccur_df <- cooccur(mat = df_cooccur, type = "spp_site", thresh = FALSE, spp_names = TRUE)
prob.table(cooccur_df)

summary(cooccur_df)
plot(cooccur_df)

data("finches")
cooccur.finches <- cooccur(mat = finches, type = "spp_site", thresh = TRUE, spp_names = TRUE)
plot(cooccur.finches)

#---ecosim

df_ecosim <- cooc_null_model(df_cooccur,suppressProg=TRUE)
summary(df_ecosim)

skewCScore <- c_score_skew(m=df_cooccur)
skewCScore <- c_score_skew(m = matrix(rbinom(100, 1, 0.5), nrow = 10))

plot(df_ecosim,type="cooc")

#---graph
library(tidyverse)
library(hrbrthemes)

cooccur_table <- read.csv("csv/cooccur.csv", sep =",")

cooccur_table <- gather(cooccur_table, Month, Probability,JAN:DEC, factor_key=TRUE)

# Viz
ggplot(cooccur_table, aes(Month, Species_Pair, fill= Probability)) + 
  geom_tile() + scale_fill_gradient(low="white", high="blue") +
  theme_minimal() + theme(axis.title = element_blank())

ggsave("plots/cooccur.png", device='png', dpi = 300, width=8, height=3)
