library(tidyverse)

setwd("~/lifewatch_speciescooccurrence/")
df_merged <- read_csv("csv/DPH_final.csv")

#######################
#SEABASS & PORP C-SCORE 
#######################

#Filter df, both species must be > 10 hours each station

df_model_seabass_porp = df_merged %>% 
  filter(!is.na(por_DPH) & !is.na(sb_DPH)) %>% 
  select(-dol_DPH, -cod_DPH) %>% 
  mutate(cooc = case_when(
    (por_DPH == 1 & sb_DPH == 1) ~ "Cooc",
    (por_DPH == 1 & sb_DPH == 0) ~ "Porpoise",
    (por_DPH == 0 & sb_DPH == 1) ~ "Seabass",
    (por_DPH == 0 & sb_DPH == 0)~ "none"))

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

df_CScore <- df_model_seabass_porp %>% group_by(day_time, Season, Diurnal) %>% summarise(Sites = nrow(location_col),
                                                                                         Rsb = if_else(all(is.na(sb_DPH)), NA_real_, sum(sb_DPH, na.rm =TRUE)),
                                                                                         Rpor = if_else(all(is.na(por_DPH)), NA_real_, sum(por_DPH, na.rm =TRUE)),
                                                                                         SSsb_por = sum(cooccur_sb_por, na.rm =TRUE)) %>%
  mutate(CScore_sb_por = if_else((Rsb == 0 & SSsb_por == 0) | (Rpor == 0 & SSsb_por == 0),NA_real_,(Rsb-SSsb_por)*(Rpor-SSsb_por))) %>% 
  mutate(CScore_sb_por=as.factor(CScore_sb_por))

#######################
#DOLPHINS & PORP C-SCORE 
#######################

df_model_dol_porp = df_merged %>% 
  filter(!is.na(por_DPH) & !is.na(dol_DPH)) %>%    
  select(-cod_DPH, -sb_DPH) %>% 
  mutate(cooc = case_when(
    (por_DPH == 1 & dol_DPH == 1) ~ "Cooc",
    (por_DPH == 1 & dol_DPH == 0) ~ "Porpoise",
    (por_DPH == 0 & dol_DPH == 1) ~ "Dolphin",
    (por_DPH == 0 & dol_DPH == 0)~ "none"))

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

df_CScore <- df_model_dol_porp %>% group_by(day_time, Season, Diurnal) %>% summarise(Sites = nrow(location_col),
                                                                                     Rpor = if_else(all(is.na(por_DPH)), NA_real_, sum(por_DPH, na.rm =TRUE)),
                                                                                     Rdol = if_else(all(is.na(dol_DPH)), NA_real_, sum(dol_DPH, na.rm =TRUE)),
                                                                                     SSdol_por = sum(cooccur_dol_por, na.rm =TRUE)) %>%
  mutate(CScore_dol_por = if_else((Rdol == 0 & SSdol_por == 0) | (Rpor == 0 & SSdol_por == 0),NA_real_,(Rdol-SSdol_por)*(Rpor-SSdol_por))) %>% 
  mutate(CScore_dol_por=as.factor(CScore_dol_por))


######################

#check date duplicates
x <- df_CScore[duplicated(df_CScore$day_time), ]

#Visualize densities of C-Score
thm <- theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size =10), axis.text=element_text(size=12), legend.title=element_blank()) 
m <- ggplot(df_CScore) + geom_density(aes(x = CScore_dol_por, color = Diurnal,..count..), size = 0.8) + facet_wrap(~Season, nrow=1) + thm + ggtitle("Dolphin & Harbour Porpoise") + xlab("C-Score") + ylab("hour count")
m <- ggplot_build(m)  # <---- INSTEAD OF `p <- print(m)
head(m$data[[1]], 3)



#scratch plots

ggplot(df_CScore) + geom_density(aes(x = CScore_dol_por, color = Season)) + thm

bwplot(CScore_sb_por~Diurnal | Season, data = df_CScore)


###########################################
#Do all subsets follow a normal distribution?
###########################################

# SB & POR 
tapply(X=df_CScore$CScore_sb_por, INDEX=df_CScore$Season, FUN=shapiro.test) # all groups not normally distributed

df_CScore$CScore_sb_por.log <- log(df_CScore$CScore_sb_por) #log transform data
df_CScore <- df_CScore[df_CScore$CScore_sb_por.log >= 0,]
tapply(X=df_CScore$CScore_sb_por.log, INDEX=df_CScore$Season, FUN=shapiro.test) # all groups not normally distributed

df_CScore$CScore_sb_por.sqrt <- sqrt(df_CScore$CScore_sb_por) #sqrt transform data
df_CScore <- df_CScore[df_CScore$CScore_sb_por.sqrt >= 0,]
tapply(X=df_CScore$CScore_sb_por.sqrt, INDEX=df_CScore$Season, FUN=shapiro.test) # all groups not normally distributed

# COD & POR
tapply(X=df_CScore$CScore_cod_por, INDEX=df_CScore$Season, FUN=shapiro.test) # all groups not normally distributed

df_CScore$CScore_cod_por.log <- log(df_CScore$CScore_cod_por) #log transform data
df_CScore_ <- df_CScore[df_CScore$CScore_cod_por.log >= 0,]
tapply(X=df_CScore_$CScore_cod_por.log, INDEX=df_CScore_$Season, FUN=shapiro.test) # all groups not normally distributed

df_CScore$CScore_cod_por.sqrt <- sqrt(df_CScore$CScore_cod_por) #sqrt transform data
df_CScore_ <- df_CScore[df_CScore$CScore_cod_por.sqrt >= 0,]
tapply(X=df_CScore_$CScore_cod_por.sqrt, INDEX=df_CScore_$Season, FUN=shapiro.test) # all groups not normally distributed

# DOL & POR
tapply(X=df_CScore$CScore_dol_por, INDEX=df_CScore$Season, FUN=shapiro.test) # all groups not normally distributed

df_CScore$CScore_dol_por.log <- log(df_CScore$CScore_dol_por) #log transform data
df_CScore_ <- df_CScore[df_CScore$CScore_dol_por.log >= 0,]
tapply(X=df_CScore_$CScore_dol_por.log, INDEX=df_CScore_$Season, FUN=shapiro.test) # all groups not normally distributed

df_CScore$CScore_dol_por.sqrt <- sqrt(df_CScore$CScore_dol_por) #sqrt transform data
df_CScore_ <- df_CScore[df_CScore$CScore_dol_por.sqrt >= 0,]
tapply(X=df_CScore_$CScore_dol_por.sqrt, INDEX=df_CScore_$Season, FUN=shapiro.test) # all groups not normally distributed

#---PERMANOVA

library(vegan)

df_CScore_ <- df_CScore[!is.na(df_CScore$CScore_cod_por >= 0),c(2,3,11)] %>% group_by(Season, Diurnal) %>% summarise(CScore_cod_por  = paste(CScore_cod_por, collapse =","))
CScore_cod_por <-  read.table(text =  df_CScore_$CScore_cod_por, header = FALSE, sep = ",", fill=TRUE)

CScore_cod_por.dist <- vegdist(CScore_cod_por, method="bray", na.rm=TRUE)

adonis2(CScore_cod_por.dist~Season*Diurnal, data = df_CScore_, na.rm=TRUE)

#---KRUSKAL-WALLIS TEST

kruskal.test(CScore_sb_por ~ Season, data = df_CScore) # insignificant seasonal effect, p-value = 0.07614
kruskal.test(CScore_sb_por ~ Diurnal, data = df_CScore) # significant diurnal effect, p-value = 5.052e-13
kruskal.test(CScore_sb_por ~ interaction(Season,Diurnal), data = df_CScore) #significant interaction effect, p-value = 2.251e-10

pairwise.wilcox.test(df_CScore$CScore_sb_por, df_CScore$Diurnal,
                     p.adjust.method = "BH")

pwc <- pairwise.wilcox.test(df_CScore$CScore_sb_por, interaction(df_CScore$Diurnal,df_CScore$Season),
                            p.adjust.method = "BH")


kruskal.test(CScore_dol_por ~ Season, data = df_CScore) # insignificant seasonal effect, p-value = 0.05694
kruskal.test(CScore_dol_por ~ Diurnal, data = df_CScore) # insignificant diurnal effect, p-value = 0.5903
kruskal.test(CScore_dol_por ~ interaction(Season,Diurnal), data = df_CScore) #significant interaction effect

pairwise.wilcox.test(df_CScore$CScore_dol_por, interaction(df_CScore$Diurnal,df_CScore$Season),
                     p.adjust.method = "BH") #nothing < 0.05

##########################
# KRUSKAL-WALLIS BOX PLOTS
##########################
library(ggsignif)
library(ggbeeswarm)
# devtools::install_github("psyteachr/introdataviz")
library(introdataviz)

df_CScore$Season <- recode_factor(df_CScore$Season,"September Equinox"  = "Autumn", "December Solstice" = "Winter", "March Equinox" = "Spring", "June Solstice"= "Summer")

#sb & por
ggplot(df_CScore, aes(x=Season, y=CScore_sb_por, fill=Diurnal)) + geom_split_violin(trim=FALSE)+ scale_y_reverse()+theme_linedraw()+
  ggtitle("European seabass & Harbour porpoise") +labs(y= "C-score", x=element_blank()) + scale_fill_manual(values=c("yellow", "blue"))+
  geom_signif(y_position = c(0.8,0.8), xmin = c(0.9,1.9), xmax = c(1.1,2.1), annotation = c("***","***"), tip_length = 0)
ggsave("plots/Cscore/cscore_sb_por.png", dpi=300, width=8, height=5)

#dol & por
ggplot(df_CScore, aes(x=Season, y=CScore_dol_por, fill=Diurnal))+ geom_split_violin(trim=FALSE)+scale_y_reverse()+theme_linedraw()+
  ggtitle("Dolphins & Harbour porpoise") +labs(y= "C-score", x=element_blank()) + scale_fill_manual(values=c("yellow", "blue"))

ggsave("plots/Cscore/cscore_dol_por.png", dpi=300, width=8, height=5)

######################
# HORIZONTAL BAR CHART
######################

#fix data frame
df_likert <- df_CScore %>% group_by(Season, Diurnal, CScore_dol_por) %>% summarise(Count = n()) %>% filter(!is.na(CScore_dol_por)) #%>% spread(Diurnal, Count) #count total scores then transform df from long to wide
df_likert$Season <- recode_factor(df_likert$Season,"September Equinox"  = "Autumn", "December Solstice" = "Winter", "March Equinox" = "Spring", "June Solstice"= "Summer")

df_likert %>% filter()
  ggplot(df_likert, aes(x = CScore_dol_por)) +
    geom_col(data = subset(df_likert, Diurnal == "day"), 
             aes(y = Count, fill = 'day')) +
    geom_col(data = subset(df_likert, Diurnal == "night"), 
             aes(y = -Count, fill = 'night')) + 
    scale_fill_manual(values = c("day" = "darkgoldenrod1","night"= "steelblue"))+
    coord_flip() +
    scale_y_continuous(breaks = seq(-4000,4000, by = 10),
                       labels = (c(seq(4000, 0, by = -10), seq(10,4000,by=10))))+
    facet_wrap(~ Season, nrow=1)+ 
    theme_classic()+theme(legend.title= element_blank())+
    ggtitle("Dolphins & Harbour porpoise") +labs(x= "C-score", y ="DPH count")

ggsave("plots/Cscore/cscore_dol_por_bar.png", dpi=300, width=10, height=3) 



