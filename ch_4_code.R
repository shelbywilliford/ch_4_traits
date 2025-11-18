setwd("C:/Users/shelb/OneDrive - UNCG/UNCG/Wilcox Lab/Research/Dissertation/Ch4-Traits")
setwd("C:/Users/User/OneDrive - UNCG/UNCG/Wilcox Lab/Research/Dissertation/Ch4-Traits") #desktop

### To load
library(readr)
library(readxl)
library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(vegan)
library(MetBrewer)
library(wesanderson)
library(ggpubr)
library(forcats)
library(nlme)
library(emmeans)
library(stringr) 
library(vegan)
library(lme4)
library(ggbiplot)

#Standard error function
sefxn = function(x, na.rm=na.rm) {
  SE = sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)]))
  return(SE)
}

#### Data cleaning ####

#Leaf calculations

NExS_main_trait_data <- read_csv("NExS_main-trait-data_2023_sw_JA.csv")
NExS_Leaf_area_2023_Report_cleaned_sw <- read_csv("NExS_Leaf_area_2023_Report_cleaned_sw_2-12.csv")

leaf_area <- NExS_Leaf_area_2023_Report_cleaned_sw %>% 
  select(block, plot, plant_tag, species, leaf, total_area) %>% 
  mutate(leaf = as.numeric(leaf)) %>% 
  mutate(block = as.factor(block)) %>% 
  drop_na(plant_tag) %>% #drop non-doms
  filter(!row_number() %in% c(720:749)) #drop tagged non-doms from block A

leaf_traits <- NExS_main_trait_data %>% 
  select(!(tiller_num:diameter_t3)) %>% 
  select(!(year:month)) %>% 
  pivot_longer(
    cols = !c(block:species), 
    names_to = c(".value", "Leaf"), 
    names_sep = "_l", 
    values_drop_na = TRUE
  ) %>% 
  mutate(leaf = as.numeric(Leaf)) %>% 
  mutate(block = as.factor(block))

leaf_calculations <- leaf_area %>% 
  left_join(leaf_traits) %>% #change join to only keep tagged plants
  mutate(SLA = total_area/dry_mass) %>% #SLA
  mutate(LDMC = (dry_mass*1000)/wet_mass) %>% #Check LDMC calculations
  select(!(Leaf)) 

#pop demo and traits
NExS_plot_key <- read_csv("NExS_plot_key.csv") %>% 
  mutate(plot = Plot,
         block = Block)

NExS_traits <- read_csv("NExS_main-trait-data_2023_sw.csv") %>% 
  mutate(month = 3)

x23_pop_demo <- read_excel("NExS_pop-demo_late_2023_completed.xlsx") 

x23_pop_demo %>% 
  rename(flower_len = flower_lenght,
         veg_height = veg_hight) %>% 
  select(!("...16"))

x24_pop_demo <- read_csv("2024_NExS_pop_demo_March_cleaned for analysis.csv") %>% 
  select(!('...17')) %>% 
  mutate(block = Block,
         plot = Plot,
         plant_tag = Tag) %>% 
  select(!(Block:Plot)) %>% 
  select(!(Tag)) %>% 
  mutate(month = 3) %>% 
  rename(aerial1 = arial1,
         aerial2 = arial2) %>% 
  mutate(species = dplyr::recode(species,
                                 "ari_con" = "ari con",
                                 "the_tri" = "the tri",
                                 "bot_rad" = "bot rad",
                                 "pan_col" = "pan col",
                                 "pan_max" = "pan max"
  )) 

full_pop_demo_mar <- x24_pop_demo %>% 
  full_join(x23_pop_demo)
write_csv(full_pop_demo_mar, "x23_24_pop_demo_mar.csv")
  
roots <- read_csv("2023_SRL.csv") %>% 
  select(plant_tag, SRL) %>% 
  filter(plant_tag != 22)

mean_leaf_calculations <- leaf_calculations %>% 
  group_by(block, plot, species, plant_tag) %>% 
  summarize((across(.cols=c(total_area, thick, SLA, LDMC),
                    .fns=list(mean=mean),
                    na.rm=T))) %>% 
  ungroup() %>% 
  rename(total_area = total_area_mean,
         thickness = thick_mean,
         SLA = SLA_mean,
         LDMC = LDMC_mean)
write_csv(leaf_calculations, "leaf_calculations_2-12.csv")

#x23_data_full <- NExS_traits %>% 
 # left_join(x23_pop_demo, by = 'plant_tag')

### Species Comp ###
NExS_WholePlot_2023 <- read_csv("NExS_SpComp_WholePlot_2023.csv") %>% 
  separate(Species,into=c("Sp", "Gn"), 
           sep=" ", convert = TRUE) %>% 
  filter(Sp == c('Aristida', 'Bothriochloa', 'Themeda', 'Panicum')) %>% 
  unite("Species", Sp:Gn, remove = FALSE) %>% 
  mutate( Abundance = (as.numeric(Abundance))) %>% 
  filter(Abundance > 0) %>% 
  group_by(Block, Species) %>% 
  summarize((across(.cols=c(Abundance),
                    .fns=list(mean=mean),
                    na.rm=T))) %>% 
  ungroup() 

ggplot(data=NExS_WholePlot_2023, aes(x=Species, y=Abundance_mean, fill=Species)) +
  geom_bar(stat="identity") +
  facet_wrap(~Block) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(text = element_text(size = 25)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = 'bottom') +
  labs(title = '2023 Species Comp')

NExS_WholePlot_2024 <- read_csv("NExS_SpComp_2024.csv") %>% 
  filter(Subplot == 'whole') %>%
  select(!(Notes)) %>% 
  pivot_longer(!Year:Species, names_to = "Season", values_to = "Abundance") %>% 
  separate(Species,into=c("Sp", "Gn"), 
           sep=" ", convert = TRUE) %>% 
  filter(Sp == c('Aristida', 'Bothriochloa', 'Themeda', 'Panicum')) %>% 
  unite("Species", Sp:Gn, remove = FALSE) %>% 
  mutate( Abundance = (as.numeric(Abundance))) %>% 
  filter(Abundance > 0) %>% 
  group_by(Block, Species) %>% 
  summarize((across(.cols=c(Abundance),
                    .fns=list(mean=mean),
                    na.rm=T))) %>% 
  ungroup() 

ggplot(data=NExS_WholePlot_2024, aes(x=Species, y=Abundance_mean, fill=Species)) +
  geom_bar(stat="identity") +
  ylim(0,100) +
  facet_wrap(~Block) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(text = element_text(size = 25)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = 'bottom') +
  labs(title = '2024 Species Comp')

NExS_WholePlot_2025 <- read_excel("NExS_SpComp_2025.xlsx") %>%
  filter(Subplot == 'whole') %>%
  select(!(Jan_Inside)) %>% 
  select(!(March_Inside)) %>% 
  select(!(Notes)) %>% 
  mutate(March_Outside = as.numeric(March_Outside)) %>% 
  pivot_longer(!Year:Species, names_to = "Season", values_to = "Abundance") %>% 
  separate(Species,into=c("Sp", "Gn"), 
           sep=" ", convert = TRUE) %>% 
  filter(Sp == c('Aristida', 'Bothriochloa', 'Themeda', 'Panicum')) %>% 
  unite("Species", Sp:Gn, remove = FALSE) %>% 
  mutate( Abundance = (as.numeric(Abundance))) %>% 
  filter(Abundance > 0) %>% 
  group_by(Block, Species) %>% 
  summarize((across(.cols=c(Abundance),
                    .fns=list(mean=mean),
                    na.rm=T))) %>% 
  ungroup() 

ggplot(data=NExS_WholePlot_2025, aes(x=Species, y=Abundance_mean, fill=Species)) +
  geom_bar(stat="identity") +
  ylim(0,100) +
  facet_wrap(~Block) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(text = element_text(size = 25)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position = 'bottom') +
  labs(title = '2025 Species Comp')

### ANPP ###
ANPP_raw <- read_csv("NExS_ANPP_2024_sw.csv")

ANPP <- ANPP_raw %>% 
  select(!(Year:Month_num)) %>% 
  select(!("notes":"Elijah data fix notes")) %>% 
  replace(is.na(.), 0) %>% 
  filter(drop == 0) %>% 
  group_by(Block, Plot, Clipping_subplot) %>% 
  dplyr::summarize((across(.cols=c(total),
                    .fns=list(sum=sum), 
                    na.rm=T))) %>% 
  ungroup() %>% 
  group_by(Block, Plot) %>% 
  summarize((across(.cols=c(total_sum),
                    .fns=list(mean=mean, sefxn = sefxn), 
                    na.rm=T))) %>% 
  full_join(NExS_plot_key) %>% 
  unite(treatment, Drought,Grazing) %>% 
  mutate(block = Block,
         plot = Plot) %>% 
  mutate(ANPP = total_sum_mean,
         ANPP_SE = total_sum_sefxn) %>% 
  ungroup()

block_totals <- ANPP %>% 
  select(!(Fire)) %>% 
  group_by(Block, treatment, DomGrass) %>% 
  summarize((across(.cols=c(total_sum_mean),
                    .fns=list(mean=mean),
                    na.rm=T))) %>% 
  pivot_wider(names_from = treatment, values_from = total_sum_mean_mean)

response_ratios <- block_totals %>% 
  mutate(
    drought = log(D_NG/ND_NG),
    graze = log(ND_G/ND_NG),
    both = log(D_G/ND_NG)) %>% 
  select(!(D_G:ND_NG)) %>% 
  pivot_longer(!c(Block, DomGrass), names_to = "treatment", values_to = "mean")

### Overall averages per treatment ###
treatment_ratios <- response_ratios %>% 
  group_by(treatment) %>% 
  summarize((across(.cols=c(mean),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T))) %>% 
  ungroup()

ratio_plot <- ggplot(data=treatment_ratios, aes(x=mean_mean, y=treatment, xmin=mean_mean-mean_sefxn, xmax=mean_mean+mean_sefxn, color = treatment)) +
  geom_errorbarh(height=0) +
  geom_point() +
  geom_vline(xintercept = 0)
ratio_plot

### Block level ###
blocks <- ANPP %>% 
  group_by(Block, treatment, Fire) %>% 
  summarize((across(.cols=c(total_sum_mean),
                    .fns=list(mean=mean),
                    na.rm=T))) %>% 
  pivot_wider(names_from = treatment, values_from = total_sum_mean_mean) 

block_response <- blocks %>% 
  group_by(Block, Fire) %>% 
  mutate(
    drought = log(D_NG/ND_NG),
    graze = log(ND_G/ND_NG),
    both = log(D_G/ND_NG)
  ) %>% 
  select(!(D_G:ND_NG)) %>% 
  pivot_longer(!c(Block:Fire), names_to = "treatment", values_to = "mean")

block_treatment_ratios <- block_response %>% 
  group_by(Block, treatment) %>% 
  summarize((across(.cols=c(mean),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T))) %>% 
  ungroup()

block_ratio_plot <- ggplot(data=block_treatment_ratios, aes(x=mean_mean, y=treatment, xmin=mean_mean-mean_sefxn, xmax=mean_mean+mean_sefxn, color = treatment)) +
  geom_point() +
  geom_errorbarh(height=0) +
  geom_vline(xintercept = 0) +
  facet_wrap(~Block)
block_ratio_plot

#plot level 
ANPP_plot <- ANPP %>% 
  full_join(NExS_plot_key) %>% 
  select(block, plot, Drought, Grazing, ANPP, ANPP_SE)

x23_traits_plot <- mean_leaf_calculations %>% 
  left_join(roots) %>% 
  group_by(block, plot, species) %>% 
  summarize((across(.cols=c(total_area:SRL),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T))) %>% 
  ungroup() %>% 
  full_join(ANPP_plot) %>% 
  unite(treatment, Drought, Grazing)

x23_traits_indiv <- mean_leaf_calculations %>% 
  left_join(roots) %>% 
  left_join(NExS_traits) %>% 
  left_join(NExS_plot_key) %>% 
  unite(treatment, Drought, Grazing) 

x23_pop_demo_jan <- read_csv("NExS_pop-demo_early_2023_Report.csv")

x23_full_data_jan <- x23_pop_demo_jan %>% 
  select(!(month)) %>% 
  right_join(x23_traits_indiv) %>% 
  mutate(basal_area = 3.14 * basal1 * basal2)

SLA_density <- ggplot(data=mean_leaf_calculations, aes(x=SLA, group=species, fill=species)) +
  geom_density(adjust=1.5, alpha=.4) 
SLA_density

#### Meeting w/ Kevin ####

#Overall Response ratios
ANPP_raw <- read_csv("NExS_ANPP_2024_sw.csv")

ANPP <- ANPP_raw %>% 
  select(!(Year:Month_num)) %>% 
  select(!("notes":"Elijah data fix notes")) %>% 
  replace(is.na(.), 0) %>% 
  filter(drop == 0) %>% 
  group_by(Block, Plot, Clipping_subplot) %>% 
  summarize((across(.cols=c(total),
                    .fns=list(sum=sum), 
                    na.rm=T))) %>% 
  ungroup() %>% 
  group_by(Block, Plot) %>% 
  summarize((across(.cols=c(total_sum),
                    .fns=list(mean=mean, sefxn = sefxn), 
                    na.rm=T))) %>% 
  full_join(NExS_plot_key) %>% 
  unite(treatment, Drought,Grazing) %>% 
  mutate(block = Block,
         plot = Plot) %>% 
  mutate(ANPP = total_sum_mean,
         ANPP_SE = total_sum_sefxn) %>% 
  ungroup()

block_totals <- ANPP %>% 
  select(!(Fire)) %>% 
  group_by(Block, treatment, DomGrass) %>% 
  summarize((across(.cols=c(total_sum_mean),
                    .fns=list(mean=mean),
                    na.rm=T))) %>% 
  pivot_wider(names_from = treatment, values_from = total_sum_mean_mean)

response_ratios <- block_totals %>% 
  mutate(
    drought = log(D_NG/ND_NG),
    graze = log(ND_G/ND_NG),
    both = log(D_G/ND_NG)
  ) %>% 
  select(!(D_G:ND_NG)) %>% 
  pivot_longer(!c(Block,DomGrass), names_to = "treatment", values_to = "mean")

### Overall averages per treatment ###
treatment_ratios <- response_ratios %>% 
  group_by(treatment) %>% 
  summarize((across(.cols=c(mean),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T))) %>% 
  ungroup() %>% 
  mutate(Treatment = dplyr::recode(treatment,
                                   "both" = "Drought & Grazing",
                                   "drought" = "Drought",
                                   "graze" = "Grazing"
  ))

ratio_plot <- treatment_ratios %>% 
  mutate(Treatment = fct_relevel(Treatment, 
                                 'Drought & Grazing', 'Grazing', "Drought")) %>% 
  ggplot(aes(x=mean_mean, y=Treatment, xmin=mean_mean-mean_sefxn, xmax=mean_mean+mean_sefxn, color = Treatment)) +
  scale_colour_manual(breaks=c('Drought', 'Grazing', 'Drought & Grazing'), values = met.brewer('Isfahan2',
                                          n=3)) +
  geom_errorbarh(height=0, linewidth = 1) +
  geom_point(size = 4) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(axis.title.y=element_blank()) +
  theme(text = element_text(size = 40)) +
  theme(legend.text = element_text(size = 30)) +
  theme(legend.position = 'bottom') +
  labs(title = 'ANPP Response Ratios', x = 'Response Ratios', y = 'Treatment') 
ratio_plot
#SSNM_treat_RR 1200x1000

### Block level ###
blocks <- ANPP %>% 
  group_by(Block, treatment, Fire, DomGrass) %>% 
  summarize((across(.cols=c(total_sum_mean),
                    .fns=list(mean=mean),
                    na.rm=T))) %>% 
  pivot_wider(names_from = treatment, values_from = total_sum_mean_mean) 

block_response <- blocks %>% 
  group_by(Block, Fire, DomGrass) %>% 
  mutate(
    drought = log(D_NG/ND_NG),
    graze = log(ND_G/ND_NG),
    both = log(D_G/ND_NG)
  ) %>% 
  select(!(D_G:ND_NG)) %>% 
  pivot_longer(!c(Block,Fire, DomGrass), names_to = "treatment", values_to = "mean")

block_treatment_ratios <- block_response %>% 
  group_by(DomGrass, treatment) %>% 
  summarize((across(.cols=c(mean),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T))) %>% 
  ungroup() %>% 
  mutate(Treatment = dplyr::recode(treatment,
                                   "both" = "Drought & Grazing",
                                   "drought" = "Drought",
                                   "graze" = "Grazing"
  )) %>% 
  mutate(treat_num = dplyr::recode(treatment,
                                   "both" = "Combined",
                                   "drought" = "Single",
                                   "graze" = "Single"
  )) %>% 
  mutate(Species = dplyr::recode(DomGrass,
                                 "ari_con" = "Aristida congesta",
                                 "the_tri" = "Themeda triandra",
                                 "bot_rad" = "Bothriochloa radicans",
                                 "pan_col" = "Panicum coloratum", 
                                 "pan_max" = "Megathyrsus maximus"))

treat_num <- block_treatment_ratios %>% 
  group_by(Species, treat_num) %>% 
  summarize((across(.cols=c(mean_mean),
                    .fns=list(mean=mean),
                    na.rm=T))) %>% 
  pivot_wider(names_from = treat_num, values_from = mean_mean_mean) %>% 
  mutate(Differences = abs(Combined-Single))

treat_num_plot <- treat_num  %>% 
  ggplot(aes(x=reorder(Species, -Differences), y=Differences, color = Species)) +
  geom_point(size = 20) +
  scale_color_brewer(palette = "Set2") +
  theme_bw() +
  theme(text = element_text(size = 45)) +
  theme(legend.text = element_text(size = 30)) +
  theme(legend.position = 'none') +
  labs(title = 'Differences in Response Ratios between \nSingle and Compounding events', x = 'Species', y = 'Differences in Response Ratios') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
treat_num_plot
#SSNM_diff 1200x1200

block_ratio_plot <- block_treatment_ratios %>% 
  mutate(Treatment = fct_relevel(Treatment, 
                                 'Drought & Grazing', 'Grazing', "Drought")) %>% 
  ggplot(aes(x=mean_mean, y=Treatment, xmin=mean_mean-mean_sefxn, xmax=mean_mean+mean_sefxn, color = Treatment)) +
  scale_colour_manual(breaks=c('Drought', 'Grazing', 'Drought & Grazing'),values = met.brewer('Isfahan2',
                                          n=3)) +
  geom_point(size = 4) +
  geom_errorbarh(height=0, linewidth= 1) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(axis.title.y=element_blank()) +
  theme(text = element_text(size = 40)) +
  theme(legend.text = element_text(size = 30)) +
  theme(legend.position = 'bottom') +
  labs(title = 'ANPP Response Ratios by Species', x = 'Response Ratios', y = 'Treatment')  +
  facet_wrap(~Species)
block_ratio_plot
#SSNM_species_RR 2000x1000

block_pop_demo <- full_pop_demo_mar %>% 
  full_join(NExS_plot_key) %>% 
  unite(treatment, Drought, Grazing) %>% 
  select(!(Block:Drt_pair)) %>% 
  select(!(Fire:"Install Date")) %>% 
  group_by(block, plot, treatment, year) %>% 
  summarize((across(.cols=c(veg_height:vol_density),
                    .fns=list(mean=mean),
                    na.rm=T))) %>% 
  ungroup()

respo_ratio_w_traits <-block_pop_demo %>% 
  mutate(treatment = dplyr::recode(treatment,
                                 "both" = "D_G",
                                 "drought" = "D_NG",
                                 "graze" = "ND_G"
  )) %>% 
  rename(RR = mean_mean,
         RR_sefxn = mean_sefxn) %>% 
  left_join(x23_traits_plot) %>% 
  mutate(Treatment = dplyr::recode(treatment,
                                   "D_G" = "Drought & Grazing",
                                   "D_NG" = "Drought",
                                   "ND_G" = "Grazing"
  )) %>% 
  group_by(block, Treatment, year) %>% 
  summarize((across(.cols=c(veg_height:vol_density),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T))) %>% 
  ungroup()

SLA_RR_plot <- ggplot(data=respo_ratio_w_traits, aes(x=SRL_mean, y=RR, color = Treatment)) +
  geom_point(alpha=0.75, size = 4) +
  geom_smooth(method=lm,
              se=FALSE) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgray", linewidth=1.5) +
  scale_colour_manual(breaks=c('Drought', 'Grazing', 'Drought & Grazing'), values = met.brewer('Isfahan2', n=3)) +
  theme_bw() +
  labs(title = 'Ecosystem Response: ANPP Response Ratio by Plot', x = 'Average SRL (mm/g) per Plot', y = 'Response Ratio') +
  theme(text = element_text(size = 30)) +
  theme(legend.text = element_text(size = 30)) +
  theme(legend.position = 'top') 
SLA_RR_plot 
#SSNM_RR_indiv #1400x800

#Isfahan2 = ["#34b6c6","#ddc000","#79ad41","#4063a3","#d7aca1"]

### 24/23 Tillers 
rr_tiller_24_23 <- full_pop_demo_mar %>% 
  mutate(year = paste('x', year, sep = "_")) %>% 
  select(year, tiller_num, block, plot,plant_tag) %>% 
  pivot_wider(names_from = year, values_from = tiller_num) %>% 
  mutate(RR = log (x_2024/x_2023)) %>% 
  left_join(x23_traits_indiv) %>% 
  filter(!is.na(species)) %>% 
  mutate(Treatment = dplyr::recode(treatment,
                                        "D_G" = "Drought & Grazing",
                                        "D_NG" = "Drought",
                                        "ND_G" = "Grazing",
                                        "ND_NG" = "Control")) %>% 
  mutate(Species = dplyr::recode(species,
                                 "ari con" = "Aristida congesta",
                                 "the tri" = "Themeda triandra",
                                 "bot rad" = "Bothriochloa radicans",
                                 "pan col" = "Panicum coloratum", 
                                 "pan max" = "Megathyrsus maximus")) %>% 
  group_by(block, Treatment) %>% 
  summarize((across(.cols=c(RR, SLA),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T))) %>% 
  ungroup() %>% 
  filter(!(Treatment == "Control")) %>% 
  filter(!(Treatment =="Drought & Grazing"))

rr_tiller_max <- rr_tiller_24_23 %>% 
  group_by(block) %>% 
  summarise(across(RR_mean, max)) %>% 
  mutate(Treatment = "Single")
  
rr_tiller_combined <- full_pop_demo_mar %>% 
  mutate(year = paste('x', year, sep = "_")) %>% 
  select(year, tiller_num, block, plot,plant_tag) %>% 
  pivot_wider(names_from = year, values_from = tiller_num) %>% 
  mutate(RR = log (x_2024/x_2023)) %>% 
  left_join(x23_traits_indiv) %>% 
  filter(!is.na(species)) %>% 
  mutate(Treatment = dplyr::recode(treatment,
                                   "D_G" = "Combined",
                                   "D_NG" = "Drought",
                                   "ND_G" = "Grazing",
                                   "ND_NG" = "Control")) %>% 
  mutate(Species = dplyr::recode(species,
                                 "ari con" = "Aristida congesta",
                                 "the tri" = "Themeda triandra",
                                 "bot rad" = "Bothriochloa radicans",
                                 "pan col" = "Panicum coloratum", 
                                 "pan max" = "Megathyrsus maximus")) %>% 
  group_by(block, Treatment) %>% 
  summarize((across(.cols=c(RR, SLA),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T))) %>% 
  ungroup() %>% 
  filter((Treatment == "Combined")) %>% 
  full_join(rr_tiller_max) %>% 
  select(block, Treatment, RR_mean) %>% 
  pivot_wider(names_from = Treatment, values_from = RR_mean) %>% 
  mutate(Difference= Combined - Single) %>% 
  full_join(SLA_block_avg)

ggplot(data=rr_tiller_combined, aes(x=SLA_mean, y=Difference, color = block)) +
  geom_point() +
  theme_bw() +
  labs(title = 'Response Ratio by SLA', x = 'SLA (cm/g) per Individual', y = 'RR Difference') 

#### SLA by RR num ####
SLA_block_plot_RR <- ggplot(data=rr_tiller_24_23, aes(x=SLA_mean, y=RR_mean, color = block)) +
  geom_point() +
  geom_errorbar(aes(ymin=RR_mean - RR_sefxn, ymax=RR_mean + RR_sefxn)) +
  geom_errorbarh(aes(xmin=SLA_mean - SLA_sefxn, xmax=SLA_mean + SLA_sefxn)) +
  facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response  Ratio by SLA', x = 'SLA (cm/g) per Individual', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(text = element_text(size = 50)) +
  theme(legend.text = element_text(size = 35)) 
SLA_block_plot_RR

SLA_indiv_plot <- ggplot(data=rr_tiller_24_23, aes(x=SLA_mean, y=RR_mean, color = block)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgray", linewidth=2) +
  geom_point(size = 6, alpha=0.75) +
  geom_smooth(method=lm,
              se=FALSE, linewidth = 1.75) +
  facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Individual Response: Tiller Number Response  Ratio by SLA', x = 'SLA (cm/g) per Individual', y = 'Response Ratio') +
    theme_bw() +
  theme(legend.position = "bottom") +
  theme(text = element_text(size = 50)) +
  theme(legend.text = element_text(size = 35)) 
SLA_indiv_plot
#SSNM_SRL_indiv 3000x1300

SLA_block_avg_treat <- full_pop_demo_mar %>% 
  left_join(NExS_plot_key) %>% 
  unite(treatment, Drought, Grazing) %>% 
  left_join(leaf_calculations) %>% 
  mutate(Treatment = dplyr::recode(treatment,
                                   "D_G" = "Drought & Grazing",
                                   "D_NG" = "Drought",
                                   "ND_G" = "Grazing",
                                   "ND_NG" = "Control")) %>% 
  filter(!(Treatment =="Control")) %>% 
  group_by(block, Treatment) %>% 
  summarize((across(.cols=c(SLA, tiller_num),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T))) %>% 
  ungroup()

#### SLA by tiller num ####
SLA_block_plot <- ggplot(data=SLA_block_avg_treat, aes(x=SLA_mean, y=tiller_num_mean, color = block)) +
  geom_point() +
  geom_errorbar(aes(ymin=tiller_num_mean - tiller_num_sefxn, ymax=tiller_num_mean + tiller_num_sefxn), width=0.25) +
  geom_errorbarh(aes(xmin=SLA_mean - SLA_sefxn, xmax=SLA_mean + SLA_sefxn), height=0.25) +
  facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', "Drought & Grazing"))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number by SLA', x = 'SLA (cm/g) per Individual', y = 'Tiller Number') +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(text = element_text(size = 50)) +
  theme(legend.text = element_text(size = 35)) 
SLA_block_plot 

SLA_treat_max <- SLA_block_avg_treat %>% 
  group_by(block) %>% 
  summarise(across(tiller_num_mean, max)) %>% 
  mutate(Treatment = "Single")

SLA_combined <- full_pop_demo_mar %>% 
  left_join(NExS_plot_key) %>% 
  unite(treatment, Drought, Grazing) %>% 
  left_join(leaf_calculations) %>% 
  mutate(Treatment = dplyr::recode(treatment,
                                   "D_G" = "Combined",
                                   "D_NG" = "Drought",
                                   "ND_G" = "Grazing",
                                   "ND_NG" = "Control")) %>% 
  filter((Treatment =="Combined")) %>% 
  group_by(block, Treatment) %>% 
  summarize((across(.cols=c(SLA, tiller_num),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T))) %>% 
  ungroup()



SLA_dif_tiller <- SLA_combined %>% 
  full_join(SLA_treat_max) %>% 
  select(block, Treatment, tiller_num_mean) %>% 
  pivot_wider(names_from = Treatment, values_from = tiller_num_mean) %>% 
  mutate(Difference= Combined - Single)

SLA_block_avg <- full_pop_demo_mar %>% 
  left_join(NExS_plot_key) %>% 
  unite(treatment, Drought, Grazing) %>% 
  left_join(leaf_calculations) %>% 
  group_by(block) %>% 
  summarize((across(.cols=c(SLA),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T))) %>% 
  ungroup

SLA_dif_avg <- SLA_block_avg %>% 
  full_join(SLA_dif_tiller)

SLA_block_dif_plot <- ggplot(data=SLA_dif_avg, aes(x=SLA_mean, y=Difference, color = block)) +
  geom_point() +
  theme_bw() +
  labs(title = 'Tiller Number by SLA', x = 'SLA (cm/g) per Individual', y = 'Tiller Number Difference') 
SLA_block_dif_plot

treat_num <- full_pop_demo_mar %>% 
  mutate(year = paste('x', year, sep = "_")) %>% 
  select(year, tiller_num, block, plot,plant_tag) %>% 
  pivot_wider(names_from = year, values_from = tiller_num) %>% 
  mutate(RR = log (x_2024/x_2023)) %>% 
  left_join(x23_traits_indiv) %>% 
  filter(!is.na(species)) %>% 
  mutate(Treatment = dplyr::recode(treatment,
                                   "D_G" = "Drought & Grazing",
                                   "D_NG" = "Drought",
                                   "ND_G" = "Grazing",
                                   "ND_NG" = "Control")) %>% 
  mutate(Species = dplyr::recode(species,
                                 "ari con" = "Aristida congesta",
                                 "the tri" = "Themeda triandra",
                                 "bot rad" = "Bothriochloa radicans",
                                 "pan col" = "Panicum coloratum", 
                                 "pan max" = "Megathyrsus maximus")) %>% 
  ungroup() %>% 
  filter(Treatment !="Control") %>% 
  select(block, Fire, plot, plant_tag, RR, Species, treat_num) %>% 
  group_by(Species, Treatment) %>% 
  summarize((across(.cols=c(RR),
                    .fns=list(mean=mean),
                    na.rm=T))) %>% 
    ungroup() %>% 
  pivot_wider(names_from = treat_num, values_from = RR_mean) %>% 
  mutate(Differences = abs(Combined-Single))

treat_num_plot <- treat_num  %>% 
  ggplot(aes(x=reorder(Species, -Differences), y=Differences, color = Species)) +
  geom_point(size = 20) +
  scale_color_brewer(palette = "Set2") +
  theme_bw() +
  theme(text = element_text(size = 45)) +
  theme(legend.text = element_text(size = 30)) +
  theme(legend.position = 'none') +
  labs(title = 'Differences in Response Ratios between \nSingle and Compounding events', x = 'Species', y = 'Differences in Response Ratios') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
treat_num_plot
#SSNM_diff 1200x1200


#### Shelby's PCA attempt ####
x23_traits_shortened <- x23_full_data_jan %>% 
  select(species, plant_tag, veg_height, tiller_num, thickness, SLA, LDMC, SRL, basal_area) %>% 
  drop_na() %>% 
  mutate(Species = dplyr::recode(species,
                                   "ari con" = "Aristida congesta",
                                   "the tri" = "Themeda triandra",
                                   "bot rad" = "Bothriochloa radicans",
                                   "pan col" = "Panicum coloratum", 
                                 "pan max" = "Megathyrsus maximus"))

x23_PCA_env <- x23_traits_shortened %>% 
  select(species, plant_tag)
  

trait.PCA <- prcomp (~ SLA + LDMC + SRL + tiller_num + veg_height + basal_area,
                    data=x23_traits_shortened,
                    scale. = TRUE)

trait.gg <-
  ggbiplot(trait.PCA, 
           obs.scale = 1, 
           var.scale = 1,
           groups = x23_traits_shortened$Species, 
           point.size=5, 
           alpha = 0.75,
           varname.adjust = 1.2,
           varname.size = 7, 
           varname.color = 'black',
           ellipse = TRUE,
           ellipse.fill = scale_color_brewer(palette = "Set2")) +
  labs(title = 'Plant Traits of the Dominant Species', fill = "Species", color = "Species") +
  theme_bw() +
  theme(legend.position = 'bottom') +
  guides(color = guide_legend(nrow = 3)) +
  theme(text = element_text(size = 30)) +
  theme(legend.text = element_text(size = 20)) +
  scale_color_brewer(palette = "Set2") +
  ylim(-3,3)
trait.gg

#Place the arrows in the forefront of the points
trait.gg$layers <- c(trait.gg$layers, trait.gg$layers[[2]])

#We can change the colour and width of the segments by doing
seg <- which(sapply(trait.gg$layers, function(x) class(x$geom)[1] == 'GeomSegment'))
trait.gg$layers[[seg[1]]]$aes_params$colour <- 'black' 
trait.gg$layers[[seg[2]]]$aes_params$colour <- 'black'

#Labels

#To change the labels to have a gray background, we need to overwrite the geom_text layer with a geom_label layer:
txt <- which(sapply(trait.gg$layers, function(x) class(x$geom)[1] == 'GeomText'))
trait.gg$layers[[txt]] <- geom_label(aes(x = xvar, y = yvar, label = varname,
                                  angle = angle, hjust = hjust, size = 100), 
                              label.size = NA,
                              data = trait.gg$layers[[txt]]$data, 
                              fill = '#dddddd80') 
trait.gg 
#SSNM_PCA #1200x800

brewer.pal(n=5,"Set2")

#RR vs PC1
summary(trait.pca)
trait.pca[["x"]]
trait.pca.df <- data.frame(trait.pca[["x"]]) %>% 
  select(PC1, PC2) %>%
  bind_cols(x23_PCA_env) %>% 
  right_join(rr_tiller_24_23) %>% 
  select(PC1, PC2, RR, Species, plant_tag, Treatment, SLA) %>% 
  drop_na()

PC1_RR_plot <- ggplot(data=trait.pca.df, aes(x=PC1, y=RR, color = Species)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgray", linewidth=1.5) +
  geom_point(size = 4, alpha=0.75) +
  geom_smooth(method=lm,
              se=FALSE) +
  scale_color_brewer(palette = "Set2") +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  theme(legend.text = element_text(size = 30)) +
  theme(legend.position = 'bottom') +
  guides(color = guide_legend(nrow = 2)) +
  facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  ylim(-2,2) +
  theme(legend.position = 'bottom', legend.direction="horizontal") +
  labs(title = 'Individual Response: Tiller Number Response Ratios across PC1', x = 'PC1', y = 'Response Ratio') 
PC1_RR_plot

ggarrange(SRL_indiv_plot, PC1_RR_plot, ncol = 1)
#SSNM_indiv 2600x1200

x23_traits_pca <- x23_traits_shortened %>% 
  select(!(species:plant_tag)) %>% 
  select(!(Species)) %>% 
  select(!(thickness))
S <- cor(x23_traits_pca)
S %>% round(2) 
S.eigen <- eigen(S) 
S.eigen$values %>% round(3)
sum(S.eigen$values) 
S.eigen.prop <- S.eigen$values / sum(S.eigen$values)
S.eigen.prop %>% round(3) # rounding for display 
S.eigen$vectors %>% round(2) # 1 vector per eigenvalue
loadings <- round(S.eigen$vectors[,1:3], digits = 3)
rownames(loadings) <- colnames(x23_traits_pca)
colnames(loadings) <- c("PC1", "PC2", "PC3")
loadings 
load.df <- data.frame(loadings)
write_csv(load.df, "loading.dataframe.csv")

trait.PCA <- princomp(x23_traits_pca, cor = TRUE)
summary(trait.PCA, loadings = TRUE, cutoff = 0)
plot(x23_traits_pca) 
trait.PCA.scores <- data.frame(trait.PCA$scores,
                              site = x23_traits_shortened$species)
adonis2(trait.PCA.scores$Comp.1 ~ site,
        data = trait.PCA.scores,
        method = "euc") 

ggplot(data = trait.PCA.scores, aes(x = site, y = Comp.1)) +
  geom_boxplot() +
  geom_jitter(aes(colour = site), width = 0.3, height = 0) +
  theme_bw() 

## Slopes code ##
rr_tiller_slopes <- full_pop_demo_mar %>% 
  mutate(year = paste('x', year, sep = "_")) %>% 
  select(year, tiller_num, block, plot,plant_tag) %>% 
  pivot_wider(names_from = year, values_from = tiller_num) %>% 
  mutate(RR = log (x_2024/x_2023)) %>% 
  left_join(x23_traits_indiv) %>% 
  filter(!is.na(species)) %>% 
  mutate(Treatment = dplyr::recode(treatment,
                                   "D_G" = "Drought & Grazing",
                                   "D_NG" = "Drought",
                                   "ND_G" = "Grazing",
                                   "ND_NG" = "Control")) %>% 
  filter(Treatment != "Control") %>% 
  mutate(Species = dplyr::recode(species,
                                 "ari con" = "Aristida congesta",
                                 "the tri" = "Themeda triandra",
                                 "bot rad" = "Bothriochloa radicans",
                                 "pan col" = "Panicum coloratum", 
                                 "pan max" = "Megathyrsus maximus")) %>%
  dplyr:: select(plant_tag, RR, SLA, Species, Treatment)

#  filter(Species == "Aristida congesta") %>% 
 #  %>% 
  #drop_na() 
  
slope_out = {}

#Aristida 
  aricon_drought_model <- lm(RR ~ SLA, data=filter(rr_tiller_slopes, Species =="Aristida congesta" & Treatment =="Drought"))
  summary_aricon_drought_model <- summary(aricon_drought_model)
  anova_model_aricon_drought_model <- anova(aricon_drought_model) 
  #right code for anova?
  
  out_model_aricon_drought <- data.frame(Treatment="Drought",
                                    metric="SLA",
                                    Species = "Aristida congesta",
                                    Intercept = summary_aricon_drought_model$coefficients[1],
                                    Slope = summary_aricon_drought_model$coefficients[2],
                                    Slope_se = summary_aricon_drought_model$coefficients[4],
                                    P_val=anova_model_aricon_drought_model$`Pr(>F)`[1],
                                    R2=summary_aricon_drought_model$r.squared,
                                    adj_R2=summary_aricon_drought_model$adj.r.squared)
  
aricon_graze_model <- lm(RR ~ SLA, data=filter(rr_tiller_slopes, Species =="Aristida congesta" & Treatment =="Grazing"))
  summary_aricon_graze_model <- summary(aricon_graze_model)
  anova_model_aricon_graze_model <- anova(aricon_graze_model) 
  anova_model_aricon_graze_model
  #right code for anova?
  
  out_model_aricon_graze <- data.frame(Treatment="Grazing",
                                    metric="SLA",
                                    Species = "Aristida congesta",
                                    Intercept = summary_aricon_graze_model$coefficients[1],
                                    Slope = summary_aricon_graze_model$coefficients[2],
                                    Slope_se = summary_aricon_graze_model$coefficients[4],
                                    P_val=anova_model_aricon_graze_model$`Pr(>F)`[1],
                                    R2=summary_aricon_graze_model$r.squared,
                                    adj_R2=summary_aricon_graze_model$adj.r.squared)
  
 
  #bot rad
  botrad_drought_model <- lm(RR ~ SLA, data=filter(rr_tiller_slopes, Species =="Bothriochloa radicans" & Treatment =="Drought"))
  summary_botrad_drought_model <- summary(botrad_drought_model)
  anova_model_botrad_drought_model <- anova(botrad_drought_model) 
  #right code for anova?
  
  out_model_botrad_drought <- data.frame(Treatment="Drought",
                                         metric="SLA",
                                         Species = "Bothriochloa radicans",
                                         Intercept = summary_botrad_drought_model$coefficients[1],
                                         Slope = summary_botrad_drought_model$coefficients[2],
                                         Slope_se = summary_botrad_drought_model$coefficients[4],
                                         P_val=anova_model_botrad_drought_model$`Pr(>F)`[1],
                                         R2=summary_botrad_drought_model$r.squared,
                                         adj_R2=summary_botrad_drought_model$adj.r.squared)
  
  botrad_graze_model <- lm(RR ~ SLA, data=filter(rr_tiller_slopes, Species =="Bothriochloa radicans" & Treatment =="Grazing"))
  summary_botrad_graze_model <- summary(botrad_graze_model)
  anova_model_botrad_graze_model <- anova(botrad_graze_model) 
  anova_model_botrad_graze_model
  #right code for anova?
  
  out_model_botrad_graze <- data.frame(Treatment="Grazing",
                                       metric="SLA",
                                       Species = "Bothriochloa radicans",
                                       Intercept = summary_botrad_graze_model$coefficients[1],
                                       Slope = summary_botrad_graze_model$coefficients[2],
                                       Slope_se = summary_botrad_graze_model$coefficients[4],
                                       P_val=anova_model_botrad_graze_model$`Pr(>F)`[1],
                                       R2=summary_botrad_graze_model$r.squared,
                                       adj_R2=summary_botrad_graze_model$adj.r.squared)
  
#Thethri
  thetri_drought_model <- lm(RR ~ SLA, data=filter(rr_tiller_slopes, Species =="Themeda triandra" & Treatment =="Drought"))
  summary_thetri_drought_model <- summary(thetri_drought_model)
  anova_model_thetri_drought_model <- anova(thetri_drought_model) 
  #right code for anova?
  
  out_model_thetri_drought <- data.frame(Treatment="Drought",
                                         metric="SLA",
                                         Species = "Themeda triandra",
                                         Intercept = summary_thetri_drought_model$coefficients[1],
                                         Slope = summary_thetri_drought_model$coefficients[2],
                                         Slope_se = summary_thetri_drought_model$coefficients[4],
                                         P_val=anova_model_thetri_drought_model$`Pr(>F)`[1],
                                         R2=summary_thetri_drought_model$r.squared,
                                         adj_R2=summary_thetri_drought_model$adj.r.squared)
  
  thetri_graze_model <- lm(RR ~ SLA, data=filter(rr_tiller_slopes, Species =="Themeda triandra" & Treatment =="Grazing"))
  summary_thetri_graze_model <- summary(thetri_graze_model)
  anova_model_thetri_graze_model <- anova(thetri_graze_model) 
  anova_model_thetri_graze_model
  #right code for anova?
  
  out_model_thetri_graze <- data.frame(Treatment="Grazing",
                                       metric="SLA",
                                       Species = "Themeda triandra",
                                       Intercept = summary_thetri_graze_model$coefficients[1],
                                       Slope = summary_thetri_graze_model$coefficients[2],
                                       Slope_se = summary_thetri_graze_model$coefficients[4],
                                       P_val=anova_model_thetri_graze_model$`Pr(>F)`[1],
                                       R2=summary_thetri_graze_model$r.squared,
                                       adj_R2=summary_thetri_graze_model$adj.r.squared)
  
#Pancol
  pancol_drought_model <- lm(RR ~ SLA, data=filter(rr_tiller_slopes, Species =="Panicum coloratum" & Treatment =="Drought"))
  summary_pancol_drought_model <- summary(pancol_drought_model)
  anova_model_pancol_drought_model <- anova(pancol_drought_model) 
  #right code for anova?
  
  out_model_pancol_drought <- data.frame(Treatment="Drought",
                                         metric="SLA",
                                         Species = "Panicum coloratum",
                                         Intercept = summary_pancol_drought_model$coefficients[1],
                                         Slope = summary_pancol_drought_model$coefficients[2],
                                         Slope_se = summary_pancol_drought_model$coefficients[4],
                                         P_val=anova_model_pancol_drought_model$`Pr(>F)`[1],
                                         R2=summary_pancol_drought_model$r.squared,
                                         adj_R2=summary_pancol_drought_model$adj.r.squared)
  
  pancol_graze_model <- lm(RR ~ SLA, data=filter(rr_tiller_slopes, Species =="Panicum coloratum" & Treatment =="Grazing"))
  summary_pancol_graze_model <- summary(pancol_graze_model)
  anova_model_pancol_graze_model <- anova(pancol_graze_model) 
  anova_model_pancol_graze_model
  #right code for anova?
  
  out_model_pancol_graze <- data.frame(Treatment="Grazing",
                                       metric="SLA",
                                       Species = "Panicum coloratum",
                                       Intercept = summary_pancol_graze_model$coefficients[1],
                                       Slope = summary_pancol_graze_model$coefficients[2],
                                       Slope_se = summary_pancol_graze_model$coefficients[4],
                                       P_val=anova_model_pancol_graze_model$`Pr(>F)`[1],
                                       R2=summary_pancol_graze_model$r.squared,
                                       adj_R2=summary_pancol_graze_model$adj.r.squared)

  
#megamax
  panmax_drought_model <- lm(RR ~ SLA, data=filter(rr_tiller_slopes, Species =="Megathyrsus maximus" & Treatment =="Drought"))
  summary_panmax_drought_model <- summary(panmax_drought_model)
  anova_model_panmax_drought_model <- anova(panmax_drought_model) 
  #right code for anova?
  
  out_model_panmax_drought <- data.frame(Treatment="Drought",
                                         metric="SLA",
                                         Species = "Megathyrsus maximus",
                                         Intercept = summary_panmax_drought_model$coefficients[1],
                                         Slope = summary_panmax_drought_model$coefficients[2],
                                         Slope_se = summary_panmax_drought_model$coefficients[4],
                                         P_val=anova_model_panmax_drought_model$`Pr(>F)`[1],
                                         R2=summary_panmax_drought_model$r.squared,
                                         adj_R2=summary_panmax_drought_model$adj.r.squared)
  
  panmax_graze_model <- lm(RR ~ SLA, data=filter(rr_tiller_slopes, Species =="Megathyrsus maximus" & Treatment =="Grazing"))
  summary_panmax_graze_model <- summary(panmax_graze_model)
  anova_model_panmax_graze_model <- anova(panmax_graze_model) 
  anova_model_panmax_graze_model
  #right code for anova?
  
  out_model_panmax_graze <- data.frame(Treatment="Grazing",
                                       metric="SLA",
                                       Species = "Megathyrsus maximus",
                                       Intercept = summary_panmax_graze_model$coefficients[1],
                                       Slope = summary_panmax_graze_model$coefficients[2],
                                       Slope_se = summary_panmax_graze_model$coefficients[4],
                                       P_val=anova_model_panmax_graze_model$`Pr(>F)`[1],
                                       R2=summary_panmax_graze_model$r.squared,
                                       adj_R2=summary_panmax_graze_model$adj.r.squared)
  
#combine
slope_out <- rbind(slope_out, out_model_aricon_drought, out_model_aricon_graze, out_model_botrad_drought, out_model_botrad_graze, out_model_thetri_drought, out_model_thetri_graze, out_model_pancol_drought, out_model_pancol_graze, out_model_panmax_drought, out_model_panmax_graze)  

slopes_df <- slope_out %>% 
  select(Species, Slope, Treatment) %>% 
  pivot_wider(names_from = Treatment, values_from = Slope)

slopes_plot <- ggplot(data=slopes_df, aes(x=Grazing, y=Drought, color=Species)) + geom_rect(aes(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0),
            alpha=0.15,
            fill = "tan") +
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0),
            alpha=0.15,
            fill = "chocolate") +
  geom_point(aes(), size = 10) +
  geom_rect(aes(xmin = -Inf, xmax = 0, ymin = 0, ymax = Inf),
            alpha=0.15,
            fill = "chocolate") +
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = 0, ymax = Inf),
            alpha=0.15,
            fill = "tan") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 1.5) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 1.5) +
  geom_point(aes(), size = 12) +
  scale_color_brewer(palette = "Set2") +
  theme_bw() +
  theme(aspect.ratio=1) +
  labs(title = 'Response Ratio Slopes', x = 'SLA-Grazing Response Slope', y = 'SLA-Drought Response Slope', fill = "Species", color = "Species") +
  theme(legend.position = 'bottom') +
  guides(color = guide_legend(nrow = 3)) +
  theme(text = element_text(size = 40)) +
  theme(legend.text = element_text(size = 30))
slopes_plot
#SSNM_slopes 1600x1600
  
#### ESA code ####

#Tiller RR by block and treatment
rr_tiller_24_23 <- full_pop_demo_mar %>% 
  mutate(year = paste('x', year, sep = "_")) %>% 
  select(year, tiller_num, block, plot,plant_tag) %>% 
  pivot_wider(names_from = year, values_from = tiller_num) %>% 
  mutate(RR = log (x_2024/x_2023)) %>% 
  left_join(x23_traits_indiv) %>% 
  filter(!is.na(species)) %>% 
  mutate(Treatment = dplyr::recode(treatment,
                                   "D_G" = "Drought & Grazing",
                                   "D_NG" = "Drought",
                                   "ND_G" = "Grazing",
                                   "ND_NG" = "Control")) %>% 
  mutate(Species = dplyr::recode(species,
                                 "ari con" = "Aristida congesta",
                                 "the tri" = "Themeda triandra",
                                 "bot rad" = "Bothriochloa radicans",
                                 "pan col" = "Panicum coloratum", 
                                 "pan max" = "Megathyrsus maximus")) %>% 
  group_by(block, Treatment) %>% 
  summarize((across(.cols=c(RR),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T))) %>% 
  ungroup() 

#mean traits by block
mean_pop_demo_24 <- x24_pop_demo  %>% 
  mutate(aerial_area = 3.14 * aerial1 * aerial2) %>% 
  select(!(aerial1:aerial2)) %>% 
  mutate(basal_area = 3.14 * basal1 * basal2) %>% 
  select(!(basal1:basal2)) %>% 
  mutate(Block = block) %>% 
  select(!(block:plant_tag)) %>% 
  group_by(Block) %>% 
  summarize((across(.cols=c(veg_height:basal_area),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T))) %>% 
  ungroup() %>% 
  mutate(block = Block)

mean_traits_full <- mean_leaf_calculations %>% 
  left_join(roots) %>% 
  group_by(block) %>% 
  summarize((across(.cols=c(total_area:SRL),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T))) %>% 
  ungroup() %>% 
  left_join(mean_pop_demo_24)

rr_traits <- rr_tiller_24_23 %>% 
  left_join(mean_traits_full)  

### Graphs
ggplot(data=rr_traits, aes(x=SLA_mean, y=RR_mean, group= Treatment, color = block)) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=RR_mean - RR_sefxn, ymax=RR_mean + RR_sefxn)) +
  geom_errorbarh(aes(xmin=SLA_mean - SLA_sefxn, xmax=SLA_mean + SLA_sefxn)) +
  facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by SLA', x = 'SLA (cm/g)', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=rr_traits, aes(x=SLA_mean, y=RR_mean, group= Treatment, color = Treatment)) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=RR_mean - RR_sefxn, ymax=RR_mean + RR_sefxn)) +
  geom_errorbarh(aes(xmin=SLA_mean - SLA_sefxn, xmax=SLA_mean + SLA_sefxn)) +
  #facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by SLA', x = 'SLA (cm/g)', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=rr_traits, aes(x=SLA_mean, y=SRL_mean, group= Treatment, color = block)) +
  geom_point() +
  #geom_smooth(method=lm, se=FALSE) +
  geom_errorbar(aes(ymin=SRL_mean - SRL_sefxn, ymax=SRL_mean + SRL_sefxn)) +
  geom_errorbarh(aes(xmin=SLA_mean - SLA_sefxn, xmax=SLA_mean + SLA_sefxn)) +
  #facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'SRL by SLA', x = 'SLA (cm/g)', y = 'SRL') +
  theme_bw() +
  theme(legend.position = "bottom")


ggplot(data=rr_traits, aes(x=SRL_mean, y=RR_mean, group= Treatment, color = block)) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=RR_mean - RR_sefxn, ymax=RR_mean + RR_sefxn)) +
  geom_errorbarh(aes(xmin=SRL_mean - SRL_sefxn, xmax=SRL_mean + SRL_sefxn)) +
  facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by SRL', x = 'SRL', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")


ggplot(data=rr_traits, aes(x=SRL_mean, y=RR_mean, group= Treatment, color = Treatment)) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=RR_mean - RR_sefxn, ymax=RR_mean + RR_sefxn)) +
  geom_errorbarh(aes(xmin=SRL_mean - SRL_sefxn, xmax=SRL_mean + SRL_sefxn)) +
  #facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by SRL', x = 'SRL', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=rr_traits, aes(x=LDMC_mean, y=SRL_mean, group= Treatment, color = block)) +
  geom_point() +
  #geom_smooth(method=lm, se=FALSE) +
  geom_errorbar(aes(ymin=SRL_mean - SRL_sefxn, ymax=SRL_mean + SRL_sefxn)) +
  geom_errorbarh(aes(xmin=LDMC_mean - LDMC_sefxn, xmax=LDMC_mean + LDMC_sefxn)) +
  #facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'SRL by LDMC', x = 'LDMC', y = 'SRL') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=rr_traits, aes(x=LDMC_mean, y=RR_mean, group= Treatment, color = block)) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=RR_mean - RR_sefxn, ymax=RR_mean + RR_sefxn)) +
  geom_errorbarh(aes(xmin=LDMC_mean - LDMC_sefxn, xmax=LDMC_mean + LDMC_sefxn)) +
  facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by LDMC', x = 'LDMC', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=rr_traits, aes(x=LDMC_mean, y=RR_mean, group= Treatment, color = Treatment)) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=RR_mean - RR_sefxn, ymax=RR_mean + RR_sefxn)) +
  geom_errorbarh(aes(xmin=LDMC_mean - LDMC_sefxn, xmax=LDMC_mean + LDMC_sefxn)) +
  #facet_wrap(~block) +
  #facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by LDMC', x = 'LDMC', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=rr_traits, aes(x=thickness_mean, y=RR_mean, group= Treatment, color = block)) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=RR_mean - RR_sefxn, ymax=RR_mean + RR_sefxn)) +
  geom_errorbarh(aes(xmin=thickness_mean - thickness_sefxn, xmax=thickness_mean + thickness_sefxn)) +
  facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by thickness', x = 'thickness', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=rr_traits, aes(x=thickness_mean, y=RR_mean, group= Treatment, color = Treatment)) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=RR_mean - RR_sefxn, ymax=RR_mean + RR_sefxn)) +
  geom_errorbarh(aes(xmin=thickness_mean - thickness_sefxn, xmax=thickness_mean + thickness_sefxn)) +
  #facet_wrap(~block) +
  #facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by thickness', x = 'thickness', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=rr_traits, aes(x=veg_height_mean, y=RR_mean, group= Treatment, color = block)) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=RR_mean - RR_sefxn, ymax=RR_mean + RR_sefxn)) +
  geom_errorbarh(aes(xmin=veg_height_mean - veg_height_sefxn, xmax=veg_height_mean + veg_height_sefxn)) +
  facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by veg_height', x = 'veg_height', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=rr_traits, aes(x=veg_height_mean, y=RR_mean, group= Treatment, color = Treatment)) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=RR_mean - RR_sefxn, ymax=RR_mean + RR_sefxn)) +
  geom_errorbarh(aes(xmin=veg_height_mean - veg_height_sefxn, xmax=veg_height_mean + veg_height_sefxn)) +
  #facet_wrap(~block) +
  #facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by veg_height', x = 'veg_height', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=rr_traits, aes(x=vol_density_mean, y=RR_mean, group= Treatment, color = block)) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=RR_mean - RR_sefxn, ymax=RR_mean + RR_sefxn)) +
  geom_errorbarh(aes(xmin=vol_density_mean - vol_density_sefxn, xmax=vol_density_mean + vol_density_sefxn)) +
  facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by vol_density', x = 'vol_density', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=rr_traits, aes(x=vol_density_mean, y=RR_mean, group= Treatment, color = Treatment)) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=RR_mean - RR_sefxn, ymax=RR_mean + RR_sefxn)) +
  geom_errorbarh(aes(xmin=vol_density_mean - vol_density_sefxn, xmax=vol_density_mean + vol_density_sefxn)) +
  #facet_wrap(~block) +
  #facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by vol_density', x = 'vol_density', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=rr_traits, aes(x=perc_green_mean, y=RR_mean, group= Treatment, color = block)) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=RR_mean - RR_sefxn, ymax=RR_mean + RR_sefxn)) +
  geom_errorbarh(aes(xmin=perc_green_mean - perc_green_sefxn, xmax=perc_green_mean + perc_green_sefxn)) +
  facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by perc_green', x = 'perc_green', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=rr_traits, aes(x=perc_green_mean, y=RR_mean, group= Treatment, color = Treatment)) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=RR_mean - RR_sefxn, ymax=RR_mean + RR_sefxn)) +
  geom_errorbarh(aes(xmin=perc_green_mean - perc_green_sefxn, xmax=perc_green_mean + perc_green_sefxn)) +
  #facet_wrap(~block) +
  #facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by perc_green', x = 'perc_green', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=rr_traits, aes(x=basal_area_mean, y=RR_mean, group= Treatment, color = block)) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=RR_mean - RR_sefxn, ymax=RR_mean + RR_sefxn)) +
  geom_errorbarh(aes(xmin=basal_area_mean - basal_area_sefxn, xmax=basal_area_mean + basal_area_sefxn)) +
  facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by basal_area', x = 'basal_area', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=rr_traits, aes(x=basal_area_mean, y=RR_mean, group= Treatment, color = Treatment)) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=RR_mean - RR_sefxn, ymax=RR_mean + RR_sefxn)) +
  geom_errorbarh(aes(xmin=basal_area_mean - basal_area_sefxn, xmax=basal_area_mean + basal_area_sefxn)) +
  #facet_wrap(~block) +
  #facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by basal_area', x = 'basal_area', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=rr_traits, aes(x=aerial_area_mean, y=RR_mean, group= Treatment, color = block)) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=RR_mean - RR_sefxn, ymax=RR_mean + RR_sefxn)) +
  geom_errorbarh(aes(xmin=aerial_area_mean - aerial_area_sefxn, xmax=aerial_area_mean + aerial_area_sefxn)) +
  facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by aerial_area', x = 'aerial_area', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=rr_traits, aes(x=aerial_area_mean, y=RR_mean, group= Treatment, color = Treatment)) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=RR_mean - RR_sefxn, ymax=RR_mean + RR_sefxn)) +
  geom_errorbarh(aes(xmin=aerial_area_mean - aerial_area_sefxn, xmax=aerial_area_mean + aerial_area_sefxn)) +
  #facet_wrap(~block) +
  #facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by aerial_area', x = 'aerial_area', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

#Differences
rr_tiller_max <- rr_tiller_24_23 %>% 
  group_by(block) %>% 
  summarise(across(RR_mean, max)) %>% 
  mutate(Treatment = "Single")

rr_traits_block <- rr_traits %>% 
  filter(Treatment == "Drought") %>% 
  select(!(Treatment:RR_sefxn))

rr_tiller_combined <- full_pop_demo_mar %>% 
  mutate(year = paste('x', year, sep = "_")) %>% 
  select(year, tiller_num, block, plot,plant_tag) %>% 
  pivot_wider(names_from = year, values_from = tiller_num) %>% 
  mutate(RR = log (x_2024/x_2023)) %>% 
  left_join(x23_traits_indiv) %>% 
  filter(!is.na(species)) %>% 
  mutate(Treatment = dplyr::recode(treatment,
                                   "D_G" = "Combined",
                                   "D_NG" = "Drought",
                                   "ND_G" = "Grazing",
                                   "ND_NG" = "Control")) %>% 
  mutate(Species = dplyr::recode(species,
                                 "ari con" = "Aristida congesta",
                                 "the tri" = "Themeda triandra",
                                 "bot rad" = "Bothriochloa radicans",
                                 "pan col" = "Panicum coloratum", 
                                 "pan max" = "Megathyrsus maximus")) %>% 
  group_by(block, Treatment) %>% 
  summarize((across(.cols=c(RR, SLA),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T))) %>% 
  ungroup() %>% 
  filter((Treatment == "Combined")) %>% 
  full_join(rr_tiller_max) %>% 
  select(block, Treatment, RR_mean) %>% 
  pivot_wider(names_from = Treatment, values_from = RR_mean) %>% 
  mutate(Difference= Combined - Single) %>% 
  full_join(rr_traits_block)
  
  
  

ggplot(data=rr_tiller_combined, aes(x=SLA_mean, y=Difference, color = block)) +
  geom_point() +
  geom_errorbarh(aes(xmin=SLA_mean - SLA_sefxn, xmax=SLA_mean + SLA_sefxn)) +
  theme_bw() +
  labs(title = 'Response Ratio by SLA', x = 'SLA (cm/g)', y = 'RR Difference')

ggplot(data=rr_tiller_combined, aes(x=SRL_mean, y=Difference, color = block)) +
  geom_point() +
  theme_bw() +
  geom_errorbarh(aes(xmin=SRL_mean - SRL_sefxn, xmax=SRL_mean + SRL_sefxn)) +
  labs(title = 'Response Ratio by SRL', x = 'SRL', y = 'RR Difference')

ggplot(data=rr_tiller_combined, aes(x=LDMC_mean, y=Difference, color = block)) +
  geom_point() +
  geom_errorbarh(aes(xmin=LDMC_mean - LDMC_sefxn, xmax=LDMC_mean + LDMC_sefxn)) +
  theme_bw() +
  labs(title = 'Response Ratio by LDMC', x = 'LDMC', y = 'RR Difference')

ggplot(data=rr_tiller_combined, aes(x=thickness_mean, y=Difference, color = block)) +
  geom_point() +
  geom_errorbarh(aes(xmin=thickness_mean - thickness_sefxn, xmax=thickness_mean + thickness_sefxn)) +
  theme_bw() +
  labs(title = 'Response Ratio by thickness', x = 'thickness', y = 'RR Difference')

ggplot(data=rr_tiller_combined, aes(x=veg_height_mean, y=Difference, color = block)) +
  geom_point() +
  geom_errorbarh(aes(xmin=veg_height_mean - veg_height_sefxn, xmax=veg_height_mean + veg_height_sefxn)) +
  theme_bw() +
  labs(title = 'Response Ratio by veg_height', x = 'veg_height', y = 'RR Difference')

ggplot(data=rr_tiller_combined, aes(x=perc_green_mean, y=Difference, color = block)) +
  geom_point() +
  geom_errorbarh(aes(xmin=perc_green_mean - perc_green_sefxn, xmax=perc_green_mean + perc_green_sefxn)) +
  theme_bw() +
  labs(title = 'Response Ratio by perc_green', x = 'perc_green', y = 'RR Difference')

ggplot(data=rr_tiller_combined, aes(x=basal_area_mean, y=Difference, color = block)) +
  geom_point() +
  geom_errorbarh(aes(xmin=basal_area_mean - basal_area_sefxn, xmax=basal_area_mean + basal_area_sefxn)) +
  theme_bw() +
  labs(title = 'Response Ratio by basal_area', x = 'basal_area', y = 'RR Difference')

### tiller num indiv
tiller_RR_indiv <- full_pop_demo_mar %>% 
  select(block, plot, plant_tag,  year, tiller_num) %>% 
  group_by(block, plant_tag) %>% 
  mutate(year = paste('x', year, sep = "_")) %>% 
  pivot_wider(names_from = year, values_from= tiller_num) %>% 
  mutate(RR = log (x_2024/x_2023)) %>% 
  ungroup() %>% 
  select(!(x_2024:x_2023)) %>% 
  full_join(mean_leaf_calculations) %>% 
  full_join(roots) %>% 
  drop_na() %>% 
  full_join(NExS_plot_key) %>% 
  unite(treatment, Drought, Grazing) %>% 
  mutate(Treatment = dplyr::recode(treatment,
                                   "D_G" = "Drought & Grazing",
                                   "D_NG" = "Drought",
                                   "ND_G" = "Grazing",
                                   "ND_NG" = "Control")) %>% 
  filter(Treatment != 'Control')
  
hist(sqrt(tiller_RR_indiv$RR + .02))
shapiro.test(sqrt(tiller_RR_indiv$RR + .02))

SLA_RR_mod <- lme(sqrt(RR + .02) ~ SLA*Treatment
                         , data=tiller_RR_indiv
                         , random = ~1 |Block/Exclosure/Plot
                         , correlation=corCompSymm(form = ~1 |Block/Exclosure/Plot)
                         , control=lmeControl(returnObject=TRUE)
                         , na.action = na.omit)
anova(tree_indiv_mod_sc, type="marginal")
summary(tree_indiv_mod_sc)

plot(SLA_RR_mod, type=c("p","smooth"), col.line=1)
qqnorm(tree_indiv_mod_sc , abline = c(0,1)) ## qqplot

emmeans(tree_indiv_mod_sc, "size_class", by="Year")
pairs(emmeans(tree_indiv_mod, ~Herbivory  | Year* size_class, adjust='sidak'))


####Kevin's RR code
### Read in and clean data
## KW Note March 29, 2024 - We are missing the pop demo data for plots 1, 2, and 3
plot_key <- read.csv("NExS_plot_key.csv") %>%
  rename(block=Block, 
         plot=Plot, 
         drought=Drought, 
         grazing=Grazing, 
         surface_sensor=Surface.Sensor,
         sensor_install_date=Install.Date)

popdemo_2023 <- read_excel("NExS_PopDemo_March2023_clean.xlsx") %>%
  full_join(dplyr::select(plot_key, block, plot, drought, grazing, Fire),
            by=c("block", "plot")) %>%
  mutate(basalarea = pi*(basal1/2)*(basal2/2)) %>% 
  mutate(aerialarea = pi*(aerial1/2)*(aerial2/2)) %>% 
  mutate(conevolume = (1/3)*pi* veg_height*(((basal1+basal2)/2)^2)+((basal1+basal2)/2)* (((aerial1+aerial2)/2)^2)+(((aerial1+aerial2)/2))) %>%
  dplyr::select(-comments) %>%
  mutate(dead_flag=NA)

popdemo_2024 <- read_excel("NExS_PopDemo_March2024_clean.xlsx") %>%
  full_join(dplyr::select(plot_key, block, plot, drought, grazing, Fire),
            by=c("block", "plot")) %>%
  mutate(basalarea = pi*(basal1/2)*(basal2/2)) %>% 
  mutate(aerialarea = pi*(aerial1/2)*(aerial2/2)) %>% 
  mutate(conevolume = (1/3)*pi* veg_height*(((basal1+basal2)/2)^2)+((basal1+basal2)/2)* (((aerial1+aerial2)/2)^2)+(((aerial1+aerial2)/2))) %>%
  dplyr::select(-perc_green, -comments) %>%
  rename(flower_length=flower_len) %>%
  mutate(dead_flag=NA)

popdemo_2025 <- read_excel("NExS_PopDemo_March2025_raw.xlsx") %>%
  full_join(dplyr::select(plot_key, block, plot, drought, grazing, Fire),
            by=c("block", "plot")) %>%
  mutate(basalarea = pi*(basal1/2)*(basal2/2)) %>% 
  mutate(aerialarea = pi*(aerial1/2)*(aerial2/2)) %>% 
  mutate(conevolume = (1/3)*pi* veg_height*(((basal1+basal2)/2)^2)+((basal1+basal2)/2)* (((aerial1+aerial2)/2)^2)+(((aerial1+aerial2)/2))) %>%
  dplyr::select(-comments)


## Check that data is complete and for outliers
with(popdemo_2024, table(block)) ## Looks good!
popdemo_2024_long <- popdemo_2024 %>% 
  pivot_longer(cols=c(veg_height:vol_density,basalarea:conevolume), names_to="metric_name", values_to="metric_value")
with(popdemo_2023, table(block)) ## Looks good!
popdemo_2023_long <- popdemo_2023 %>% 
  pivot_longer(cols=c(veg_height:vol_density,basalarea:conevolume), names_to="metric_name", values_to="metric_value")
popdemo_2025_long <- popdemo_2025 %>% 
  pivot_longer(cols=c(veg_height:vol_density,basalarea:conevolume), names_to="metric_name", values_to="metric_value")

### Boxplots to look for outliers
ggplot(popdemo_2023_long, aes(x=species, y=metric_value)) +
  geom_boxplot() +
  facet_wrap(~metric_name, scales="free")
ggplot(popdemo_2024_long, aes(x=species, y=metric_value)) +
  geom_boxplot() +
  facet_wrap(~metric_name, scales="free")
ggplot(popdemo_2025_long, aes(x=species, y=metric_value)) +
  geom_boxplot() +
  facet_wrap(~metric_name, scales="free")

### Removed one outlier that had 120 cm basal 2 entry, basal 1 measurement for this individual was 4.5 so this was clearly an error -- we should look back at the raw datasheet to assess.
popdemo_2023 <- popdemo_2023 %>%
  mutate(basal2=replace(basal2, basal2>119, NA))
popdemo_2024 <- popdemo_2024 %>%
  mutate(basal2=replace(tiller_num, tiller_num>300, NA))
# There may be a couple other outliers in there, but I think we keep everything in there for now (KW April 2025)

### Merge 2023 and 2024 by tag number to double check that they line up properly
test <- popdemo_2023 %>%
  full_join(popdemo_2025, by=c("block", "plot", "plant_tag"))
### Found an issue with 2023 tag 38 missing -- there are two tag 37s, one of which is likely actually 38
### For now, just changing the second one manually and then remerging... (line 28 change tag to 38)
popdemo_2023 <- edit(popdemo_2023)

### Combine years
popdemo_all <- popdemo_2023 %>%
  bind_rows(popdemo_2024) %>%
  bind_rows(popdemo_2025) %>%
  rename(fire=Fire) %>%
  mutate(tiller_num=replace(tiller_num, dead_flag==1,0))

### Calculate change in metrics from 2023 to plot
popdemo_pretrt <- popdemo_all %>%
  filter(year==2023) %>%
  rename(veg_height_pre=veg_height,
         flower_length_pre=flower_length,
         tiller_num_pre=tiller_num,
         flower_num_pre=flower_num,
         basalarea_pre=basalarea,
         aerialarea_pre=aerialarea,
         conevolume_pre=conevolume) %>%
  dplyr::select(-basal1,-basal2,-aerial1,-aerial2,-vol_density,-dead_flag, -year,-month)

popdemo_pre_post <- popdemo_all %>%
  filter(year %in% 2024:2025) %>%
  dplyr::select(-basal1,-basal2,-aerial1,-aerial2,-vol_density, -month) %>%
  full_join(popdemo_pretrt, by=c("block","plot","plant_tag","species","drought","grazing","fire")) %>%
  mutate(veg_height_diff = veg_height - veg_height_pre,
         flower_length_diff = flower_length - flower_length_pre,
         tiller_num_diff = tiller_num - tiller_num_pre,
         flower_num_diff = flower_num - flower_num_pre,
         basalarea_diff = basalarea - basalarea_pre,
         aerialarea_diff = aerialarea - aerialarea_pre,
         conevolume_diff = conevolume - conevolume_pre
  ) %>%
  mutate(veg_height_pchange = (veg_height - veg_height_pre)/veg_height_pre,
         flower_length_pchange = (flower_length - flower_length_pre)/flower_length_pre,
         tiller_num_pchange = (tiller_num - tiller_num_pre)/tiller_num_pre,
         flower_num_pchange = (flower_num - flower_num_pre)/flower_num_pre,
         basalarea_pchange = (basalarea - basalarea_pre)/basalarea_pre,
         aerialarea_pchange = (aerialarea - aerialarea_pre)/aerialarea_pre,
         conevolume_pchange = (conevolume - conevolume_pre)/conevolume_pre
  ) %>%
  mutate(veg_height_lnrr = log(veg_height / veg_height_pre),
         flower_length_lnrr = log(flower_length / flower_length_pre),
         tiller_num_lnrr = log(tiller_num / tiller_num_pre),
         flower_num_lnrr = log(flower_num / flower_num_pre),
         basalarea_lnrr = log(basalarea / basalarea_pre),
         aerialarea_lnrr = log(aerialarea / aerialarea_pre),
         conevolume_lnrr = log(conevolume / conevolume_pre)
  )

### Calculate means and se of lnRR by year
popdemo_lnrr_means <- popdemo_pre_post %>%
  mutate(dead_flag=replace(dead_flag, is.na(dead_flag), 0)) %>%
  filter(dead_flag != 1) %>%
  group_by(year, species, drought, grazing, fire) %>%
  summarize_at(.vars=vars(veg_height_lnrr, flower_length_lnrr, tiller_num_lnrr, flower_num_lnrr, basalarea_lnrr, aerialarea_lnrr, conevolume_lnrr),
               .funs=list(mean=mean, se=sefxn),na.rm=T) %>%
  mutate(trt_all = paste(drought, grazing, fire))

popdemo_lnrr_means$trt_all <- factor(popdemo_lnrr_means$trt_all, levels=c("ND NG AF",
                                                                          "ND G AF","D NG AF","ND NG EF",
                                                                          "D G AF","D NG EF","ND G EF",
                                                                          "D G EF"))
unique(popdemo_lnrr_means$trt_all)

ggplot(popdemo_lnrr_means, aes(x=trt_all, y=tiller_num_lnrr_mean, ymin=tiller_num_lnrr_mean-tiller_num_lnrr_se, ymax=tiller_num_lnrr_mean+tiller_num_lnrr_se, fill=trt_all)) +
  geom_errorbar(width=0) +
  geom_col() +
  theme_bw() +
  facet_grid(block~year) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

popdemo_tiller_RR <- popdemo_lnrr_means %>% 
  #filter(year == 2025) %>% 
  select(year:fire, tiller_num_lnrr_mean, tiller_num_lnrr_se) %>% 
  mutate(de = ifelse(drought == 'D', 1, 0)) %>% 
  mutate(ge = ifelse(grazing == 'G', 1, 0)) %>% 
  mutate(fe = ifelse(fire == 'EF', 1, 0)) %>% 
  unite(trt_all, drought, grazing, fire, sep = " ") %>% 
  mutate(extremes = de + ge + fe) %>% 
  rename(tiller_num_rr = tiller_num_lnrr_mean) %>% 
  group_by(year, species, extremes) %>% 
  summarize((across(.cols=c(tiller_num_rr),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T)))

ggplot(data=popdemo_tiller_RR, aes(x=extremes, y=tiller_num_rr_mean, group= species, color= species)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgray", linewidth=1.5) +
  geom_point() +
  geom_errorbar(aes(ymin=tiller_num_rr_mean - tiller_num_rr_sefxn, ymax=tiller_num_rr_mean + tiller_num_rr_sefxn)) +
  geom_smooth(method=lm,
              se=FALSE) 
#facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by basal_area', x = 'basal_area', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")



### Calculate mortality probability

mortality_df <- popdemo_all %>%
  mutate(dead_flag=replace(dead_flag, is.na(dead_flag), 0)) %>%
  group_by(year, block, plot, species, drought, grazing, fire) %>%
  summarize(pMortality = sum(dead_flag)/length(dead_flag)) %>%
  mutate(trt_all = paste(drought, grazing, fire)) %>%
  ungroup()


mortality_df$trt_all <- factor(mortality_df$trt_all, levels=c("ND NG AF",
                                                              "ND G AF","D NG AF","ND NG EF",
                                                              "D G AF","D NG EF","ND G EF",
                                                              "D G EF"))
mortality_means <- mortality_df %>%
  unite(treatment, drought,grazing) %>% 
  filter(fire == 'AF') %>% 
  filter(year == 2025) %>% 
  mutate(Treatment = dplyr::recode(treatment,
                                   "D_G" = "Drought & Grazing",
                                   "D_NG" = "Drought",
                                   "ND_G" = "Grazing",
                                   "ND_NG" = "Control"
  )) %>% 
  group_by(year, Treatment) %>%
  summarize_at(.vars=vars(pMortality), .funs=list(mean=mean, se=sefxn),na.rm=T) %>%
  ungroup() %>% 
  mutate(extremes = dplyr::recode(Treatment,
                                   "Drought & Grazing" = 2,
                                   "Drought" = 1,
                                   "Grazing" = 1,
                                   "Control" = 0
  ))


# plotting overall mortality means
ggplot(mortality_means, x=Treatment, y=mean, col=Treatment) +
  geom_bar(stat="identity") +
  geom_errorbar(width=.1, col="black") +
  scale_x_continuous(limits=c('Control', 'Drought', 'Grazing', 'Drought & Grazing')) %>% 
  theme_bw()

ggplot(subset(mortality_means,year==2025), aes(x=trt_all, y=mean, ymin=mean-se, ymax=mean+se, fill=trt_all)) +
  geom_errorbar(width=0) +
  geom_col() +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# plotting mortality by species and treatment
ggplot(subset(mortality_df,year==2025), aes(x=trt_all, y=pMortality, fill=species)) +
  geom_col() +
  theme_bw() +
  facet_wrap(~block) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

#### Linking traits w/ mortality
mean_traits_species <- mean_leaf_calculations %>% 
  left_join(roots) %>% 
  left_join(NExS_plot_key) %>% 
  unite(trt_all, Drought, Grazing, Fire, sep = " ") %>% 
  mutate(trt_all = as.factor(trt_all)) %>% 
  group_by(block, plot, DomGrass, trt_all) %>% 
  summarize((across(.cols=c(total_area:SRL),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T))) %>% 
  ungroup() %>% 
  rename(species = DomGrass)

mortality_species <- mortality_df %>% 
  filter(year == 2025) %>% 
  select(!(drought:fire)) %>% 
  full_join(mean_traits_species)

ggplot(data=mortality_species, aes(x=SLA_mean, y=pMortality, group= block, color = block)) +
  geom_point() +
  geom_errorbarh(aes(xmin=SLA_mean - SLA_sefxn, xmax=SLA_mean + SLA_sefxn)) +
  facet_wrap(~trt_all) 
#facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by basal_area', x = 'basal_area', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

mortality_extremes <- popdemo_all %>%
  mutate(dead_flag=replace(dead_flag, is.na(dead_flag), 0)) %>%
  group_by(year, block, plot, species, drought, grazing, fire) %>%
  summarize(pMortality = sum(dead_flag)/length(dead_flag)) %>%
  mutate(de = ifelse(drought == 'D', 1, 0)) %>% 
  mutate(ge = ifelse(grazing == 'G', 1, 0)) %>% 
  mutate(fe = ifelse(fire == 'EF', 1, 0)) %>% 
  unite(trt_all, drought, grazing, fire, sep = " ") %>% 
  mutate(extremes = de + ge + fe) %>% 
  ungroup() %>% 
  select(!(de:fe)) %>% 
  right_join(mean_traits_species) %>% #original last step
  select(!(ends_with("sefxn"))) %>% 
  group_by(year, block, extremes) %>% 
  summarize((across(.cols=c(pMortality, total_area_mean:SRL_mean),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T))) %>% 
  ungroup() %>% 
  right_join(popdemo_tiller_RR) %>% 
  filter(year == 2025)



ggplot(data=mortality_extremes, aes(y=pMortality_mean, x=LDMC_mean_mean, group= as.factor(block), color = as.factor(block))) +
  geom_point() +
  geom_errorbarh(aes(xmin=LDMC_mean_mean -LDMC_mean_sefxn, xmax=LDMC_mean_mean + LDMC_mean_sefxn)) +
  geom_errorbar(aes(ymin=pMortality_mean -pMortality_sefxn, ymax=pMortality_mean + pMortality_sefxn)) +
  facet_wrap(~extremes) 
#facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by basal_area', x = 'basal_area', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=mortality_extremes, aes(y=tiller_num_rr_mean, x=SLA_mean_mean, group= as.factor(extremes), color = as.factor(extremes))) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgray", linewidth=1.5) +
  geom_point() +
  geom_errorbarh(aes(xmin=SLA_mean_mean - SLA_mean_sefxn, xmax=SLA_mean_mean + SLA_mean_sefxn)) +
  geom_errorbar(aes(ymin=tiller_num_rr_mean - tiller_num_rr_sefxn, ymax=tiller_num_rr_mean + tiller_num_rr_sefxn)) +
  facet_wrap(~block) 
#facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by basal_area', x = 'basal_area', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=mortality_extremes, aes(x=pMortality_mean, y=tiller_num_rr_mean, group= block, color = block)) +
  geom_point() +
  geom_errorbar(aes(ymin=tiller_num_rr_mean - tiller_num_rr_sefxn, ymax=tiller_num_rr_mean + tiller_num_rr_sefxn)) +
  geom_errorbarh(aes(xmin=pMortality_mean - pMortality_sefxn, xmax=pMortality_mean + pMortality_sefxn)) +
  facet_wrap(~extremes) 
#facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by basal_area', x = 'basal_area', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

#Graphs from earlier but with 2025 pop demo
ggplot(data=mortality_extremes, aes(x=SLA_mean_mean, y=tiller_num_rr_mean, group= extremes, color = block)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgray", linewidth=1.5) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=tiller_num_rr_mean - tiller_num_rr_sefxn, ymax=tiller_num_rr_mean + tiller_num_rr_sefxn)) +
  geom_errorbarh(aes(xmin=SLA_mean_mean - SLA_mean_sefxn, xmax=SLA_mean_mean + SLA_mean_sefxn)) +
  facet_wrap(~extremes) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by SLA', x = 'SLA (cm/g)', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=mortality_extremes, aes(x=SLA_mean_mean, y=tiller_num_rr_mean, group= as.factor(extremes), color = as.factor(extremes))) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=tiller_num_rr_mean - tiller_num_rr_sefxn, ymax=tiller_num_rr_mean + tiller_num_rr_sefxn)) +
  geom_errorbarh(aes(xmin=SLA_mean_mean - SLA_mean_sefxn, xmax=SLA_mean_mean + SLA_mean_sefxn)) +
  labs(title = 'Tiller Number Response Ratio by SLA', x = 'SLA (cm/g)', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=mortality_extremes, aes(x=SLA_mean_mean, y=SRL_mean_mean, group= as.factor(extremes), color = block)) +
  geom_point() +
  #geom_smooth(method=lm, se=FALSE) +
  geom_errorbar(aes(ymin=SRL_mean_mean - SRL_mean_sefxn, ymax=SRL_mean_mean + SRL_mean_sefxn)) +
  geom_errorbarh(aes(xmin=SLA_mean_mean - SLA_mean_sefxn, xmax=SLA_mean_mean + SLA_mean_sefxn)) +
  #facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  labs(title = 'SRL by SLA', x = 'SLA (cm/g)', y = 'SRL') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=mortality_extremes, aes(x=SRL_mean_mean, y=tiller_num_rr_mean, group= extremes, color = block)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgray", linewidth=1.5) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=tiller_num_rr_mean - tiller_num_rr_sefxn, ymax=tiller_num_rr_mean + tiller_num_rr_sefxn)) +
  geom_errorbarh(aes(xmin=SRL_mean_mean - SRL_mean_sefxn, xmax=SRL_mean_mean + SRL_mean_sefxn)) +
  facet_wrap(~extremes) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by SRL', x = 'SRL (cm/g)', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=mortality_extremes, aes(x=SRL_mean_mean, y=tiller_num_rr_mean, group= as.factor(extremes), color = as.factor(extremes))) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=tiller_num_rr_mean - tiller_num_rr_sefxn, ymax=tiller_num_rr_mean + tiller_num_rr_sefxn)) +
  geom_errorbarh(aes(xmin=SRL_mean_mean - SRL_mean_sefxn, xmax=SRL_mean_mean + SRL_mean_sefxn)) +
  labs(title = 'Tiller Number Response Ratio by SRL', x = 'SRL (cm/g)', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=mortality_extremes, aes(x=LDMC_mean_mean, y=tiller_num_rr_mean, group= extremes, color = block)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgray", linewidth=1.5) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=tiller_num_rr_mean - tiller_num_rr_sefxn, ymax=tiller_num_rr_mean + tiller_num_rr_sefxn)) +
  geom_errorbarh(aes(xmin=LDMC_mean_mean - LDMC_mean_sefxn, xmax=LDMC_mean_mean + LDMC_mean_sefxn)) +
  facet_wrap(~extremes) +
  scale_color_brewer(palette = "Set2") +
  labs(title = 'Tiller Number Response Ratio by LDMC', x = 'LDMC (cm/g)', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data=mortality_extremes, aes(x=LDMC_mean_mean, y=tiller_num_rr_mean, group= as.factor(extremes), color = as.factor(extremes))) +
  geom_point() +
  geom_smooth(method=lm,  
              se=FALSE) +
  geom_errorbar(aes(ymin=tiller_num_rr_mean - tiller_num_rr_sefxn, ymax=tiller_num_rr_mean + tiller_num_rr_sefxn)) +
  geom_errorbarh(aes(xmin=LDMC_mean_mean - LDMC_mean_sefxn, xmax=LDMC_mean_mean + LDMC_mean_sefxn)) +
  labs(title = 'Tiller Number Response Ratio by LDMC', x = 'LDMC (cm/g)', y = 'RR') +
  theme_bw() +
  theme(legend.position = "bottom")

traits_plant_tag <- popdemo_pre_post %>% 
  select(!(species)) %>% 
  full_join(mean_leaf_calculations) %>% 
  full_join(roots)

library(readr)
traits_indiv <- read_csv("C:/Users/User/OneDrive - UNCG/UNCG/Wilcox Lab/Research/Dissertation/Ch4-Traits/NExS 2023_leaf area thickness drymass wetmass SLA LDMC tillerDiam_dom species_CLEANED_kw_20250729.csv") %>% 
  group_by(block, plot, plant_tag, species) %>% 
  summarize((across(.cols=c(total_area:LDMC),
                    .fns=list(mean=mean),
                    na.rm=T))) %>% 
  ungroup() %>% 
  full_join(roots) %>% 
  full_join(popdemo_pre_post) %>% 
  mutate(dead_flag=replace_na(dead_flag, 0)) %>% 
  filter((dead_flag !=1)) %>% 
  filter(fire!='EF') %>% 
  filter(year != 2024) %>% 
  unite(treatment, drought, grazing) %>% 
  mutate(Treatment = dplyr::recode(treatment,
                                                            "D_G" = "Drought & Grazing",
                                                            "D_NG" = "Drought",
                                                            "ND_G" = "Grazing",
                                                            "ND_NG" = "Control")) %>% 
  mutate(Species = dplyr::recode(species,
                                 "ari con" = "Aristida congesta",
                                 "the tri" = "Themeda triandra",
                                 "bot rad" = "Bothriochloa radicans",
                                 "pan col" = "Panicum coloratum", 
                                 "pan max" = "Megathyrsus maximus"))

ggplot(traits_indiv, aes(x=LDMC_mean, y =tiller_num_lnrr, group = species, color= species)) +
  geom_point() +
  facet_wrap(~Treatment)

ggplot(traits_indiv, aes(x=SRL, y =tiller_num_lnrr, group = species, color= species)) +
  geom_point() +
  facet_wrap(~Treatment)

popdemo_tiller_RR_treat <- popdemo_tiller_RR %>% 
  group_by(Treatment) %>% 
  summarize((across(.cols=c(tiller_num_lnrr_mean),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T))) %>% 
  ungroup() %>% 
  mutate(extremes = dplyr::recode(Treatment,
                                  "Drought & Grazing" = 4,
                                  "Drought" = 3,
                                  "Grazing" = 2,
                                  "Control" = 1
  ))


#### ESA code for real this time

cbPalette_4 <- c("#E69F00", "#0072B2", "#009E73", "#CC79A7")

#F1
ggplot(data=popdemo_tiller_RR_treat, aes(x=reorder(Treatment, extremes), y=tiller_num_lnrr_mean_mean, color=Treatment)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgray", linewidth=1.5) +
  geom_point(size = 12) + 
  scale_color_discrete(breaks=c('Control', 'Grazing', 'Drought', 'Drought & Grazing'), type = cbPalette_4) +
  geom_errorbar(aes(ymin=tiller_num_lnrr_mean_mean - tiller_num_lnrr_mean_sefxn, ymax=tiller_num_lnrr_mean_mean + tiller_num_lnrr_mean_sefxn, width = 0.25)) +
  geom_smooth(method=lm,  
              se=FALSE) +
  theme_bw() +
  labs(title = 'Tiller Number Response Ratio by Treatment', x = 'Treatment', y = 'Tiller Number Response Ratio') +
  theme(text = element_text(size = 40)) +
  theme(legend.text = element_text(size = 20))

popdemo_tiller_rr_25 <- popdemo_pre_post %>% 
  filter(year == 2025) %>% 
  filter(fire == "AF") %>% 
  unite(treatment, drought, grazing) %>% 
  mutate(Treatment = dplyr::recode(treatment,
                                   "D_G" = "Drought & Grazing",
                                   "D_NG" = "Drought",
                                   "ND_G" = "Grazing",
                                   "ND_NG" = "Control")) %>% 
  select(block, plant_tag, species, Treatment, tiller_num_lnrr) %>% 
  filter(tiller_num_lnrr != -Inf)

  

hist(sqrt(popdemo_tiller_rr_25$tiller_num_lnrr))
shapiro.test(sqrt(popdemo_tiller_rr_25$tiller_num_lnrr))

tiller_treat_aov <- aov(formula = sqrt(popdemo_tiller_rr_25$tiller_num_lnrr) ~ Treatment,
              data = popdemo_tiller_rr_25)
summary(tiller_treat_aov)
TukeyHSD(tiller_treat_aov)

#F2
popdemo_tiller_RR <- popdemo_lnrr_means %>% 
  filter(year == 2025) %>% 
  filter(year == 2025) %>% 
  filter(fire == 'AF') %>% 
  unite(treatment, drought, grazing) %>% 
  mutate(Treatment = dplyr::recode(treatment,
                                   "D_G" = "Drought & Grazing",
                                   "D_NG" = "Drought",
                                   "ND_G" = "Grazing",
                                   "ND_NG" = "Control")) %>% 
  mutate(extremes = dplyr::recode(Treatment,
                                  "Drought & Grazing" = 3,
                                  "Drought" = 2,
                                  "Grazing" = 1,
                                  "Control" = 0
  )) %>% 
  mutate(Species = dplyr::recode(species,
                                 "ari_con" = "Aristida congesta",
                                 "the_tri" = "Themeda triandra",
                                 "bot_rad" = "Bothriochloa radicans",
                                 "pan_col" = "Panicum coloratum", 
                                 "pan_max" = "Megathyrsus maximus")) 

  
ggplot(data=popdemo_tiller_RR, aes(x=reorder(Treatment, extremes), y=tiller_num_lnrr_mean, group = Species, color=Treatment)) + 
  geom_hline(yintercept=0, linetype="dashed", 
               color = "darkgray", linewidth=1.5) +
  geom_point(size = 8) +
  geom_errorbar(aes(ymin=tiller_num_lnrr_mean - tiller_num_lnrr_se, ymax=tiller_num_lnrr_mean + tiller_num_lnrr_se, width = 0.25)) +
  scale_color_discrete(breaks=c('Control', 'Drought', 'Grazing', 'Drought & Grazing'), type = cbPalette_4) +
    facet_wrap(~Species) +
  theme_bw() +
    labs(title = 'Tiller Number Response Ratio by Treatment', x = 'Treatment', y = 'Tiller Number Response Ratio') +
    theme(text = element_text(size = 30),
          axis.text.x = element_text(angle = 45, hjust = 1))

#F3
traits_single <- traits_indiv %>% 
  filter(Treatment != "Drought & Grazing") %>% 
  filter(Treatment != "Control") 
  #filter(species != 'ari_con')
  
traits_single <- traits_indiv %>% 
  filter(Treatment == "Grazing") %>% 
  filter(species != 'ari_con')

  ggplot(traits_single, aes(x=LDMC_mean, y =tiller_num_lnrr, group = Treatment, color= Species)) +
    geom_hline(yintercept=0, linetype="dashed", 
               color = "darkgray", linewidth=1.5) +
    geom_point(size = 10) +
    scale_color_brewer(palette = "Set2") +
    geom_smooth(method=lm,  
                se=FALSE) +
    #facet_grid(Species~Treatment)
    facet_wrap(~Treatment) +
    labs(title = 'Tiller Number Response Ratio by LDMC', x = 'LDMC', y = 'Tiller Number Response Ratio') +
    theme(text = element_text(size = 30)) +
    theme(legend.text = element_text(size = 40))
  
  
  ggplot(traits_single, aes(x=SLA_mean, y =tiller_num_lnrr, group = Treatment, color= species)) +
    geom_hline(yintercept=0, linetype="dashed", 
               color = "darkgray", linewidth=1.5) +
    geom_point(size = 10) +
    scale_color_brewer(palette = "Set2") +
    geom_smooth(method=lm,  
                se=FALSE) +
    facet_wrap(~Treatment) +
    labs(title = 'Tiller Number Response Ratio by SLA', x = 'SLA', y = 'Tiller Number Response Ratio') +
    theme(text = element_text(size = 30)) +
    theme(legend.text = element_text(size = 40))
  
  ggplot(traits_single, aes(x=SRL, y =tiller_num_lnrr, group = Treatment, color= species)) +
    geom_hline(yintercept=0, linetype="dashed", 
               color = "darkgray", linewidth=1.5) +
    geom_point(size = 10) +
    scale_color_brewer(palette = "Set2") +
    geom_smooth(method=lm,  
                se=FALSE) +
    facet_wrap(~Treatment) +
    labs(title = 'Tiller Number Response Ratio by SRL', x = 'SRL', y = 'Tiller Number Response Ratio') +
    theme(text = element_text(size = 30)) +
    theme(legend.text = element_text(size = 40))
  
#F4
  traits_combined <- traits_indiv %>% 
    filter(Treatment == "Drought & Grazing") %>% 
    filter(species != 'ari_con')
  
  ggplot(traits_combined, aes(x=LDMC_mean, y =tiller_num_lnrr, group = Treatment, color= species)) +
    geom_hline(yintercept=0, linetype="dashed", 
               color = "darkgray", linewidth=1.5) +
    geom_point(size = 10) +
    scale_color_brewer(palette = "Set2") +
    geom_smooth(method=lm,  
                se=FALSE) +
    labs(title = 'Tiller Number Response Ratio by LDMC', x = 'LDMC', y = 'Tiller Number Response Ratio') +
    theme(text = element_text(size = 30)) +
    theme(legend.text = element_text(size = 40))
  
  ggplot(traits_combined, aes(x=SLA_mean, y =tiller_num_lnrr, group = Treatment, color= species)) +
    geom_hline(yintercept=0, linetype="dashed", 
               color = "darkgray", linewidth=1.5) +
    geom_point(size = 10) +
    scale_color_brewer(palette = "Set2") +
    geom_smooth(method=lm,  
                se=FALSE) +
    labs(title = 'Tiller Number Response Ratio by SLA', x = 'SLA', y = 'Tiller Number Response Ratio') +
    theme(text = element_text(size = 30)) +
    theme(legend.text = element_text(size = 40))
  
  ggplot(traits_combined, aes(x=SRL, y =tiller_num_lnrr, group = Treatment, color= species)) +
    geom_hline(yintercept=0, linetype="dashed", 
               color = "darkgray", linewidth=1.5) +
    geom_point(size = 10) +
    scale_color_brewer(palette = "Set2") +
    geom_smooth(method=lm,  
                se=FALSE) +
    labs(title = 'Tiller Number Response Ratio by SRL', x = 'SRL', y = 'Tiller Number Response Ratio') +
    theme(text = element_text(size = 30)) +
    theme(legend.text = element_text(size = 40))
  
  ggplot(traits_single, aes(x=LDMC_mean, y =tiller_num_lnrr, group = Treatment, color = Treatment)) +
    geom_point() +
    geom_smooth(method=lm,  
                se=FALSE) +
    facet_wrap(~species) +
    scale_color_brewer(palette = "Set2") +
    labs(title = 'Tiller Number Response Ratio by LDMC', x = 'LDMC', y = 'RR') +
    theme_bw() +
    theme(legend.position = "bottom")
  
#thinking
rr_blocks <- popdemo_lnrr_means %>% 
  full_join(mean_traits_full) %>% 
  filter(year == 2025) %>% 
  filter(fire == 'AF') %>% 
  unite(treatment, drought, grazing) %>% 
  mutate(Treatment = dplyr::recode(treatment,
                                   "D_G" = "Drought & Grazing",
                                   "D_NG" = "Drought",
                                   "ND_G" = "Grazing",
                                   "ND_NG" = "Control")) %>% 
  filter(Treatment != "Control") #%>%  filter(Treatment != "Drought & Grazing")
 
  
  ggplot(data=rr_blocks, aes(x=SLA_mean, y=tiller_num_lnrr_mean, group= Treatment, color = block)) +
    geom_point() +
    geom_smooth(method=lm,  
                se=FALSE) +
    geom_errorbar(aes(ymin=tiller_num_lnrr_mean - tiller_num_lnrr_se, ymax=tiller_num_lnrr_mean + tiller_num_lnrr_se)) +
    geom_errorbarh(aes(xmin=SLA_mean - SLA_sefxn, xmax=SLA_mean + SLA_sefxn)) +
    facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
    scale_color_brewer(palette = "Set2") +
    labs(title = 'Tiller Number Response Ratio by SLA', x = 'SLA (cm/g)', y = 'RR') +
    theme_bw() +
    theme(legend.position = "bottom")
  
  ggplot(data=rr_blocks, aes(x=SRL_mean, y=tiller_num_lnrr_mean, group= Treatment, color = block)) +
    geom_hline(yintercept=0, linetype="dashed", 
               color = "darkgray", linewidth=1.5) +
    geom_point(size = 8) +
    geom_point() +
    geom_smooth(method=lm,  
                se=FALSE) +
    geom_errorbar(aes(ymin=tiller_num_lnrr_mean - tiller_num_lnrr_se, ymax=tiller_num_lnrr_mean + tiller_num_lnrr_se)) +
    geom_errorbarh(aes(xmin=SRL_mean - SRL_sefxn, xmax=SRL_mean + SRL_sefxn)) +
    facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
    scale_color_brewer(palette = "Set2") +
    labs(title = 'Tiller Number Response Ratio by SRL', x = 'SRL (cm/g)', y = 'RR') +
    theme_bw() +
    theme(legend.position = "bottom")
  
  ggplot(data=rr_blocks, aes(x=SLA_mean, y=tiller_num_lnrr_mean, group= Treatment, color = Treatment)) +
    geom_hline(yintercept=0, linetype="dashed", 
               color = "darkgray", linewidth=1.5) +
    geom_point(size = 8) +
    geom_smooth(method=lm,  
                se=FALSE) +
    geom_errorbar(aes(ymin=tiller_num_lnrr_mean - tiller_num_lnrr_se, ymax=tiller_num_lnrr_mean + tiller_num_lnrr_se)) +
    geom_errorbarh(aes(xmin=SLA_mean - SLA_sefxn, xmax=SLA_mean + SLA_sefxn)) +
    #facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
    scale_color_discrete(breaks=c('Drought', 'Grazing', 'Drought & Grazing')) +
    labs(title = 'Tiller Number Response Ratio by SLA', x = 'SLA', y = 'RR') +
    theme_bw() +
    theme(legend.position = "bottom") +
    theme(text = element_text(size = 30)) +
    theme(legend.text = element_text(size = 40))
  
  ggplot(data=rr_blocks, aes(x=SRL_mean, y=tiller_num_lnrr_mean, group= Treatment, color = Treatment)) +
    geom_hline(yintercept=0, linetype="dashed", 
               color = "darkgray", linewidth=1.5) +
    geom_point(size = 8) +
    geom_point(size = 8) +
    geom_smooth(method=lm,  
                se=FALSE) +
    geom_errorbar(aes(ymin=tiller_num_lnrr_mean - tiller_num_lnrr_se, ymax=tiller_num_lnrr_mean + tiller_num_lnrr_se)) +
    geom_errorbarh(aes(xmin=SRL_mean - SRL_sefxn, xmax=SRL_mean + SRL_sefxn)) +
    #facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
    scale_color_discrete(breaks=c('Drought', 'Grazing', 'Drought & Grazing')) +
    labs(title = 'Tiller Number Response Ratio by SRL', x = 'SRL', y = 'RR') +
    theme_bw() +
    theme(legend.position = "bottom") +
    theme(text = element_text(size = 30)) +
    theme(legend.text = element_text(size = 40))

#Ecophys
eco_phys <- read_csv("C:/Users/User/OneDrive - UNCG/UNCG/Wilcox Lab/Research/Dissertation/Ch4-Traits/NExS_ACi_C4doms_pretreat_outliers_marked.csv") %>% 
  select(block, plot, species, plant_tag, Rubisco_Vcmax, Light_Jmax) %>% 
  mutate(species = dplyr::recode(species,
                                 'ARICON' = "ari_con",
                                 'THETRI' = "the_tri",
                                 'BOTRAD' = "bot_rad",
                                 'PANCOL' = "pan_col", 
                                 "PANMAX" = "pan_max")) %>% 
  filter(Light_Jmax <= 500)

traits_treat <- traits_indiv  %>% 
  left_join(eco_phys) 


#Slopes graphs
ggplot(traits_treat, aes(x=Rubisco_Vcmax, y =tiller_num_lnrr, group = Treatment, color= Species)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgray", linewidth=1.5) +
  geom_point(size = 10) +
  scale_color_brewer(palette = "Set2") +
  geom_smooth(method=lm,  
              se=FALSE) +
  facet_grid(~factor(Treatment, levels=c('Control' , 'Drought', 'Grazing', 'Drought & Grazing'))) +
  theme_bw() +
  labs(title = 'Tiller Number Response Ratio by Vcmax', x = 'Vcmax', y = 'Tiller Number Response Ratio') +
  theme(text = element_text(size = 30)) +
  theme(legend.text = element_text(size = 40))

ggplot(traits_treat, aes(x=Light_Jmax, y =tiller_num_lnrr, group = Treatment, color= Species)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgray", linewidth=1.5) +
  geom_point(size = 10) +
  scale_color_brewer(palette = "Set2") +
  geom_smooth(method=lm,  
              se=FALSE) +
  facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  theme_bw() +
  labs(title = 'Tiller Number Response Ratio by Jmax', x = 'Jmax', y = 'Tiller Number Response Ratio') +
  theme(text = element_text(size = 30)) +
  theme(legend.text = element_text(size = 40))

ggplot(traits_treat, aes(x=LDMC_mean, y =tiller_num_lnrr, group = Treatment, color= Species)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgray", linewidth=1.5) +
  geom_point(size = 10) +
  scale_color_brewer(palette = "Set2") +
  geom_smooth(method=lm,  
              se=FALSE) +
  facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  theme_bw() +
  labs(title = 'Tiller Number Response Ratio by LDMC', x = 'LDMC', y = 'Tiller Number Response Ratio') +
  theme(text = element_text(size = 30)) +
  theme(legend.text = element_text(size = 40))


ggplot(traits_treat, aes(x=SLA_mean, y =tiller_num_lnrr, group = Treatment, color= species)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgray", linewidth=1.5) +
  geom_point(size = 10) +
  scale_color_brewer(palette = "Set2") +
  geom_smooth(method=lm,  
              se=FALSE) +
  facet_grid(~factor(Treatment, levels=c('Control', 'Grazing', 'Drought', 'Drought & Grazing'))) +
  theme_bw() +
  labs(title = 'Tiller Number Response Ratio by SLA', x = 'SLA', y = 'Tiller Number Response Ratio') +
  theme(text = element_text(size = 40)) +
  theme(legend.text = element_text(size = 40))

ggplot(traits_treat, aes(x=SRL, y =tiller_num_lnrr, group = Treatment, color= species)) +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "darkgray", linewidth=1.5) +
  geom_point(size = 10) +
  scale_color_brewer(palette = "Set2") +
  geom_smooth(method=lm,  
              se=FALSE) +
  facet_grid(~factor(Treatment, levels=c('Drought', 'Grazing', 'Drought & Grazing'))) +
  theme_bw() +
  labs(title = 'Tiller Number Response Ratio by SRL', x = 'SRL', y = 'Tiller Number Response Ratio') +
  theme(text = element_text(size = 30)) +
  theme(legend.text = element_text(size = 40))

m1 <- lm(tiller_num_lnrr ~ SLA_mean, data = filter(traits_treat, Treatment == "Drought"))
summary(m1)
plot(m1, which = 2, col = 'dodgerblue')

slope_out = {}

#########SLA

#Drought
SLA_drought_model <- lm(tiller_num_lnrr ~ SLA_mean, data=filter(traits_treat, Treatment =="Drought"))
summary_SLA_drought_model <- summary(SLA_drought_model)
anova_model_SLA_drought_model <- anova(SLA_drought_model) 

out_model_SLA_drought <- data.frame(Treatment="Drought",
                                       metric="SLA",
                                       Intercept = summary_SLA_drought_model$coefficients[1],
                                       Slope = summary_SLA_drought_model$coefficients[2],
                                       Slope_se = summary_SLA_drought_model$coefficients[4],
                                       P_val=anova_model_SLA_drought_model$`Pr(>F)`[1],
                                       R2=summary_SLA_drought_model$r.squared,
                                       adj_R2=summary_SLA_drought_model$adj.r.squared)

#Grazing
SLA_Grazing_model <- lm(tiller_num_lnrr ~ SLA_mean, data=filter(traits_treat, Treatment =="Grazing"))
summary_SLA_Grazing_model <- summary(SLA_Grazing_model)
anova_model_SLA_Grazing_model <- anova(SLA_Grazing_model) 

out_model_SLA_Grazing <- data.frame(Treatment="Grazing",
                                    metric="SLA",
                                    Intercept = summary_SLA_Grazing_model$coefficients[1],
                                    Slope = summary_SLA_Grazing_model$coefficients[2],
                                    Slope_se = summary_SLA_Grazing_model$coefficients[4],
                                    P_val=anova_model_SLA_Grazing_model$`Pr(>F)`[1],
                                    R2=summary_SLA_Grazing_model$r.squared,
                                    adj_R2=summary_SLA_Grazing_model$adj.r.squared)

#Drought & Grazing
SLA_DroughtGrazing_model <- lm(tiller_num_lnrr ~ SLA_mean, data=filter(traits_treat, Treatment =="Drought & Grazing"))
summary_SLA_DroughtGrazing_model <- summary(SLA_DroughtGrazing_model)
anova_model_SLA_DroughtGrazing_model <- anova(SLA_DroughtGrazing_model) 

out_model_SLA_DroughtGrazing <- data.frame(Treatment="Drought & Grazing",
                                    metric="SLA",
                                    Intercept = summary_SLA_DroughtGrazing_model$coefficients[1],
                                    Slope = summary_SLA_DroughtGrazing_model$coefficients[2],
                                    Slope_se = summary_SLA_DroughtGrazing_model$coefficients[4],
                                    P_val=anova_model_SLA_DroughtGrazing_model$`Pr(>F)`[1],
                                    R2=summary_SLA_DroughtGrazing_model$r.squared,
                                    adj_R2=summary_SLA_DroughtGrazing_model$adj.r.squared)

#########SRL

#Drought
SRL_drought_model <- lm(tiller_num_lnrr ~ SRL, data=filter(traits_treat, Treatment =="Drought"))
summary_SRL_drought_model <- summary(SRL_drought_model)
anova_model_SRL_drought_model <- anova(SRL_drought_model) 

out_model_SRL_drought <- data.frame(Treatment="Drought",
                                    metric="SRL",
                                    Intercept = summary_SRL_drought_model$coefficients[1],
                                    Slope = summary_SRL_drought_model$coefficients[2],
                                    Slope_se = summary_SRL_drought_model$coefficients[4],
                                    P_val=anova_model_SRL_drought_model$`Pr(>F)`[1],
                                    R2=summary_SRL_drought_model$r.squared,
                                    adj_R2=summary_SRL_drought_model$adj.r.squared)

#Grazing
SRL_Grazing_model <- lm(tiller_num_lnrr ~ SRL, data=filter(traits_treat, Treatment =="Grazing"))
summary_SRL_Grazing_model <- summary(SRL_Grazing_model)
anova_model_SRL_Grazing_model <- anova(SRL_Grazing_model) 

out_model_SRL_Grazing <- data.frame(Treatment="Grazing",
                                    metric="SRL",
                                    Intercept = summary_SRL_Grazing_model$coefficients[1],
                                    Slope = summary_SRL_Grazing_model$coefficients[2],
                                    Slope_se = summary_SRL_Grazing_model$coefficients[4],
                                    P_val=anova_model_SRL_Grazing_model$`Pr(>F)`[1],
                                    R2=summary_SRL_Grazing_model$r.squared,
                                    adj_R2=summary_SRL_Grazing_model$adj.r.squared)

#Drought & Grazing
SRL_DroughtGrazing_model <- lm(tiller_num_lnrr ~ SRL, data=filter(traits_treat, Treatment =="Drought & Grazing"))
summary_SRL_DroughtGrazing_model <- summary(SRL_DroughtGrazing_model)
anova_model_SRL_DroughtGrazing_model <- anova(SRL_DroughtGrazing_model) 

out_model_SRL_DroughtGrazing <- data.frame(Treatment="Drought & Grazing",
                                           metric="SRL",
                                           Intercept = summary_SRL_DroughtGrazing_model$coefficients[1],
                                           Slope = summary_SRL_DroughtGrazing_model$coefficients[2],
                                           Slope_se = summary_SRL_DroughtGrazing_model$coefficients[4],
                                           P_val=anova_model_SRL_DroughtGrazing_model$`Pr(>F)`[1],
                                           R2=summary_SRL_DroughtGrazing_model$r.squared,
                                           adj_R2=summary_SRL_DroughtGrazing_model$adj.r.squared)

#control
SRL_control_model <- lm(tiller_num_lnrr ~ SRL, data=filter(traits_treat, Treatment =="Control"))
summary_SRL_control_model <- summary(SRL_control_model)
anova_model_SRL_control_model <- anova(SRL_control_model) 

out_model_SRL_control <- data.frame(Treatment="Control",
                                    metric="SRL",
                                    Intercept = summary_SRL_control_model$coefficients[1],
                                    Slope = summary_SRL_control_model$coefficients[2],
                                    Slope_se = summary_SRL_control_model$coefficients[4],
                                    P_val=anova_model_SRL_control_model$`Pr(>F)`[1],
                                    R2=summary_SRL_control_model$r.squared,
                                    adj_R2=summary_SRL_control_model$adj.r.squared)

#########LDMC

#Drought
LDMC_drought_model <- lm(tiller_num_lnrr ~ LDMC_mean, data=filter(traits_treat, Treatment =="Drought"))
summary_LDMC_drought_model <- summary(LDMC_drought_model)
anova_model_LDMC_drought_model <- anova(LDMC_drought_model) 

out_model_LDMC_drought <- data.frame(Treatment="Drought",
                                    metric="LDMC",
                                    Intercept = summary_LDMC_drought_model$coefficients[1],
                                    Slope = summary_LDMC_drought_model$coefficients[2],
                                    Slope_se = summary_LDMC_drought_model$coefficients[4],
                                    P_val=anova_model_LDMC_drought_model$`Pr(>F)`[1],
                                    R2=summary_LDMC_drought_model$r.squared,
                                    adj_R2=summary_LDMC_drought_model$adj.r.squared)

#Grazing
LDMC_Grazing_model <- lm(tiller_num_lnrr ~ LDMC_mean, data=filter(traits_treat, Treatment =="Grazing"))
summary_LDMC_Grazing_model <- summary(LDMC_Grazing_model)
anova_model_LDMC_Grazing_model <- anova(LDMC_Grazing_model) 

out_model_LDMC_Grazing <- data.frame(Treatment="Grazing",
                                    metric="LDMC",
                                    Intercept = summary_LDMC_Grazing_model$coefficients[1],
                                    Slope = summary_LDMC_Grazing_model$coefficients[2],
                                    Slope_se = summary_LDMC_Grazing_model$coefficients[4],
                                    P_val=anova_model_LDMC_Grazing_model$`Pr(>F)`[1],
                                    R2=summary_LDMC_Grazing_model$r.squared,
                                    adj_R2=summary_LDMC_Grazing_model$adj.r.squared)

#Drought & Grazing
LDMC_DroughtGrazing_model <- lm(tiller_num_lnrr ~ LDMC_mean, data=filter(traits_treat, Treatment =="Drought & Grazing"))
summary_LDMC_DroughtGrazing_model <- summary(LDMC_DroughtGrazing_model)
anova_model_LDMC_DroughtGrazing_model <- anova(LDMC_DroughtGrazing_model) 

out_model_LDMC_DroughtGrazing <- data.frame(Treatment="Drought & Grazing",
                                           metric="LDMC",
                                           Intercept = summary_LDMC_DroughtGrazing_model$coefficients[1],
                                           Slope = summary_LDMC_DroughtGrazing_model$coefficients[2],
                                           Slope_se = summary_LDMC_DroughtGrazing_model$coefficients[4],
                                           P_val=anova_model_LDMC_DroughtGrazing_model$`Pr(>F)`[1],
                                           R2=summary_LDMC_DroughtGrazing_model$r.squared,
                                           adj_R2=summary_LDMC_DroughtGrazing_model$adj.r.squared)

#########Rubisco_Vcmax

#Drought
Rubisco_Vcmax_drought_model <- lm(tiller_num_lnrr ~ Rubisco_Vcmax, data=filter(traits_treat, Treatment =="Drought"))
summary_Rubisco_Vcmax_drought_model <- summary(Rubisco_Vcmax_drought_model)
anova_model_Rubisco_Vcmax_drought_model <- anova(Rubisco_Vcmax_drought_model) 

out_model_Rubisco_Vcmax_drought <- data.frame(Treatment="Drought",
                                     metric="Rubisco_Vcmax",
                                     Intercept = summary_Rubisco_Vcmax_drought_model$coefficients[1],
                                     Slope = summary_Rubisco_Vcmax_drought_model$coefficients[2],
                                     Slope_se = summary_Rubisco_Vcmax_drought_model$coefficients[4],
                                     P_val=anova_model_Rubisco_Vcmax_drought_model$`Pr(>F)`[1],
                                     R2=summary_Rubisco_Vcmax_drought_model$r.squared,
                                     adj_R2=summary_Rubisco_Vcmax_drought_model$adj.r.squared)

#Grazing
Rubisco_Vcmax_Grazing_model <- lm(tiller_num_lnrr ~ Rubisco_Vcmax, data=filter(traits_treat, Treatment =="Grazing"))
summary_Rubisco_Vcmax_Grazing_model <- summary(Rubisco_Vcmax_Grazing_model)
anova_model_Rubisco_Vcmax_Grazing_model <- anova(Rubisco_Vcmax_Grazing_model) 

out_model_Rubisco_Vcmax_Grazing <- data.frame(Treatment="Grazing",
                                     metric="Rubisco_Vcmax",
                                     Intercept = summary_Rubisco_Vcmax_Grazing_model$coefficients[1],
                                     Slope = summary_Rubisco_Vcmax_Grazing_model$coefficients[2],
                                     Slope_se = summary_Rubisco_Vcmax_Grazing_model$coefficients[4],
                                     P_val=anova_model_Rubisco_Vcmax_Grazing_model$`Pr(>F)`[1],
                                     R2=summary_Rubisco_Vcmax_Grazing_model$r.squared,
                                     adj_R2=summary_Rubisco_Vcmax_Grazing_model$adj.r.squared)

#Drought & Grazing
Rubisco_Vcmax_DroughtGrazing_model <- lm(tiller_num_lnrr ~ Rubisco_Vcmax, data=filter(traits_treat, Treatment =="Drought & Grazing"))
summary_Rubisco_Vcmax_DroughtGrazing_model <- summary(Rubisco_Vcmax_DroughtGrazing_model)
anova_model_Rubisco_Vcmax_DroughtGrazing_model <- anova(Rubisco_Vcmax_DroughtGrazing_model) 

out_model_Rubisco_Vcmax_DroughtGrazing <- data.frame(Treatment="Drought & Grazing",
                                            metric="Rubisco_Vcmax",
                                            Intercept = summary_Rubisco_Vcmax_DroughtGrazing_model$coefficients[1],
                                            Slope = summary_Rubisco_Vcmax_DroughtGrazing_model$coefficients[2],
                                            Slope_se = summary_Rubisco_Vcmax_DroughtGrazing_model$coefficients[4],
                                            P_val=anova_model_Rubisco_Vcmax_DroughtGrazing_model$`Pr(>F)`[1],
                                            R2=summary_Rubisco_Vcmax_DroughtGrazing_model$r.squared,
                                            adj_R2=summary_Rubisco_Vcmax_DroughtGrazing_model$adj.r.squared)

#########Light_Jmax

#Drought
Light_Jmax_drought_model <- lm(tiller_num_lnrr ~ Light_Jmax, data=filter(traits_treat, Treatment =="Drought"))
summary_Light_Jmax_drought_model <- summary(Light_Jmax_drought_model)
anova_model_Light_Jmax_drought_model <- anova(Light_Jmax_drought_model) 

out_model_Light_Jmax_drought <- data.frame(Treatment="Drought",
                                     metric="Light_Jmax",
                                     Intercept = summary_Light_Jmax_drought_model$coefficients[1],
                                     Slope = summary_Light_Jmax_drought_model$coefficients[2],
                                     Slope_se = summary_Light_Jmax_drought_model$coefficients[4],
                                     P_val=anova_model_Light_Jmax_drought_model$`Pr(>F)`[1],
                                     R2=summary_Light_Jmax_drought_model$r.squared,
                                     adj_R2=summary_Light_Jmax_drought_model$adj.r.squared)

#Grazing
Light_Jmax_Grazing_model <- lm(tiller_num_lnrr ~ Light_Jmax, data=filter(traits_treat, Treatment =="Grazing"))
summary_Light_Jmax_Grazing_model <- summary(Light_Jmax_Grazing_model)
anova_model_Light_Jmax_Grazing_model <- anova(Light_Jmax_Grazing_model) 

out_model_Light_Jmax_Grazing <- data.frame(Treatment="Grazing",
                                     metric="Light_Jmax",
                                     Intercept = summary_Light_Jmax_Grazing_model$coefficients[1],
                                     Slope = summary_Light_Jmax_Grazing_model$coefficients[2],
                                     Slope_se = summary_Light_Jmax_Grazing_model$coefficients[4],
                                     P_val=anova_model_Light_Jmax_Grazing_model$`Pr(>F)`[1],
                                     R2=summary_Light_Jmax_Grazing_model$r.squared,
                                     adj_R2=summary_Light_Jmax_Grazing_model$adj.r.squared)

#Drought & Grazing
Light_Jmax_DroughtGrazing_model <- lm(tiller_num_lnrr ~ Light_Jmax, data=filter(traits_treat, Treatment =="Drought & Grazing"))
summary_Light_Jmax_DroughtGrazing_model <- summary(Light_Jmax_DroughtGrazing_model)
anova_model_Light_Jmax_DroughtGrazing_model <- anova(Light_Jmax_DroughtGrazing_model) 

out_model_Light_Jmax_DroughtGrazing <- data.frame(Treatment="Drought & Grazing",
                                            metric="Light_Jmax",
                                            Intercept = summary_Light_Jmax_DroughtGrazing_model$coefficients[1],
                                            Slope = summary_Light_Jmax_DroughtGrazing_model$coefficients[2],
                                            Slope_se = summary_Light_Jmax_DroughtGrazing_model$coefficients[4],
                                            P_val=anova_model_Light_Jmax_DroughtGrazing_model$`Pr(>F)`[1],
                                            R2=summary_Light_Jmax_DroughtGrazing_model$r.squared,
                                            adj_R2=summary_Light_Jmax_DroughtGrazing_model$adj.r.squared)

#combine
slope_out <- rbind(slope_out, out_model_SLA_drought, out_model_SLA_Grazing, out_model_SLA_DroughtGrazing, out_model_SRL_drought, out_model_SRL_Grazing, out_model_SRL_DroughtGrazing, out_model_SRL_control, out_model_LDMC_drought, out_model_LDMC_Grazing, out_model_LDMC_DroughtGrazing, out_model_Rubisco_Vcmax_drought, out_model_Rubisco_Vcmax_Grazing, out_model_Rubisco_Vcmax_DroughtGrazing, out_model_Light_Jmax_drought, out_model_Light_Jmax_Grazing, out_model_Light_Jmax_DroughtGrazing)  %>% 
  mutate(extremes = ifelse(Treatment == "Drought & Grazing", 2, 1))

slope_out$Treatment <- factor(slope_out$Treatment, levels = c('Drought', 'Grazing', 'Drought & Grazing'))

cbPalette_3 <- c("#009E73", "#0072B2", "#CC79A7")

ggplot(data=slope_out, aes(x=reorder(metric, extremes), y=Slope, fill=Treatment)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Slope - Slope_se, ymax=Slope + Slope_se, width = 0.5) , position=position_dodge(width = 0.9)) +
  theme_bw() +
  scale_fill_discrete(breaks=c('Drought', 'Grazing', 'Drought & Grazing'), type = cbPalette_3) +
  labs(title = 'Slope of Tiller Number RR by Trait', x = 'Trait', y = 'Slope') +
  theme(legend.position = 'left') +
  theme(text = element_text(size = 30)) 
  

ggplot(data=filter(slope_out, metric == "LDMC" ), aes(x=metric, y=Slope, fill=Treatment)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Slope - Slope_se, ymax=Slope + Slope_se, width = 0.5) , position=position_dodge(width = 0.9)) +
  theme_bw() +
  scale_fill_discrete(breaks=c('Drought', 'Grazing', 'Drought & Grazing'), type = cbPalette_3) +
  labs(title = 'Slope of Tiller Number RR by LDMC', x = 'LDMC', y = 'Slope') +
  theme(legend.position = 'bottom') +
  theme(text = element_text(size = 30)) 

ggplot(data=filter(slope_out, metric == "SRL" ), aes(x=metric, y=Slope, fill=Treatment)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Slope - Slope_se, ymax=Slope + Slope_se, width = 0.5) , position=position_dodge(width = 0.9)) +
  theme_bw() +
  scale_fill_discrete(breaks=c('Control' , 'Drought', 'Grazing', 'Drought & Grazing'), type = cbPalette_3) +
  labs(x = 'Trait', y = 'Slope') +
  #theme(legend.position = 'none') +
  theme(text = element_text(size = 30)) 

traits_scaled <- traits_treat %>% 
  select(block, Treatment, tiller_num_lnrr, SLA_mean, LDMC_mean, SRL, Rubisco_Vcmax, Light_Jmax) %>% 
  mutate(tiller_num_lnrr = as.character(tiller_num_lnrr)) %>% 
  mutate(across(where(is.numeric), scale)) %>% 
  mutate(tiller_num_lnrr = as.numeric(tiller_num_lnrr)) %>% 
  mutate(LDMC_mean = -LDMC_mean)

slope_out = {}

#########SLA

#Control
SLA_Control_model <- lm(tiller_num_lnrr ~ SLA_mean, data=filter(traits_scaled, Treatment =="Control"))
summary_SLA_Control_model <- summary(SLA_Control_model)
anova_model_SLA_Control_model <- anova(SLA_Control_model) 

out_model_SLA_Control <- data.frame(Treatment="Control",
                                    metric="SLA",
                                    Intercept = summary_SLA_Control_model$coefficients[1],
                                    Slope = summary_SLA_Control_model$coefficients[2],
                                    Slope_se = summary_SLA_Control_model$coefficients[4],
                                    P_val=anova_model_SLA_Control_model$`Pr(>F)`[1],
                                    R2=summary_SLA_Control_model$r.squared,
                                    adj_R2=summary_SLA_Control_model$adj.r.squared)

#Drought
SLA_drought_model <- lm(tiller_num_lnrr ~ SLA_mean, data=filter(traits_scaled, Treatment =="Drought"))
summary_SLA_drought_model <- summary(SLA_drought_model)
anova_model_SLA_drought_model <- anova(SLA_drought_model) 

out_model_SLA_drought <- data.frame(Treatment="Drought",
                                    metric="SLA",
                                    Intercept = summary_SLA_drought_model$coefficients[1],
                                    Slope = summary_SLA_drought_model$coefficients[2],
                                    Slope_se = summary_SLA_drought_model$coefficients[4],
                                    P_val=anova_model_SLA_drought_model$`Pr(>F)`[1],
                                    R2=summary_SLA_drought_model$r.squared,
                                    adj_R2=summary_SLA_drought_model$adj.r.squared)

#Grazing
SLA_Grazing_model <- lm(tiller_num_lnrr ~ SLA_mean, data=filter(traits_scaled, Treatment =="Grazing"))
summary_SLA_Grazing_model <- summary(SLA_Grazing_model)
anova_model_SLA_Grazing_model <- anova(SLA_Grazing_model) 

out_model_SLA_Grazing <- data.frame(Treatment="Grazing",
                                    metric="SLA",
                                    Intercept = summary_SLA_Grazing_model$coefficients[1],
                                    Slope = summary_SLA_Grazing_model$coefficients[2],
                                    Slope_se = summary_SLA_Grazing_model$coefficients[4],
                                    P_val=anova_model_SLA_Grazing_model$`Pr(>F)`[1],
                                    R2=summary_SLA_Grazing_model$r.squared,
                                    adj_R2=summary_SLA_Grazing_model$adj.r.squared)

#Drought & Grazing
SLA_DroughtGrazing_model <- lm(tiller_num_lnrr ~ SLA_mean, data=filter(traits_scaled, Treatment =="Drought & Grazing"))
summary_SLA_DroughtGrazing_model <- summary(SLA_DroughtGrazing_model)
anova_model_SLA_DroughtGrazing_model <- anova(SLA_DroughtGrazing_model) 

out_model_SLA_DroughtGrazing <- data.frame(Treatment="Drought & Grazing",
                                           metric="SLA",
                                           Intercept = summary_SLA_DroughtGrazing_model$coefficients[1],
                                           Slope = summary_SLA_DroughtGrazing_model$coefficients[2],
                                           Slope_se = summary_SLA_DroughtGrazing_model$coefficients[4],
                                           P_val=anova_model_SLA_DroughtGrazing_model$`Pr(>F)`[1],
                                           R2=summary_SLA_DroughtGrazing_model$r.squared,
                                           adj_R2=summary_SLA_DroughtGrazing_model$adj.r.squared)

#########SRL

#Drought
SRL_drought_model <- lm(tiller_num_lnrr ~ SRL, data=filter(traits_scaled, Treatment =="Drought"))
summary_SRL_drought_model <- summary(SRL_drought_model)
anova_model_SRL_drought_model <- anova(SRL_drought_model) 

out_model_SRL_drought <- data.frame(Treatment="Drought",
                                    metric="SRL",
                                    Intercept = summary_SRL_drought_model$coefficients[1],
                                    Slope = summary_SRL_drought_model$coefficients[2],
                                    Slope_se = summary_SRL_drought_model$coefficients[4],
                                    P_val=anova_model_SRL_drought_model$`Pr(>F)`[1],
                                    R2=summary_SRL_drought_model$r.squared,
                                    adj_R2=summary_SRL_drought_model$adj.r.squared)

#Grazing
SRL_Grazing_model <- lm(tiller_num_lnrr ~ SRL, data=filter(traits_scaled, Treatment =="Grazing"))
summary_SRL_Grazing_model <- summary(SRL_Grazing_model)
anova_model_SRL_Grazing_model <- anova(SRL_Grazing_model) 

out_model_SRL_Grazing <- data.frame(Treatment="Grazing",
                                    metric="SRL",
                                    Intercept = summary_SRL_Grazing_model$coefficients[1],
                                    Slope = summary_SRL_Grazing_model$coefficients[2],
                                    Slope_se = summary_SRL_Grazing_model$coefficients[4],
                                    P_val=anova_model_SRL_Grazing_model$`Pr(>F)`[1],
                                    R2=summary_SRL_Grazing_model$r.squared,
                                    adj_R2=summary_SRL_Grazing_model$adj.r.squared)

#Drought & Grazing
SRL_DroughtGrazing_model <- lm(tiller_num_lnrr ~ SRL, data=filter(traits_scaled, Treatment =="Drought & Grazing"))
summary_SRL_DroughtGrazing_model <- summary(SRL_DroughtGrazing_model)
anova_model_SRL_DroughtGrazing_model <- anova(SRL_DroughtGrazing_model) 

out_model_SRL_DroughtGrazing <- data.frame(Treatment="Drought & Grazing",
                                           metric="SRL",
                                           Intercept = summary_SRL_DroughtGrazing_model$coefficients[1],
                                           Slope = summary_SRL_DroughtGrazing_model$coefficients[2],
                                           Slope_se = summary_SRL_DroughtGrazing_model$coefficients[4],
                                           P_val=anova_model_SRL_DroughtGrazing_model$`Pr(>F)`[1],
                                           R2=summary_SRL_DroughtGrazing_model$r.squared,
                                           adj_R2=summary_SRL_DroughtGrazing_model$adj.r.squared)

#control
SRL_control_model <- lm(tiller_num_lnrr ~ SRL, data=filter(traits_scaled, Treatment =="Control"))
summary_SRL_control_model <- summary(SRL_control_model)
anova_model_SRL_control_model <- anova(SRL_control_model) 

out_model_SRL_control <- data.frame(Treatment="Control",
                                    metric="SRL",
                                    Intercept = summary_SRL_control_model$coefficients[1],
                                    Slope = summary_SRL_control_model$coefficients[2],
                                    Slope_se = summary_SRL_control_model$coefficients[4],
                                    P_val=anova_model_SRL_control_model$`Pr(>F)`[1],
                                    R2=summary_SRL_control_model$r.squared,
                                    adj_R2=summary_SRL_control_model$adj.r.squared)

#########LDMC

#Control
LDMC_Control_model <- lm(tiller_num_lnrr ~ LDMC_mean, data=filter(traits_scaled, Treatment =="Control"))
summary_LDMC_Control_model <- summary(LDMC_Control_model)
anova_model_LDMC_Control_model <- anova(LDMC_Control_model) 

out_model_LDMC_Control <- data.frame(Treatment="Control",
                                     metric="LDMC",
                                     Intercept = summary_LDMC_Control_model$coefficients[1],
                                     Slope = summary_LDMC_Control_model$coefficients[2],
                                     Slope_se = summary_LDMC_Control_model$coefficients[4],
                                     P_val=anova_model_LDMC_Control_model$`Pr(>F)`[1],
                                     R2=summary_LDMC_Control_model$r.squared,
                                     adj_R2=summary_LDMC_Control_model$adj.r.squared)


#Drought
LDMC_drought_model <- lm(tiller_num_lnrr ~ LDMC_mean, data=filter(traits_scaled, Treatment =="Drought"))
summary_LDMC_drought_model <- summary(LDMC_drought_model)
anova_model_LDMC_drought_model <- anova(LDMC_drought_model) 

out_model_LDMC_drought <- data.frame(Treatment="Drought",
                                     metric="LDMC",
                                     Intercept = summary_LDMC_drought_model$coefficients[1],
                                     Slope = summary_LDMC_drought_model$coefficients[2],
                                     Slope_se = summary_LDMC_drought_model$coefficients[4],
                                     P_val=anova_model_LDMC_drought_model$`Pr(>F)`[1],
                                     R2=summary_LDMC_drought_model$r.squared,
                                     adj_R2=summary_LDMC_drought_model$adj.r.squared)

#Grazing
LDMC_Grazing_model <- lm(tiller_num_lnrr ~ LDMC_mean, data=filter(traits_scaled, Treatment =="Grazing"))
summary_LDMC_Grazing_model <- summary(LDMC_Grazing_model)
anova_model_LDMC_Grazing_model <- anova(LDMC_Grazing_model) 

out_model_LDMC_Grazing <- data.frame(Treatment="Grazing",
                                     metric="LDMC",
                                     Intercept = summary_LDMC_Grazing_model$coefficients[1],
                                     Slope = summary_LDMC_Grazing_model$coefficients[2],
                                     Slope_se = summary_LDMC_Grazing_model$coefficients[4],
                                     P_val=anova_model_LDMC_Grazing_model$`Pr(>F)`[1],
                                     R2=summary_LDMC_Grazing_model$r.squared,
                                     adj_R2=summary_LDMC_Grazing_model$adj.r.squared)

#Drought & Grazing
LDMC_DroughtGrazing_model <- lm(tiller_num_lnrr ~ LDMC_mean, data=filter(traits_scaled, Treatment =="Drought & Grazing"))
summary_LDMC_DroughtGrazing_model <- summary(LDMC_DroughtGrazing_model)
anova_model_LDMC_DroughtGrazing_model <- anova(LDMC_DroughtGrazing_model) 

out_model_LDMC_DroughtGrazing <- data.frame(Treatment="Drought & Grazing",
                                            metric="LDMC",
                                            Intercept = summary_LDMC_DroughtGrazing_model$coefficients[1],
                                            Slope = summary_LDMC_DroughtGrazing_model$coefficients[2],
                                            Slope_se = summary_LDMC_DroughtGrazing_model$coefficients[4],
                                            P_val=anova_model_LDMC_DroughtGrazing_model$`Pr(>F)`[1],
                                            R2=summary_LDMC_DroughtGrazing_model$r.squared,
                                            adj_R2=summary_LDMC_DroughtGrazing_model$adj.r.squared)

#########Rubisco_Vcmax

#Control
Rubisco_Vcmax_Control_model <- lm(tiller_num_lnrr ~ Rubisco_Vcmax, data=filter(traits_scaled, Treatment =="Control"))
summary_Rubisco_Vcmax_Control_model <- summary(Rubisco_Vcmax_Control_model)
anova_model_Rubisco_Vcmax_Control_model <- anova(Rubisco_Vcmax_Control_model) 

out_model_Rubisco_Vcmax_Control <- data.frame(Treatment="Control",
                                              metric="Rubisco_Vcmax",
                                              Intercept = summary_Rubisco_Vcmax_Control_model$coefficients[1],
                                              Slope = summary_Rubisco_Vcmax_Control_model$coefficients[2],
                                              Slope_se = summary_Rubisco_Vcmax_Control_model$coefficients[4],
                                              P_val=anova_model_Rubisco_Vcmax_Control_model$`Pr(>F)`[1],
                                              R2=summary_Rubisco_Vcmax_Control_model$r.squared,
                                              adj_R2=summary_Rubisco_Vcmax_Control_model$adj.r.squared)

#Drought
Rubisco_Vcmax_drought_model <- lm(tiller_num_lnrr ~ Rubisco_Vcmax, data=filter(traits_scaled, Treatment =="Drought"))
summary_Rubisco_Vcmax_drought_model <- summary(Rubisco_Vcmax_drought_model)
anova_model_Rubisco_Vcmax_drought_model <- anova(Rubisco_Vcmax_drought_model) 

out_model_Rubisco_Vcmax_drought <- data.frame(Treatment="Drought",
                                              metric="Rubisco_Vcmax",
                                              Intercept = summary_Rubisco_Vcmax_drought_model$coefficients[1],
                                              Slope = summary_Rubisco_Vcmax_drought_model$coefficients[2],
                                              Slope_se = summary_Rubisco_Vcmax_drought_model$coefficients[4],
                                              P_val=anova_model_Rubisco_Vcmax_drought_model$`Pr(>F)`[1],
                                              R2=summary_Rubisco_Vcmax_drought_model$r.squared,
                                              adj_R2=summary_Rubisco_Vcmax_drought_model$adj.r.squared)

#Grazing
Rubisco_Vcmax_Grazing_model <- lm(tiller_num_lnrr ~ Rubisco_Vcmax, data=filter(traits_scaled, Treatment =="Grazing"))
summary_Rubisco_Vcmax_Grazing_model <- summary(Rubisco_Vcmax_Grazing_model)
anova_model_Rubisco_Vcmax_Grazing_model <- anova(Rubisco_Vcmax_Grazing_model) 

out_model_Rubisco_Vcmax_Grazing <- data.frame(Treatment="Grazing",
                                              metric="Rubisco_Vcmax",
                                              Intercept = summary_Rubisco_Vcmax_Grazing_model$coefficients[1],
                                              Slope = summary_Rubisco_Vcmax_Grazing_model$coefficients[2],
                                              Slope_se = summary_Rubisco_Vcmax_Grazing_model$coefficients[4],
                                              P_val=anova_model_Rubisco_Vcmax_Grazing_model$`Pr(>F)`[1],
                                              R2=summary_Rubisco_Vcmax_Grazing_model$r.squared,
                                              adj_R2=summary_Rubisco_Vcmax_Grazing_model$adj.r.squared)

#Drought & Grazing
Rubisco_Vcmax_DroughtGrazing_model <- lm(tiller_num_lnrr ~ Rubisco_Vcmax, data=filter(traits_scaled, Treatment =="Drought & Grazing"))
summary_Rubisco_Vcmax_DroughtGrazing_model <- summary(Rubisco_Vcmax_DroughtGrazing_model)
anova_model_Rubisco_Vcmax_DroughtGrazing_model <- anova(Rubisco_Vcmax_DroughtGrazing_model) 

out_model_Rubisco_Vcmax_DroughtGrazing <- data.frame(Treatment="Drought & Grazing",
                                                     metric="Rubisco_Vcmax",
                                                     Intercept = summary_Rubisco_Vcmax_DroughtGrazing_model$coefficients[1],
                                                     Slope = summary_Rubisco_Vcmax_DroughtGrazing_model$coefficients[2],
                                                     Slope_se = summary_Rubisco_Vcmax_DroughtGrazing_model$coefficients[4],
                                                     P_val=anova_model_Rubisco_Vcmax_DroughtGrazing_model$`Pr(>F)`[1],
                                                     R2=summary_Rubisco_Vcmax_DroughtGrazing_model$r.squared,
                                                     adj_R2=summary_Rubisco_Vcmax_DroughtGrazing_model$adj.r.squared)

#########Light_Jmax

#Control
Light_Jmax_Control_model <- lm(tiller_num_lnrr ~ Light_Jmax, data=filter(traits_scaled, Treatment =="Control"))
summary_Light_Jmax_Control_model <- summary(Light_Jmax_Control_model)
anova_model_Light_Jmax_Control_model <- anova(Light_Jmax_Control_model) 

out_model_Light_Jmax_Control <- data.frame(Treatment="Control",
                                           metric="Light_Jmax",
                                           Intercept = summary_Light_Jmax_Control_model$coefficients[1],
                                           Slope = summary_Light_Jmax_Control_model$coefficients[2],
                                           Slope_se = summary_Light_Jmax_Control_model$coefficients[4],
                                           P_val=anova_model_Light_Jmax_Control_model$`Pr(>F)`[1],
                                           R2=summary_Light_Jmax_Control_model$r.squared,
                                           adj_R2=summary_Light_Jmax_Control_model$adj.r.squared)

#Drought
Light_Jmax_drought_model <- lm(tiller_num_lnrr ~ Light_Jmax, data=filter(traits_scaled, Treatment =="Drought"))
summary_Light_Jmax_drought_model <- summary(Light_Jmax_drought_model)
anova_model_Light_Jmax_drought_model <- anova(Light_Jmax_drought_model) 

out_model_Light_Jmax_drought <- data.frame(Treatment="Drought",
                                           metric="Light_Jmax",
                                           Intercept = summary_Light_Jmax_drought_model$coefficients[1],
                                           Slope = summary_Light_Jmax_drought_model$coefficients[2],
                                           Slope_se = summary_Light_Jmax_drought_model$coefficients[4],
                                           P_val=anova_model_Light_Jmax_drought_model$`Pr(>F)`[1],
                                           R2=summary_Light_Jmax_drought_model$r.squared,
                                           adj_R2=summary_Light_Jmax_drought_model$adj.r.squared)

#Grazing
Light_Jmax_Grazing_model <- lm(tiller_num_lnrr ~ Light_Jmax, data=filter(traits_scaled, Treatment =="Grazing"))
summary_Light_Jmax_Grazing_model <- summary(Light_Jmax_Grazing_model)
anova_model_Light_Jmax_Grazing_model <- anova(Light_Jmax_Grazing_model) 

out_model_Light_Jmax_Grazing <- data.frame(Treatment="Grazing",
                                           metric="Light_Jmax",
                                           Intercept = summary_Light_Jmax_Grazing_model$coefficients[1],
                                           Slope = summary_Light_Jmax_Grazing_model$coefficients[2],
                                           Slope_se = summary_Light_Jmax_Grazing_model$coefficients[4],
                                           P_val=anova_model_Light_Jmax_Grazing_model$`Pr(>F)`[1],
                                           R2=summary_Light_Jmax_Grazing_model$r.squared,
                                           adj_R2=summary_Light_Jmax_Grazing_model$adj.r.squared)

#Drought & Grazing
Light_Jmax_DroughtGrazing_model <- lm(tiller_num_lnrr ~ Light_Jmax, data=filter(traits_scaled, Treatment =="Drought & Grazing"))
summary_Light_Jmax_DroughtGrazing_model <- summary(Light_Jmax_DroughtGrazing_model)
anova_model_Light_Jmax_DroughtGrazing_model <- anova(Light_Jmax_DroughtGrazing_model) 

out_model_Light_Jmax_DroughtGrazing <- data.frame(Treatment="Drought & Grazing",
                                                  metric="Light_Jmax",
                                                  Intercept = summary_Light_Jmax_DroughtGrazing_model$coefficients[1],
                                                  Slope = summary_Light_Jmax_DroughtGrazing_model$coefficients[2],
                                                  Slope_se = summary_Light_Jmax_DroughtGrazing_model$coefficients[4],
                                                  P_val=anova_model_Light_Jmax_DroughtGrazing_model$`Pr(>F)`[1],
                                                  R2=summary_Light_Jmax_DroughtGrazing_model$r.squared,
                                                  adj_R2=summary_Light_Jmax_DroughtGrazing_model$adj.r.squared)

#combine
slope_out <- rbind(slope_out, out_model_SLA_drought, out_model_SLA_Grazing, out_model_SLA_Control, out_model_SLA_DroughtGrazing, out_model_SRL_drought, out_model_SRL_Grazing, out_model_SRL_DroughtGrazing, out_model_SRL_control, out_model_LDMC_drought, out_model_LDMC_Control, out_model_LDMC_Grazing, out_model_LDMC_DroughtGrazing, out_model_Rubisco_Vcmax_drought, out_model_Rubisco_Vcmax_Control,out_model_Rubisco_Vcmax_Grazing, out_model_Rubisco_Vcmax_DroughtGrazing, out_model_Light_Jmax_drought,out_model_Light_Jmax_Control, out_model_Light_Jmax_Grazing, out_model_Light_Jmax_DroughtGrazing)  %>% 
  mutate(order = ifelse(Treatment == "Drought & Grazing", 3, 1)) %>% 
  mutate(order = ifelse(Treatment == "Control", 0, order)) %>% 
  mutate(order = ifelse(Treatment == "Drought", 2, order))

slope_out$Treatment <- factor(slope_out$Treatment, levels = c('Drought & Grazing','Drought', 'Grazing','Control'))
cbPalette_4 <- c("#009E73","#0072B2","#CC79A7", "#E69F00")

ggplot(data=slope_out, aes(y=reorder(metric, order), x=Slope, fill=Treatment)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbarh(aes(xmin=Slope - Slope_se, xmax=Slope + Slope_se, height = 0.5) , position=position_dodge(width = 0.9)) +
  theme_bw() +
  scale_fill_discrete(type = cbPalette_4) +
  labs(title = 'Slope of Tiller Number RR by Trait', y = 'Trait', x = 'Slope') +
  theme(legend.position = 'top') +
  theme(text = element_text(size = 30)) 

ggplot(data=filter(slope_out, metric != "Rubisco_Vcmax" & metric != 'Light_Jmax'), aes(y=reorder(metric, order), x=Slope, fill=Treatment)) +
  geom_vline(xintercept=0, linetype="dashed", 
             color = "darkgray", linewidth=1.5) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbarh(aes(xmin=Slope - Slope_se, xmax=Slope + Slope_se, height = 0.5) , position=position_dodge(width = 0.9)) +
  theme_bw() +
  scale_fill_discrete(type = cbPalette_4) +
  labs(title = 'Slope of Tiller Number RR by Trait', y = 'Trait', x = 'Slope') +
  theme(legend.position = 'top') +
    guides(fill = guide_legend(reverse = TRUE)) +
  theme(text = element_text(size = 30)) 


ggplot(data=filter(slope_out, metric == "LDMC" ), aes(x=metric, y=Slope, fill=Treatment)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Slope - Slope_se, ymax=Slope + Slope_se, width = 0.5) , position=position_dodge(width = 0.9)) +
  theme_bw() +
  scale_fill_discrete(breaks=c('Drought', 'Grazing', 'Drought & Grazing'), type = cbPalette_3) +
  labs(title = 'Slope of Tiller Number RR by LDMC', x = 'LDMC', y = 'Slope') +
  theme(legend.position = 'bottom') +
  theme(text = element_text(size = 30)) 

ggplot(data=filter(slope_out, metric == "SLA" ), aes(y=metric, x=Slope, fill=Treatment)) +
  geom_vline(xintercept=0, linetype="dashed", 
             color = "darkgray", linewidth=1.5) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbarh(aes(xmin=Slope - Slope_se,xmax=Slope + Slope_se, height = 0.5) , position=position_dodge(width = 0.9)) +
  theme_bw() +
  xlim(-2,2) +
  scale_fill_discrete(type = cbPalette_4) +
  labs(title = 'Slope of Tiller Number RR by SLA', y = 'SLA', x = 'Slope') +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(text = element_text(size = 30)) 

SLA_density <- ggplot(data=traits_treat, aes(x=SLA_mean, group=species, fill=species)) +
  geom_density(adjust=1.5, alpha=.4) 
SLA_density

#commit testing


# CWT ---------------------------------------------------------------------

non_dom_traits <- read_csv("NExS_Terry_ExternalTraits2025_missingDryWeights.csv")

non_dom_pop_demo <- read_csv("NExS_Terry_ExternalPopDemo2025.csv")

non_dom_full <- non_dom_traits %>% 
  full_join(non_dom_pop_demo) %>% 
  select(!year, block, plant_tag) 

non_dom_species <- non_dom_full %>% 
  select(!(block)) %>% 
  select(!(plant_tag:rep_num)) %>% 
  select(!(leaf_dry_weight_g:notes)) %>% 
  group_by(species) %>% 
  summarise(across(everything(), mean, na.rm=TRUE)) %>% 
  mutate(veg_height = veg_height_cm) %>% 
  mutate(flower_len = flower_length_cm) %>% 
  mutate(leaf_thickness = leaf_thickness_mm) %>% 
  mutate(leaf_wet_mass = leaf_wet_weight_g) %>% 
  mutate(basal_area =basal1 * basal2 * 3.14) %>% 
  mutate(arial_area = aerial1 * aerial2 * 3.14) %>% 
  mutate(arial1 = aerial1) %>% 
  mutate(arial2 = aerial2)

species_traits <- x23_full_data_jan %>% 
  group_by(species) %>% 
  select(!(month)) %>% 
  select(!(Block:"Install Date")) %>% 
  select(!(year:plant_tag)) %>% 
  select(!(height_t1:height_t3)) %>% 
  summarise(across(everything(), mean, na.rm=TRUE)) %>% 
  select(!(repro_height)) %>% 
  select(!(vol_density)) %>% 
  mutate(tiller_diameter = (diameter_t1 + diameter_t2 + diameter_t3/3)) %>% 
  mutate(leaf_thickness = (thick_l1 + thick_l2 + thick_l3/3)) %>% 
  mutate(leaf_dry_mass = (dry_mass_l1 + dry_mass_l2 + dry_mass_l3/3)) %>% 
  mutate(leaf_wet_mass = (wet_mass_l1 + wet_mass_l2 + wet_mass_l3/3)) %>% 
  select(!(diameter_t1:wet_mass_l3)) %>% 
  mutate(basal_area =basal1 * basal2 * 3.14) %>% 
  mutate(arial_area = arial1 * arial2 * 3.14)

full_species_traits <- species_traits %>% 
  full_join(non_dom_species) %>% 
  select(!(veg_height_cm:aerial2))

NExS_WholePlot_2025_full <- read_excel("NExS_SpComp_2025.xlsx") %>%
  filter(Subplot == 'whole') %>%
  select(!(Jan_Inside)) %>% 
  select(!(March_Inside)) %>% 
  select(!(Notes)) %>% 
  mutate(March_Outside = as.numeric(March_Outside)) %>% 
  pivot_longer(!Year:Species, names_to = "Season", values_to = "Abundance") %>% 
  separate(Species,into=c("Sp", "Gn"), 
           sep=" ", convert = TRUE) %>% 
  unite("Species", Sp:Gn, remove = FALSE) %>% 
  mutate( Abundance = (as.numeric(Abundance))) %>% 
  filter(Abundance > 0) %>% 
  group_by(Block, Species) %>% 
  summarize((across(.cols=c(Abundance),
                    .fns=list(mean=mean),
                    na.rm=T))) %>% 
  ungroup() 

ggplot(data = species_traits, aes(x = SLA, y = LDMC, color = species)) +
  geom_point() +
  geom_smooth(method=lm,
              se=FALSE) +
  #facet_grid(drought~grazing) +
  labs(title = 'SLA vs LDMC')

doms_m <- lm(SLA ~ LDMC, data = species_traits)
summary(doms_m)

# ANPP & BNPP -------------------------------------------------------------

#ANPP 
anpp <- read_excel("NExS_ANPP_2025.xlsx") %>% 
  mutate(Total_herb = Grass + Forb) %>% 
  select(!(Notes)) %>% 
  full_join(plot_key) %>% 
  unite(treatment, drought, grazing, Fire, remove = FALSE)

anpp_avg <- anpp %>% 
  group_by(block, plot, treatment, drought, grazing, Fire) %>% 
  summarize((across(.cols=c(Grass, Forb, Woody, Pdead, Total_herb),
                    .fns=list(sum=sum),
                    na.rm=T))) %>% 
  ungroup() %>% 
  drop_na()
  

ggplot(data=anpp_avg, aes(x=treatment, y=Total_herb_sum, fill= treatment)) +
  geom_boxplot() +
  facet_grid(~Fire) +
  theme_bw()

stat_compare_means(method = "anova", label.y = 200, size = 5) +      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "ND_NG", size =5)  

#BNPP
bnpp <- read_excel("NExS_BNPP_2025.xlsx") %>% 
  full_join(plot_key) %>% 
  unite(treatment, drought, grazing, Fire, remove = FALSE)

bnpp_avg <- bnpp %>% 
  group_by(block, plot, treatment, drought, grazing, Fire) %>% 
  summarize((across(.cols=c(LiveRoot_DryMass, SOM_DryMass),
                    .fns=list(mean=mean),
                    na.rm=T))) %>% 
  ungroup() %>% 
  drop_na()

ggplot(data=bnpp_avg, aes(x=treatment, y=LiveRoot_DryMass_mean, fill= treatment)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = 'Treatment') +
  stat_compare_means(method = "anova", label.y = .8, size = 5) +      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "ND_NG_AF", size =5)  

ggplot(data=bnpp_avg, aes(x=drought, y=LiveRoot_DryMass_mean, fill= drought)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = 'drought') +
  stat_compare_means(method = "anova", label.y = .8, size = 5) +      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "ND", size =5)  

ggplot(data=bnpp_avg, aes(x=grazing, y=LiveRoot_DryMass_mean, fill= grazing)) +
  geom_boxplot() +
  #facet_grid(~Fire) +
  theme_bw() +
  labs(title = 'grazing') +
  stat_compare_means(method = "anova", label.y = .8, size = 5) +      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "NG", size =5)  

ggplot(data=bnpp_avg, aes(x=Fire, y=LiveRoot_DryMass_mean, fill= Fire)) +
  geom_boxplot() +
  #facet_grid(~Fire) +
  theme_bw() +
  labs(title = 'Fire') +
  stat_compare_means(method = "anova", label.y = .8, size = 5) +      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "AF", size =5)  

full_npp_avg <- anpp_avg %>% 
  full_join(bnpp_avg)

ggplot(data = full_npp_avg, aes(x = LiveRoot_DryMass_mean, y = Total_herb_sum, color = treatment, group = treatment)) +
  geom_point() +
  geom_smooth(method=lm,
              se=FALSE) +
  facet_grid(drought~grazing)
  
x23_full_species_traits <- read_csv("NExS_trait-data_ALL.csv") %>% 
  mutate(total_area = as.numeric(total_area)) %>% 
  mutate(dry_weight = as.numeric(dry_weight)) %>% 
  mutate(wet_weight = as.numeric(wet_weight)) %>% 
  mutate(SLA = total_area/dry_weight) %>% #SLA
  mutate(LDMC = (dry_weight*1000)/wet_weight) %>% 
  group_by(species_id) %>% 
  summarize((across(.cols=c(SLA, LDMC),
                    .fns=list(mean=mean, sefxn=sefxn),
                    na.rm=T))) %>% 
  ungroup() %>% 
  mutate(Species = dplyr::recode(species_id,
                                   "ari_con" = "Aristida_congesta",
                                   "bot_rad" = "Bothriochloa_radicans",
                                   "cen_cil" = "Cenchrus_ciliare",
                                   "com_car" = "Commicarpus_pilosus",
                                 "ehh_rid" = "Ehritia_rigida",
                                 "era_sup" = "Eragrostis_superba",
                                 "eriosonum" = "Eriosema_sp.",
                                 "uro_mos" = "Urochloa_mosambicensis",
                                 "evo_nut" = "Evolvulous_alsinoide",
                                 "hel_stu" = "Heliotropium_steudneri",
                                 "her_gla" = "Hermannia_glanduligera",
                                 "het_con" = "Heteropogon_contortus",
                                 "ind_vic" = "Indigophera_vicioides",
                                 "oci_mum" = "Omocarpum_tricocarpum",
                                 "pan_col" = "Panicum_coloratum",
                                 "pan_max" = "Panicum_maximum",
                                 "pavonia" = "Pavonia_NA",
                                 "purple_tube" = "Purple_tube",
                                 "sch_pap" = "Schmidtia_pappophoroides",
                                 "tep_pur" = "Tephrosia_purpurea",
                                 "the_tri" = "Themeda_triandra"))

ggplot(data = x23_full_species_traits, aes(x = LDMC_mean, y = SLA_mean, group = species_id, color = species_id)) +
  geom_point() +
  geom_smooth(method=lm,
              se=FALSE) +
  #facet_grid(drought~grazing) +
  labs(title = 'CWT: LDMC x ANPP')

x25_species_comp <- read_excel("NExS_SpComp_2025.xlsx") %>%
  filter(Subplot == 'whole') %>%
  select(!(Jan_Inside)) %>% 
  select(!(March_Inside)) %>% 
  select(!(Notes)) %>% 
  mutate(March_Outside = as.numeric(March_Outside)) %>% 
  pivot_longer(!Year:Species, names_to = "Season", values_to = "Abundance") %>% 
  separate(Species,into=c("Sp", "Gn"), 
           sep=" ", convert = TRUE) %>% 
  unite("Species", Sp:Gn, remove = FALSE) %>% 
  mutate( Abundance = (as.numeric(Abundance))) %>% 
  filter(Abundance > 0) %>% 
  select(!(Season)) %>% 
  group_by(Block, Plot, Species) %>% 
  filter(Abundance == max(Abundance)) %>% 
  ungroup() %>% 
  distinct() 

CWT <- x25_species_comp %>% 
  mutate(Percent = (Abundance/100)) %>% 
  full_join(x23_full_species_traits) %>% 
  drop_na() %>% 
  mutate(SLA_CWT = (Percent * SLA_mean)) %>% 
  mutate(LDMC_CWT = (Percent * LDMC_mean)) %>% 
  group_by(Block, Plot) %>% 
  dplyr::summarize((dplyr::across(.cols=c(SLA_CWT, LDMC_CWT, Percent),
                                  .fns=list(sum=sum),
                                  na.rm=T))) %>% 
  ungroup() %>% 
  mutate(block = Block) %>% 
  mutate(plot = Plot) 

npp_cwt <- full_npp_avg %>% 
  full_join(CWT) %>% 
  mutate(Fire = as.factor(Fire)) %>% 
  mutate(drought = as.factor(drought)) %>% 
  mutate(grazing = as.factor(grazing)) %>% 
  select(!(Block:Plot)) %>% 
  mutate(plot_mod = dplyr::recode(plot,
                                  '1' = '1' ,
                                  '2' = '2',
                                  '3' = '3' ,
                                  '4' = '4',
                                  '5' = '5', 
                                  '6' = '6' , 
                                  '7' = '7', 
                                  '8' = '8' ,
                                  '9' = '1',
                                 '10' ='2', 
                                 '11' = '3',
                                 '12' = '4',
                                 '13' = '5', 
                                 '14' = '6',
                                 '15' = '7',
                                 '16' = '8',
                                 '17' = '1' ,
                                 '18' = '2' ,
                                 '19' = '3' ,
                                 '20' = '4',
                                 '21' = '5' ,
                                 '22' = '6' ,
                                 '23' = '7' ,
                                 '24' = '8' ,
                                 '25'= '1' ,
                                 '26' = '2' ,
                                 '27' = '3' ,
                                 '28'= '4',
                                 '29' = '5' ,
                                 '30'= '6' ,
                                 '31'= '7' ,
                                 '32' = '8' ,
                                 '33' = '1' ,
                                 '34' = '2' ,
                                 '35' = '3' ,
                                 '36' = '4',
                                 '37' = '5' ,
                                 '38' = '6' ,
                                 '39' = '7' ,
                                 '40' = '8' ,
                                 '41' = '1' ,
                                 '42' = '2' ,
                                 '43' = '3' ,
                                 '44' = '4' ,
                                 '45' = '5' ,
                                 '46' = '6' ,
                                 '47' = '7' ,
                                 '48' = '8' )) 

ggplot(data=npp_cwt, aes(x=Percent_sum, group=block, fill=block)) +
  geom_density(adjust=1.5, alpha = 0.25) +
  theme_bw()

hist(npp_cwt$Percent_sum)

ggplot(data = npp_cwt, aes(x = SLA_CWT_sum, y = Total_herb_sum)) +
  geom_point() +
  geom_smooth(method=lm,
              se=FALSE) +
  #facet_grid(drought~grazing) +
  labs(title = 'CWT: SLA x ANPP')

ggplot(data = npp_cwt, aes(x = LDMC_CWT_sum, y = Total_herb_sum)) +
  geom_point() +
  geom_smooth(method=lm,
              se=FALSE) +
  #facet_grid(drought~grazing) +
  labs(title = 'CWT: LDMC x ANPP')

ggplot(data = npp_cwt, aes(x = SLA_CWT_sum, y = LiveRoot_DryMass_mean)) +
  geom_point() +
  geom_smooth(method=lm,
              se=FALSE) +
  #facet_grid(drought~grazing) +
  labs(title = 'CWT: SLA x BNPP')


ggplot(data = npp_cwt, aes(x = LDMC_CWT_sum, y = LiveRoot_DryMass_mean)) +
  geom_point() +
  geom_smooth(method=lm,
              se=FALSE) +
  labs(title = 'CWT: LDMC x BNPP')

m_sla_anpp <- lm(Total_herb_sum ~ SLA_CWT_sum, data = npp_cwt)
summary(m_sla_anpp)

ggplot(data = npp_cwt, aes(x = SLA_CWT_sum, y = Total_herb_sum)) +
  geom_point() +
  geom_smooth(method=lm,
              se=FALSE) +
  #facet_grid(drought~grazing) +
  labs(title = 'CWT: SLA x ANPP')

m_LDMC_anpp <- lm(Total_herb_sum ~ LDMC_CWT_sum, data = npp_cwt,)
summary(m_LDMC_anpp)

ggplot(data = npp_cwt, aes(x = LDMC_CWT_sum, y = Total_herb_sum, color = treatment, group = treatment)) +
  geom_point() +
  geom_smooth(method=lm,
              se=FALSE) +
  #facet_grid(drought~grazing) +
  labs(title = 'CWT: LDMC x ANPP')

m_sla_bnpp <- lm(LiveRoot_DryMass_mean ~ SLA_CWT_sum, data = npp_cwt)
summary(m_sla_bnpp)

m_LDMC_bnpp <- lm(LiveRoot_DryMass_mean ~ LDMC_CWT_sum, data = npp_cwt)
summary(m_LDMC_bnpp)

m_LDMC_SLA <- lm(SLA_CWT_sum ~ LDMC_CWT_sum, data = npp_cwt)
summary(m_LDMC_SLA)

ggplot(data=npp_cwt, aes(x=treatment, y=SLA_CWT_sum, fill= treatment)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = 'Treatment') +
  stat_compare_means(method = "anova", label.y = 100, size = 10) +      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "ND_NG_AF", size =10) 

ggplot(data=npp_cwt, aes(x=drought, y=SLA_CWT_sum, fill= drought)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = 'drought') +
  stat_compare_means(method = "anova", label.y = 100, size = 10) +      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "ND", size =10) 

ggplot(data=npp_cwt, aes(x=grazing, y=SLA_CWT_sum, fill= grazing)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = 'grazing') +
  stat_compare_means(method = "anova", label.y = .8, size = 5) +      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "NG", size =5) 

ggplot(data=npp_cwt, aes(x=Fire, y=SLA_CWT_sum, fill= Fire)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = 'fire') +
  stat_compare_means(method = "anova", label.y = .8, size = 5) +      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "AF", size =5) 

#Stats and graphs for presentation

#anpp
hist(log(npp_cwt$Total_herb_sum))
shapiro.test(log(npp_cwt$Total_herb_sum))

anpp_aov <- aov(log(Total_herb_sum) ~ drought * grazing, 
               data = npp_cwt)
print(anpp_aov)
summary(anpp_aov)

tukey.test <- TukeyHSD(anpp_aov)
tukey.test

anpp_mod <- lme(log(Total_herb_sum) ~ (Fire + drought + grazing)^2
                         , data= npp_cwt
                         , random = ~1 |block
                         , correlation=corCompSymm(form = ~1 |block)
                         , control=lmeControl(returnObject=TRUE)
                         , na.action = na.omit)


anova(anpp_mod, type="marginal")
summary(anpp_mod)

plot(anpp_mod, type=c("p","smooth"), col.line=1)
qqnorm(anpp_mod, abline = c(0,1)) ## qqplot

ggplot(data=npp_cwt, aes(x=Fire, y=log(Total_herb_sum), fill= Fire)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = 'ANPP x Fire') +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title=element_text(size=15)) +
  stat_compare_means(method = "anova", label.y = 2.5, size = 10) +      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "ND_NG_AF", size =10) 

#bnpp
hist((npp_cwt$LiveRoot_DryMass_mean))
shapiro.test((npp_cwt$LiveRoot_DryMass_mean))

bnpp_mod <- lme(LiveRoot_DryMass_mean ~ (Fire + drought + grazing)^2
                , data= npp_cwt
                , random = ~1 |block
                , correlation=corCompSymm(form = ~1 |block)
                , control=lmeControl(returnObject=TRUE)
                , na.action = na.omit)


anova(bnpp_mod, type="marginal")
summary(bnpp_mod)

plot(anpp_mod, type=c("p","smooth"), col.line=1)
qqnorm(anpp_mod, abline = c(0,1)) ## qqplot

ggplot(data=npp_cwt, aes(x=treatment, y=LiveRoot_DryMass_mean, fill= treatment)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = 'BNPP x Treatment') +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title=element_text(size=15)) +
  stat_compare_means(method = "anova", label.y = 0, size = 10) +      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "ND_NG_AF", size =10)

#SLA
hist((npp_cwt$SLA_CWT_sum))
shapiro.test((npp_cwt$SLA_CWT_sum))



sla_mod <- lme(SLA_CWT_sum ~ (Fire + drought + grazing)^2
                , data= npp_cwt
                , random = ~1 |block
                , correlation=corCompSymm(form = ~1 |block)
                , control=lmeControl(returnObject=TRUE)
                , na.action = na.omit)


anova(sla_mod, type="marginal")
summary(sla_mod)

plot(sla_mod, type=c("p","smooth"), col.line=1)
qqnorm(sla_mod, abline = c(0,1)) ## qqplot

hist((npp_cwt$LDMC_CWT_sum))
shapiro.test((npp_cwt$LDMC_CWT_sum))

ggplot(data=npp_cwt, aes(x=drought, y=SLA_CWT_sum, fill= drought)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = 'drought') +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title=element_text(size=15)) +
  stat_compare_means(method = "anova", label.y = 0, size = 10) +      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "ND", size =10) 

LDMC_mod <- lme(LDMC_CWT_sum ~ LiveRoot_DryMass_mean * treatment
               , data= npp_cwt
               , random = ~1 |block
               , correlation=corCompSymm(form = ~1 |block)
               , control=lmeControl(returnObject=TRUE)
               , na.action = na.omit)


anova(LDMC_mod, type="marginal")
summary(LDMC_mod)

plot(LDMC_mod, type=c("p","smooth"), col.line=1)
qqnorm(LDMC_mod, abline = c(0,1)) ## qqplot

ggplot(data=npp_cwt, aes(x=treatment, y=LDMC_CWT_sum, fill= treatment)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = 'Treatment') +
  
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title=element_text(size=15)) +
  stat_compare_means(method = "anova", label.y = 0, size = 10) +      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "ND_NG_AF", size =10) 

ggplot(data = npp_cwt, aes(x = SLA_CWT_sum, y = LDMC_CWT_sum)) +
  geom_point() +
  geom_smooth(method=lm,
              se=FALSE) +
  #facet_grid(drought~grazing) +
  labs(title = 'CWT: SLA x LDMC')


# Simper ------------------------------------------------------------------

x25_sc_treatments <- x25_species_comp %>% 
  full_join(NExS_plot_key) %>% 
  unite(treatment, Drought, Grazing, Fire, remove = FALSE) %>% 
  mutate(treatment = as.factor(treatment)) %>% 
  pivot_wider(names_from = Species, values_from = Abundance)

x25_sc <- x25_sc_treatments %>% 
  subset(select = 18:81) %>% 
  replace(is.na(.), 0) 

x25_env <- x25_sc_treatments %>% 
  subset(select= c(block, plot, treatment, Drought, Grazing, Fire))

(sim <- with(x25_env, simper(x25_sc, Fire)))
summary(sim)

