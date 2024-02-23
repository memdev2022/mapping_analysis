rm(list=ls())
{
  library(rstudioapi)
  library(data.table)
  library(readxl)
  library(reshape2)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(plotly)
  library(ggalt)
  library(ggbump)
}

directory = dirname(getSourceEditorContext()$path) %>% str_extract(pattern = "^(.*?)mapping_analysis")
{
  data_path = file.path(directory,'00_Data xlsx/')
  funcon_path = file.path(directory,'03_Best-script/00_FunCon/')
  preprocessing_path = file.path(directory,'03_Best-script/00_Data-Preprocessing/')
  script_path = file.path(directory,'03_Best-script/05-Interference/')
  result_path = file.path(directory,'04_Results/')
  descriptives_path = file.path(directory,'05_Descriptives/')
}


# Cosmetics
{source(file.path(funcon_path, "PN_GetPalette.R"))}
alpha <- c(1,.995,.99,.985,.98,.975)

# PREPROCESSING ----
# Select variables of interest
# Read data set

##Add: only PS == 1
Entry_ID_PS1 <- read_excel(file.path(data_path,"Lit_Review_QC.xlsx"))%>%
  filter(PS==1 |Article_ID == 'Sommer_2021') %>%
  mutate(Entry_ID = paste0(Article_ID,", ", Task_number)) %>% pull(Entry_ID)

select.these = c('Canada_2018, Task_1', 'Bouyeure_2021, Task_1', 'Hassevoort_2020, Task_3', 'Keresztes_2017, Task_1',
                 'Ngo_2018, Task_1', 'Ngo_2019b, Task_1', 'Ngo_2019c, Task_1', 'Ngo_2021, Task_4',
                 'Rollins_2018, Task_1', 'Ngo_2019b, Task_2', 'Ribordy_2015, Task_1', 'Sommer_2021, Task_1',
                 'Vlach_2016, Task_1','Fandakova_2019, Task_3','Jack_2012, Task_2')

reviewtable <- read_excel(file.path(data_path,"Lit_Review_Preprocessed.xlsx"))%>%
  select(Entry_ID, Design, Task_type, `To-be-remembered_information`, starts_with("Similarity_manipulation_at")) %>% 
  filter(Design %in% c(1,2) & Task_type %in% c(1,2))%>%
  filter(Entry_ID %in% Entry_ID_PS1)

# Recode all similarity manipuation = 1 to NA
reviewtable <- reviewtable %>%
  rename(c(To_be_remembered_information = `To-be-remembered_information`,
           Similarity_manipulation_1 = `Similarity_manipulation_at_encoding_1`,
           Similarity_manipulation_2 = `Similarity_manipulation_at_encoding_2`,
           Similarity_manipulation_3 = `Similarity_manipulation_at_retrieval_1`,
           Similarity_manipulation_4 = `Similarity_manipulation_at_retrieval_2`)) %>%
  mutate_at(vars(starts_with('Similarity_manipulation')), list(~recode(., `1` = -999)))


# Define indentifier variables
identifier_var <- c("Entry_ID")

# Define NAs
reviewtable[reviewtable==-999] <- NA

# Sort similarity for each entry: 1>>2>>3>>...
# reviewtable1, Sort stores sim manipualtions as a sorted list
reviewtable1 <- reviewtable %>% rowwise() %>%
  mutate(Sort = list(c(Similarity_manipulation_1,Similarity_manipulation_2,Similarity_manipulation_3,Similarity_manipulation_4) %>% unique() %>% sort()))%>%
  filter(length(Sort)>0) %>% # only get rows that contain similarity manipulation
  rowwise()%>% mutate(Sort = toString(Sort)) %>%
  select(Entry_ID, Design, Task_type, To_be_remembered_information, Sort)
# Check how many sim manipulations an entry could have maximum
reviewtable1$Sort %>% unique() # 25.12.21: two manipulations maximum

reviewtable1 <- reviewtable %>% rowwise() %>%
  # Sort stores sim manipualtions as a sorted list
  mutate(Sort = list(c(Similarity_manipulation_1, Similarity_manipulation_2, Similarity_manipulation_3, Similarity_manipulation_4) %>% unique() %>% sort()))%>%
  # only get rows that contain similarity manipulation
  filter(length(Sort)>0) %>%
  # Pull out sim manipulations in order
  mutate(Similarity1 = Sort[1], Similarity2 = Sort[2], Similarity3 = Sort[3], Similarity4 = Sort[4]) %>%
  # Get cleaned data
  select(Entry_ID, Design, Task_type, To_be_remembered_information, Similarity1, Similarity2, Similarity3, Similarity4) %>%
  select_if(~sum(!is.na(.)) > 0)

reviewtable1$Design <- factor(reviewtable1$Design, levels = c(1,2))
reviewtable1$Task_type <- factor(reviewtable1$Task_type, levels = c(1,2))
reviewtable1$To_be_remembered_information <- factor(reviewtable1$To_be_remembered_information, levels = c(1:6))

nEntry = nrow(reviewtable1)


# LAYER I: Design & TT----

## Plot connection ----
### Design
dfDes <- reviewtable1 %>%
  arrange(Design, Task_type)%>%
  select(Entry_ID, Design)
dfDes$des <- seq(1:nrow(dfDes))
dfDes <- dfDes %>% mutate(DES = case_when(Design == 1 ~ des -5, 
                                          Design == 2 ~ des +5))

ggplot(dfDes)+ geom_point(aes(y=1, x=DES, color=Design), show.legend = F)

### TT
dfTT <- reviewtable1 %>%
  arrange(Task_type, Design)%>%
  select(Entry_ID, Task_type) 
dfTT$tt <- seq(1:nrow(dfTT))
dfTT <- dfTT %>% mutate(TT = case_when(Task_type == 1 ~ tt -5,
                                       Task_type == 2 ~ tt +5))

ggplot(dfTT)+ geom_point(aes(y=1, x=TT, color=Task_type), show.legend = F)

# combine plots
df1 <- merge(dfDes, dfTT, by = "Entry_ID") %>% arrange(des,tt)
ggplot() + geom_segment(data=df1, aes(x = DES, y = 1, xend = TT, yend = 2), alpha = 0.09, show.legend = F)


## Plot bar ----
### Design
dfSumDes <- df1 %>%
  group_by(Design) %>%
  summarise(maxDES = max(DES), minDES = min(DES), sumDES = n(), medianDES = median(DES))%>%
  arrange(desc(sumDES))
Design <- unique(dfSumDes$Design)
dfSumDes <- dfSumDes %>%
  mutate(alphaDES = case_when(Design %in% Design[1] ~ alpha[1],
                             Design %in% Design[2] ~ alpha[2]) %>% as.factor())

### TT
dfSumTT <- df1 %>%
  group_by(Task_type)%>%
  summarise(maxTT = max(TT), minTT = min(TT), sumTT = n(), medianTT = median(TT)) %>%
  arrange(desc(sumTT))
Task_type <- unique(dfSumTT$Task_type)
dfSumTT <- dfSumTT %>%  
  mutate(alphaTT = case_when(Task_type %in% Task_type[1] ~ alpha[1],
                             Task_type %in% Task_type[2] ~ alpha[2]) %>% as.factor())

# combine plots
ggplot()+
  geom_sigmoid(data=df1, aes(x = DES, xend = TT, y = 0, yend = 4, group = Entry_ID), 
               alpha = 1, smooth = 3.5, color = '#CFD8DC', size = .8, show.legend = F)+
  geom_segment(data = dfSumDes, aes(x = minDES, xend = maxDES, y = 0, yend = 0),
               size = 4.5, show.legend = F, color = projectPalette[2])+
  geom_segment(data = dfSumTT, aes(x = minTT, xend = maxTT, y = 4, yend = 4),
               size = 4.5, show.legend = F, color = projectPalette[2])



# LAYER II: TT &  Tbr ----
## Plot connection ----
### TT
dfTT <- reviewtable1 %>%
  arrange(Task_type, To_be_remembered_information)%>%
  select(Entry_ID, Task_type) 
dfTT$tt <- seq(1:nrow(dfTT))
dfTT <- dfTT %>% mutate(TT = case_when(Task_type == 1 ~ tt -5,
                                       Task_type == 2 ~ tt +5))

ggplot(dfTT)+ geom_point(aes(y=1, x=TT, color=Task_type), show.legend = F)

# Tbr
dfTbr <- reviewtable1 %>%
  arrange(To_be_remembered_information, Task_type) %>%
  select(Entry_ID, To_be_remembered_information)
dfTbr$tbr <- seq(1:nrow(dfTbr))
dfTbr <- dfTbr %>%
  mutate(Tbr = case_when(To_be_remembered_information == 1 ~ tbr - 50,
                         To_be_remembered_information == 2 ~ tbr - 25,
                         To_be_remembered_information == 3 ~ tbr - 5,
                         To_be_remembered_information == 4 ~ tbr + 5,
                         To_be_remembered_information == 5 ~ tbr + 25,
                         To_be_remembered_information == 6 ~ tbr + 50))


ggplot(dfTbr) + geom_point(aes(y=2, x=Tbr, color=To_be_remembered_information), show.legend = F)

# plot connectionsonnec
df2 <- merge(dfTT, dfTbr, by = "Entry_ID") %>% arrange(tt,tbr)

ggplot() + geom_segment(data=df2, aes(x = TT, y = 1, xend = Tbr, yend = 2), alpha = 0.09, show.legend = F)


## Plot bar ----

### Task type
dfSumTT <- df2 %>%
  group_by(Task_type)%>%
  summarise(maxTT = max(TT), minTT = min(TT), sumTT = n(), medianTT = median(TT),
            maxTbr = max(Tbr), minTbr = min(Tbr)) %>%
  arrange(desc(sumTT))
Task_type <- unique(dfSumTT$Task_type)
dfSumTT <- dfSumTT %>%  
  mutate(alphaTT = case_when(Task_type %in% Task_type[1] ~ alpha[1],
                             Task_type %in% Task_type[2] ~ alpha[2]) %>% as.factor())

### Tbr
dfSumTbr <- df2 %>%
  group_by(To_be_remembered_information)%>%
  summarise(maxTT = max(TT), minTT = min(TT), medianTbr = median(Tbr),
            maxTbr = max(Tbr), minTbr = min(Tbr), sumTbr = n()) %>%
  arrange(desc(sumTbr))
To_be_remembered_information <- unique(dfSumTbr$To_be_remembered_information)
dfSumTbr <- dfSumTbr %>%
  mutate(alphaTbr = case_when(To_be_remembered_information %in% To_be_remembered_information[1] ~ alpha[1],
                              To_be_remembered_information %in% To_be_remembered_information[2] ~ alpha[2],
                              To_be_remembered_information %in% To_be_remembered_information[3] ~ alpha[3],
                              To_be_remembered_information %in% To_be_remembered_information[4] ~ alpha[4],
                              To_be_remembered_information %in% To_be_remembered_information[5] ~ alpha[5],
                              To_be_remembered_information %in% To_be_remembered_information[6] ~ alpha[6]) %>% as.factor())

# Combine plot
ggplot()+
  geom_sigmoid(data=df1, aes(x = DES, xend = TT, y = 0, yend = 4, group = Entry_ID), 
               alpha = 1, smooth = 3.5, color = '#CFD8DC', size = .8, show.legend = F)+
  geom_sigmoid(data=df2, aes(x = TT, xend = Tbr, y = 4, yend = 8, group = Entry_ID),
               alpha = 1, smooth = 3.5, color = '#CFD8DC', size = .8, show.legend = F)+
  geom_segment(data = dfSumDes, aes(x = minDES, xend = maxDES, y = 0, yend = 0),
               size = 4.5, show.legend = F, color = projectPalette[2])+
  geom_segment(data = dfSumTT, aes(x = minTT, xend = maxTT, y = 4, yend = 4),
               size = 4.5, show.legend = F, color = projectPalette[2])+
  geom_segment(data = dfSumTbr, aes(x = minTbr, xend = maxTbr, y = 8, yend = 8),
               size = 4.5, show.legend = F, color = projectPalette[2])



# LAYER III: Tbr & Cue ----
## Plot connection ----
### Sim
dfS <- reviewtable1 %>% 
  select(Entry_ID, To_be_remembered_information, Similarity1, Similarity2)
dfS <- melt(dfS, id.vars = c("Entry_ID", "To_be_remembered_information"), value.name = 'Similarity')%>%
  filter(!is.na(Similarity))
Entry_ID.dup <- dfS$Entry_ID[duplicated(dfS$Entry_ID)]


# Special case for multiple sim manipulations per entry
# Version 1: ignore ordering by multiple manipulations, focus on real order
# order: Tbr >> Similarity
dfS <- reviewtable1 %>%
  select(Entry_ID, To_be_remembered_information, Similarity1, Similarity2)%>%
  arrange(To_be_remembered_information, Similarity1, Similarity2)
dfS$tbr <- seq(1:nrow(dfS))

dfS <- melt(dfS, id.vars = c("Entry_ID", "To_be_remembered_information", "tbr"),
            value.name = 'Similarity')%>%
  filter(!is.na(Similarity)) %>%
  arrange(To_be_remembered_information, Similarity)

dfS <- dfS %>%
  arrange(Similarity, tbr)
dfS$sim <- seq(1:nrow(dfS))

df3 <- dfS %>%
  mutate(Sim = case_when(Similarity == 2 ~ sim - 30,
                         Similarity == 3 ~ sim - 15,
                         Similarity == 4 ~ sim + 0,
                         Similarity == 5 ~ sim + 15,
                         Similarity == 6 ~ sim + 30))%>%
  mutate(Tbr = case_when(To_be_remembered_information == 1 ~ tbr - 50,
                         To_be_remembered_information == 2 ~ tbr - 25,
                         To_be_remembered_information == 3 ~ tbr - 5,
                         To_be_remembered_information == 4 ~ tbr + 5,
                         To_be_remembered_information == 5 ~ tbr + 25,
                         To_be_remembered_information == 6 ~ tbr + 50)) %>%
  mutate(Entry_ID = paste(Entry_ID, Similarity, sep = '_')) %>%
  arrange(Sim, Tbr)

df3$Similarity <- factor(df3$Similarity, levels = c(2:6))

ggplot() + geom_segment(data=df3, aes(x = Tbr, y = 1, xend = Sim, yend = 2),
                        alpha = 0.59, show.legend = F)


## Plot bars ----

dfSumSim <- df3 %>%
  group_by(Similarity)%>%
  summarise(maxTbr = max(Tbr), minTbr = min(Tbr), medianSim = median(Sim),
            maxSim = max(Sim), minSim = min(Sim), sumSim = n()) %>%
  arrange(desc(sumSim))
Similarity <- unique(dfSumSim$Similarity)
dfSumSim <- dfSumSim %>%
  mutate(alphaSim = case_when(Similarity %in% Similarity[1] ~ alpha[1],
                              Similarity %in% Similarity[2] ~ alpha[2],
                              Similarity %in% Similarity[3] ~ alpha[3],
                              Similarity %in% Similarity[4] ~ alpha[4],
                              Similarity %in% Similarity[5] ~ alpha[5],
                              Similarity %in% Similarity[6] ~ alpha[6]) %>% as.factor())


# COMPILE ----
p <- ggplot()+
  # geom_sigmoid(data=df1, aes(x = DES, xend = TT, y = 0, yend = 4, group = Entry_ID), 
  #              alpha = 1, smooth = 3.5, color = '#CFD8DC', size = .8, show.legend = F)+
  # geom_sigmoid(data=df2, aes(x = TT, xend = Tbr, y = 4, yend = 8, group = Entry_ID),
  #              alpha = 1, smooth = 3.5, color = '#CFD8DC', size = .8, show.legend = F)+
  # geom_sigmoid(data=df3, aes(x = Tbr, xend = Sim, y = 8, yend = 12, group = Entry_ID),
  #              alpha = 1, smooth = 3.5, color = '#CFD8DC', size = .8, show.legend = F)+
  geom_sigmoid(data=df1 %>% filter(Entry_ID %in% select.these), aes(x = DES, xend = TT, y = 0, yend = 4, group = Entry_ID),
               alpha = 1, smooth = 3.5, color = '#CFD8DC', size = .8, show.legend = F)+
  geom_sigmoid(data=df2 %>% filter(Entry_ID %in% select.these), aes(x = TT, xend = Tbr, y = 4, yend = 8, group = Entry_ID),
               alpha = 1, smooth = 3.5, color = '#CFD8DC', size = .8, show.legend = F)+
  geom_sigmoid(data=df3 %>% filter(str_detect(Entry_ID, paste(select.these, collapse="|"))), aes(x = Tbr, xend = Sim, y = 8, yend = 12, group = Entry_ID),
               alpha = 1, smooth = 3.5, color = '#CFD8DC', size = .8, show.legend = F)+
  geom_segment(data = dfSumDes, aes(x = minDES, xend = maxDES, y = 0, yend = 0),
               size = 4.5, alpha=.7, show.legend = F, color = nodePalette[2])+
  geom_segment(data = dfSumTT, aes(x = minTT, xend = maxTT, y = 4, yend = 4),
               size = 4.5, alpha=.7, show.legend = F, color = nodePalette[3])+
  geom_segment(data = dfSumTbr, aes(x = minTbr, xend = maxTbr, y = 8, yend = 8),
               size = 4.5, alpha=.7, show.legend = F, color = nodePalette[4])+
  geom_segment(data = dfSumSim, aes(x = minSim, xend = maxSim, y = 12, yend = 12),
               size = 4.5, alpha=.7, show.legend = F, color = nodePalette[9])+
  geom_segment(data = dfSumSim, aes(x = minSim, xend = maxSim, y = 12, yend = 12),
               size = 2.25, alpha=.7, show.legend = F, color = nodePalette[8])+
  # geom_label(data = dfSumDes, aes(label = Design, x = medianDES, y = 0), alpha = 0, label.size = NA, fill = nodePalette[1], show.legend = F)+
  # geom_label(data = dfSumTT, aes(label = Task_type, x = medianTT, y = 4), alpha = 0, label.size = NA, fill = nodePalette[2], show.legend = F)+
  # geom_label(data = dfSumTbr, aes(label = To_be_remembered_information, x = medianTbr, y = 8), alpha = 0, label.size = NA, fill = nodePalette[3], show.legend = F)+
  # geom_label(data = dfSumSim, aes(label = Similarity, x = medianSim, y = 12), alpha = 0, label.size = NA, fill = nodePalette[4], show.legend = F)+
  scale_x_continuous(limits = c(-85, NA))+
  geom_text(aes(label = 'Design', x = -75, y = 0), size = 5)+
  geom_text(aes(label = 'Task type', x = -75, y = 4), size = 5)+
  geom_text(aes(label = 'To-be-remembered \n information', x = -75 , y = 8), size = 5)+
  geom_text(aes(label = 'Simularity manipulation', x = -75, y = 12), size = 5)+
  theme(
    text = element_text(family = "Pristina"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.key = element_rect(fill = "transparent", colour = NA) # get rid of key legend fill, and of the surrounding
  )
p

ggsave(filename = file.path(result_path,"Interference_Similarity_PC1_15entries.pdf"), p, width = 1228*.8, height = 634*.8, units = "px", dpi = "screen")
message('Create ', file.path(result_path, "Interference_Similarity.pdf"))



# Exploration
# 011223: Zoe asks: how many papers are there in the 131 entries plot?
reviewtable$Entry_ID %>% str_extract('.+(?=,)') %>% unique()

tabletoextract <- read_excel(file.path(data_path,"Lit_Review_QC.xlsx"))%>%
  filter(paste0(Article_ID,', ',Task_number) %in% reviewtable$Entry_ID)

