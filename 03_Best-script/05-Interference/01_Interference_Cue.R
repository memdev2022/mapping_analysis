rm(list = ls())
# https://www.r-graph-gallery.com/322-custom-colours-in-sankey-diagram.html
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
reviewtable <- read_excel(file.path(data_path,"Lit_Review_Preprocessed.xlsx"))%>%
  select(Entry_ID, Design, Task_type, `To-be-remembered_information`, starts_with("Cue_manipulation")) %>% 
  filter(Design %in% c(1,2) & Task_type %in% c(1,2))
# Define indentifier variables
identifier_var <- c("Entry_ID")

# Recode Cue_manipulation_[...]
# Avoid Cue_manipulation = 1
# idea: if manipulation1 is 1, then manipulation2 is -999.
# else, while manipulation1 is >1, if manipulation2 is 1, than manipulation2 is -999:

reviewtable <- reviewtable %>%
  mutate(Cue_manipulation_2 = case_when(Cue_manipulation_1 == 1 ~ -999,
                                        Cue_manipulation_1 != 1 & Cue_manipulation_2 == 1 ~ -999,
                                        Cue_manipulation_1 != 1 & Cue_manipulation_2 != 1 ~ Cue_manipulation_2))
# Recode all similarity manipuation = 1 to NA
reviewtable <- reviewtable %>%
  mutate_at(vars(starts_with('Cue_manipulation')), funs(recode(., `1` = -999)))
# Define NAs
reviewtable[reviewtable==-999] <- NA

# Sort cue manipulation for each entry: 1>>2>>3>>...
# reviewtable1, Sort stores cue manipualtions as a sorted list
reviewtable1 <- reviewtable %>% rowwise() %>%
  mutate(Sort = list(c(Cue_manipulation_1,Cue_manipulation_2) %>% unique() %>% sort()))%>%
  filter(length(Sort)>0) %>% # only get rows that contain Cue manipulation
  rowwise()%>% mutate(Sort = toString(Sort)) %>%
  select(Entry_ID, Design, Task_type, `To-be-remembered_information`, Sort)
# Check how many cue manipulations an entry could have maximum
reviewtable1$Sort %>% unique() # 25.12.21: two manipulations maximum

reviewtable1 <- reviewtable %>% rowwise() %>%
  # Sort stores cue manipualtions as a sorted list
  mutate(Sort = list(c(Cue_manipulation_1, Cue_manipulation_2) %>% unique() %>% sort()))%>%
  # only get rows that contain Cue manipulation
  filter(length(Sort)>0) %>%
  # Pull out cue manipulations in order
  mutate(Cue1 = Sort[1], Cue2 = Sort[2]) %>%
  # Get cleaned data
  select(Entry_ID, Design, Task_type, `To-be-remembered_information`, Cue1, Cue2) %>%
  select_if(~sum(!is.na(.)) > 0)

reviewtable1$Design <- factor(reviewtable1$Design, levels = c(1,2))
reviewtable1$Task_type <- factor(reviewtable1$Task_type, levels = c(1,2))
reviewtable1$`To-be-remembered_information` <- factor(reviewtable1$`To-be-remembered_information`, levels = c(1:6))

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

### Task type (TT)
dfTT <- reviewtable1 %>%
  arrange(Task_type, Design)%>%
  select(Entry_ID, Task_type) 
dfTT$tt <- seq(1:nrow(dfTT))
dfTT <- dfTT %>% mutate(TT = case_when(Task_type == 1 ~ tt -5,
                                       Task_type == 2 ~ tt +5))
ggplot(dfTT)+ geom_point(aes(y=1, x=TT, color=Task_type), show.legend = F)

# combine plots:
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
  arrange(Task_type, `To-be-remembered_information`)%>%
  select(Entry_ID, Task_type) 
dfTT$tt <- seq(1:nrow(dfTT))
dfTT <- dfTT %>% mutate(TT = case_when(Task_type == 1 ~ tt -5,
                                       Task_type == 2 ~ tt +5))
ggplot(dfTT)+ geom_point(aes(y=1, x=TT, color=Task_type), show.legend = F)

### Tbr
dfTbr <- reviewtable1 %>%
  arrange(`To-be-remembered_information`, Task_type) %>%
  select(Entry_ID, `To-be-remembered_information`)
dfTbr$tbr <- seq(1:nrow(dfTbr))
dfTbr <- dfTbr %>%
  mutate(Tbr = case_when(`To-be-remembered_information` == 1 ~ tbr - 35,
                         `To-be-remembered_information` == 2 ~ tbr - 15,
                         `To-be-remembered_information` == 3 ~ tbr - 5,
                         `To-be-remembered_information` == 4 ~ tbr + 5,
                         `To-be-remembered_information` == 5 ~ tbr + 15,
                         `To-be-remembered_information` == 6 ~ tbr + 35))
ggplot(dfTbr) + geom_point(aes(y=2, x=Tbr, color=`To-be-remembered_information`), show.legend = F)

# Plot connections
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
  group_by(`To-be-remembered_information`)%>%
  summarise(maxTT = max(TT), minTT = min(TT), medianTbr = median(Tbr),
            maxTbr = max(Tbr), minTbr = min(Tbr), sumTbr = n()) %>%
  arrange(desc(sumTbr))
`To-be-remembered_information` <- unique(dfSumTbr$`To-be-remembered_information`)
dfSumTbr <- dfSumTbr %>%
  mutate(alphaTbr = case_when(`To-be-remembered_information` %in% `To-be-remembered_information`[1] ~ alpha[1],
                              `To-be-remembered_information` %in% `To-be-remembered_information`[2] ~ alpha[2],
                              `To-be-remembered_information` %in% `To-be-remembered_information`[3] ~ alpha[3],
                              `To-be-remembered_information` %in% `To-be-remembered_information`[4] ~ alpha[4],
                              `To-be-remembered_information` %in% `To-be-remembered_information`[5] ~ alpha[5],
                              `To-be-remembered_information` %in% `To-be-remembered_information`[6] ~ alpha[6]) %>% as.factor())


# combine plots
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
### CUE
dfS <- reviewtable1 %>% 
  select(Entry_ID, `To-be-remembered_information`, Cue1, Cue2)
dfS <- melt(dfS, id.vars = c("Entry_ID", "To-be-remembered_information"), value.name = 'Cue')%>%
  filter(!is.na(Cue))
Entry_ID.dup <- dfS$Entry_ID[duplicated(dfS$Entry_ID)]


# Special case for multiple cue manipulations per entry
# Version 1: ignore ordering by multiple manipulations, focus on real order
# order: Tbr >> Cue
dfS <- reviewtable1 %>%
  select(Entry_ID, `To-be-remembered_information`, Cue1, Cue2)%>%
  arrange(`To-be-remembered_information`, Cue1, Cue2)
dfS$tbr <- seq(1:nrow(dfS))

dfS <- melt(dfS, id.vars = c("Entry_ID", "To-be-remembered_information", "tbr"),
            value.name = 'Cue')%>%
  filter(!is.na(Cue)) %>%
  arrange(`To-be-remembered_information`, Cue)

dfS <- dfS %>%
  arrange(Cue, tbr)
dfS$cue <- seq(1:nrow(dfS))

df3 <- dfS %>%
  mutate(CUE = case_when(Cue == 2 ~ cue - 25,
                         Cue == 3 ~ cue - 15,
                         Cue == 4 ~ cue + 0,
                         Cue == 5 ~ cue + 15,
                         Cue == 6 ~ cue + 25))%>%
  mutate(Tbr = case_when(`To-be-remembered_information` == 1 ~ tbr - 35,
                         `To-be-remembered_information` == 2 ~ tbr - 15,
                         `To-be-remembered_information` == 3 ~ tbr - 5,
                         `To-be-remembered_information` == 4 ~ tbr + 5,
                         `To-be-remembered_information` == 5 ~ tbr + 15,
                         `To-be-remembered_information` == 6 ~ tbr + 35)) %>%
  mutate(Entry_ID = paste(Entry_ID, Cue, sep = '_')) %>%
  arrange(CUE, Tbr)

df3$Cue <- factor(df3$Cue, levels = c(2:6))

ggplot() + geom_segment(data=df3, aes(x = Tbr, y = 1, xend = CUE, yend = 2),
                        alpha = 0.59, show.legend = F)

## Plot bar ----

dfSumCUE <- df3 %>%
  group_by(Cue)%>%
  summarise(maxTbr = max(Tbr), minTbr = min(Tbr), medianCUE = median(CUE),
            maxCUE = max(CUE), minCUE = min(CUE), sumCUE = n()) %>%
  arrange(desc(sumCUE))
Cue <- unique(dfSumCUE$Cue)
dfSumCUE <- dfSumCUE %>%
  mutate(alphaCUE = case_when(Cue %in% Cue[1] ~ alpha[1],
                              Cue %in% Cue[2] ~ alpha[2],
                              Cue %in% Cue[3] ~ alpha[3],
                              Cue %in% Cue[4] ~ alpha[4],
                              Cue %in% Cue[5] ~ alpha[5],
                              Cue %in% Cue[6] ~ alpha[6]) %>% as.factor())


# COMPILE ----
p <- ggplot()+
  geom_sigmoid(data=df1, aes(x = DES, xend = TT, y = 0, yend = 4, group = Entry_ID), 
               alpha = 1, smooth = 3.5, color = '#CFD8DC', size = .8, show.legend = F)+
  geom_sigmoid(data=df2, aes(x = TT, xend = Tbr, y = 4, yend = 8, group = Entry_ID),
               alpha = 1, smooth = 3.5, color = '#CFD8DC', size = .8, show.legend = F)+
  geom_sigmoid(data=df3, aes(x = Tbr, xend = CUE, y = 8, yend = 12, group = Entry_ID),
               alpha = 1, smooth = 3.5, color = '#CFD8DC', size = .8, show.legend = F)+
  geom_segment(data = dfSumDes, aes(x = minDES, xend = maxDES, y = 0, yend = 0),
               size = 4.5, show.legend = F, color = nodePalette[2])+
  geom_segment(data = dfSumTT, aes(x = minTT, xend = maxTT, y = 4, yend = 4),
               size = 4.5, show.legend = F, color = nodePalette[3])+
  geom_segment(data = dfSumTbr, aes(x = minTbr, xend = maxTbr, y = 8, yend = 8),
               size = 4.5, show.legend = F, color = nodePalette[4])+
  geom_segment(data = dfSumCUE, aes(x = minCUE, xend = maxCUE, y = 12, yend = 12),
               size = 4.5, show.legend = F, color = nodePalette[9])+
  geom_label(data = dfSumDes, aes(label = Design, x = medianDES, y = 0), alpha = 0, label.size = NA, fill = nodePalette[1], show.legend = F)+
  geom_label(data = dfSumTT, aes(label = Task_type, x = medianTT, y = 4), alpha = 0, label.size = NA, fill = nodePalette[2], show.legend = F)+
  geom_label(data = dfSumTbr, aes(label = `To-be-remembered_information`, x = medianTbr, y = 8), alpha = 0, label.size = NA, fill = nodePalette[3], show.legend = F)+
  geom_label(data = dfSumCUE, aes(label = Cue, x = medianCUE, y = 12), alpha = 0, label.size = NA, fill = nodePalette[4], show.legend = F)+
  scale_x_continuous(limits = c(-55, NA))+
  geom_text(aes(label = 'Design', x = -45, y = 0), size = 5)+
  geom_text(aes(label = 'Task type', x = -45, y = 4), size = 5)+
  geom_text(aes(label = 'To-be-remembered \n information', x = -45 , y = 8), size = 5)+
  geom_text(aes(label = 'Cue \n manipulation', x = -45, y = 12), size = 5)+
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

ggsave(filename = file.path(result_path,"Interference_Cue.pdf"), p, width = 1228*.8, height = 634*.8, units = "px", dpi = "screen")
message('Create ', file.path(result_path, "Interference_Cue.pdf"))
