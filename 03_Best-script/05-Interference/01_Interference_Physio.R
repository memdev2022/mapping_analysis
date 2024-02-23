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
  select(Entry_ID, Design, Task_type, To_be_remembered_information, starts_with("Eye"), starts_with("Imaging")) %>% 
  filter(Design %in% c(1,2) & Task_type %in% c(1,2))

# Recode Eye_tracking_1 (=1), Imaging_structure (=2) and Imaging_function (=3)
# If not 0 >> 1, if 0 >> -999
reviewtable <- reviewtable %>%
  mutate(Imaging_function = case_when(Imaging_function == 0 ~ -999,
                                      Imaging_function != 0 ~ 3),
         Imaging_structure = case_when(Imaging_structure == 0 ~ -999,
                                       Imaging_structure != 0 ~ 2),
         Eye_tracking_1 = case_when(Eye_tracking_1 == 0 ~ -999,
                                  Eye_tracking_1 == 1 ~ 1))

# Define NAs
reviewtable[reviewtable==-999] <- NA

# Get df with physiology measures
reviewtable1 <- reviewtable %>%
  filter(!(is.na(Eye_tracking_1) & is.na(Imaging_structure) & is.na(Imaging_function)))


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

### Tbr
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

# plot connections
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


# LAYER III: Tbr & Physio ----
## Plot connection ----
### PHYSIO

# Special case for multiple physio manipulations per entry
# Version 1: ignore ordering by multiple manipulations, focus on real order
# order: Tbr >> Physio
dfP <- reviewtable1 %>%
  select(Entry_ID, To_be_remembered_information, Eye_tracking_1, Imaging_structure, Imaging_function)%>%
  arrange(To_be_remembered_information, Eye_tracking_1, Imaging_structure, Imaging_function)
dfP$tbr <- seq(1:nrow(dfP))

dfP <- melt(dfP, id.vars = c("Entry_ID", "To_be_remembered_information", "tbr"), 
            value.name = 'Physio')%>%
  filter(!is.na(Physio)) %>%
  arrange(To_be_remembered_information, Physio)

dfP <- dfP %>%
  arrange(Physio, tbr)
dfP$physio <- seq(1:nrow(dfP))

df3 <- dfP %>%
  mutate(PHYSIO = case_when(Physio == 1 ~ physio - 30,
                            Physio == 2 ~ physio - 20,
                            Physio == 3 ~ physio - 10))%>%
  mutate(Tbr = case_when(To_be_remembered_information == 1 ~ tbr - 50,
                         To_be_remembered_information == 2 ~ tbr - 25,
                         To_be_remembered_information == 3 ~ tbr - 5,
                         To_be_remembered_information == 4 ~ tbr + 5,
                         To_be_remembered_information == 5 ~ tbr + 25,
                         To_be_remembered_information == 6 ~ tbr + 50)) %>%
  mutate(Entry_ID = paste(Entry_ID, Physio, sep = '_')) %>%
  arrange(PHYSIO, Tbr)

df3$Physio <- factor(df3$Physio, levels = c(1:3))

ggplot() + geom_segment(data=df3, aes(x = Tbr, y = 1, xend = PHYSIO, yend = 2), alpha = 0.59, show.legend = F)


### Plot bar ----
dfSumP <- df3 %>%
  group_by(Physio)%>%
  summarise(maxTbr = max(Tbr), minTbr = min(Tbr), medianPHYSIO = median(PHYSIO),
            maxPHYSIO = max(PHYSIO), minPHYSIO = min(PHYSIO), sumPHYSIO = n()) %>%
  arrange(desc(sumPHYSIO))
Physio <- unique(dfSumP$Physio)
dfSumP <- dfSumP %>%
  mutate(alphaPHYSIO = case_when(Physio %in% Physio[1] ~ alpha[1],
                              Physio %in% Physio[2] ~ alpha[2],
                              Physio %in% Physio[3] ~ alpha[3],
                              Physio %in% Physio[4] ~ alpha[4],
                              Physio %in% Physio[5] ~ alpha[5],
                              Physio %in% Physio[6] ~ alpha[6]) %>% as.factor())



# COMPILE ----
p <- ggplot()+
  geom_sigmoid(data=df1, aes(x = DES, xend = TT, y = 0, yend = 4, group = Entry_ID), 
               alpha = 1, smooth = 3.5, color = '#CFD8DC', size = .8, show.legend = F)+
  geom_sigmoid(data=df2, aes(x = TT, xend = Tbr, y = 4, yend = 8, group = Entry_ID),
               alpha = 1, smooth = 3.5, color = '#CFD8DC', size = .8, show.legend = F)+
  geom_sigmoid(data=df3, aes(x = Tbr, xend = PHYSIO, y = 8, yend = 12, group = Entry_ID),
               alpha = 1, smooth = 3.5, color = '#CFD8DC', size = .8, show.legend = F)+
  geom_segment(data = dfSumDes, aes(x = minDES, xend = maxDES, y = 0, yend = 0),
               size = 4.5, show.legend = F, color = nodePalette[2])+
  geom_segment(data = dfSumTT, aes(x = minTT, xend = maxTT, y = 4, yend = 4),
               size = 4.5, show.legend = F, color = nodePalette[3])+
  geom_segment(data = dfSumTbr, aes(x = minTbr, xend = maxTbr, y = 8, yend = 8),
               size = 4.5, show.legend = F, color = nodePalette[4])+
  geom_segment(data = dfSumP, aes(x = minPHYSIO, xend = maxPHYSIO, y = 12, yend = 12),
               size = 4.5, show.legend = F, color = nodePalette[12:14])+
  geom_label(data = dfSumDes, aes(label = Design, x = medianDES, y = 0), alpha = 0, label.size = NA, fill = nodePalette[1], show.legend = F)+
  geom_label(data = dfSumTT, aes(label = Task_type, x = medianTT, y = 4), alpha = 0, label.size = NA, fill = nodePalette[2], show.legend = F)+
  geom_label(data = dfSumTbr, aes(label = To_be_remembered_information, x = medianTbr, y = 8), alpha = 0, label.size = NA, fill = nodePalette[3], show.legend = F)+
  geom_label(data = dfSumP, aes(label = Physio, x = medianPHYSIO, y = 12), alpha = 0, label.size = NA, fill = nodePalette[4], show.legend = F)+
  scale_x_continuous(limits = c(-80, NA))+
  geom_text(aes(label = 'Design', x = -70, y = 0), size = 5)+
  geom_text(aes(label = 'Task type', x = -70, y = 4), size = 5)+
  geom_text(aes(label = 'To-be-remembered \n information', x = -70 , y = 8), size = 5)+
  geom_text(aes(label = 'Physiological \nmeasures', x = -70, y = 12), size = 5)+
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

plotly::ggplotly(p)
ggsave(filename = file.path(result_path,"Interference_Physio.pdf"), p, width = 1228*.8, height = 634*.8, units = "px", dpi = "screen")
message('Create ', file.path(result_path, "Interference_Physio.pdf"))
