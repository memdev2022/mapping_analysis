rm(list = ls())



#Step1. Load packages####
{library(dplyr)
library(tidyverse)
library(readxl)
library(stringi)
library(stringr)
library(filesstrings)
library(rstudioapi)}

#Step2. From QC sheet
directory = dirname(getSourceEditorContext()$path) %>% str_extract(pattern = '[^litreview_visualization]+') %>% file.path("litreview_visualization")
data_path = file.path(directory,'00_Data xlsx/')


reviewtable <- read_excel(file.path(data_path,"Lit_Review_QC.xlsx", sep = "")) %>%
  select(Article_ID,Title,Year, Authors)%>%
  rowwise()%>%
  mutate(Year=as.character(Year),
         title = Title %>% str_replace_all('[^[:alnum:]]','') %>% tolower() , .after=Title) %>%
  distinct(Article_ID, Year, Title, .keep_all = T)



#Step3. From LitReview Zotero
zoterotable <- read.csv(file.path(data_path,"LitReview.csv", sep = ""), encoding = "UTF-8") %>%
  transmute(Item.Type,
            Authors=Author,
            Year=Publication.Year,
            Title = str_replace(Title,"\\.$",""),
            Publication.Title,
            Volume,Issue,Pages, Publisher,
            DOI=paste0('https://doi.org/',DOI),
            Url=Url,
            title=Title %>% str_replace_all('[^[:alnum:]]','') %>% tolower())%>%
  mutate(across(everything(), as.character))


(ggplot()+
  geom_line(data=reviewtable %>% group_by(Year) %>% summarise(n=n()), aes(x=as.numeric(Year),y=n), color='red')+
  geom_line(data=zoterotable %>% group_by(Year) %>% summarise(n=n()), aes(x=as.numeric(Year),y=n), color='blue')
) %>% plotly::ggplotly()

# abbreviations for author names ----
zoterotable$Authors.abbreviation = NA

for (i in 1:nrow(zoterotable)){
  
  current.zoterotable = zoterotable[i,]
  current.Authors = current.zoterotable$Authors
  
  current.Authors.list = (str_split(current.Authors,'; ') %>% unlist() %>% str_split(', '))
  current.Authors.abbreviation = ''
  
  for (j in 1:length(current.Authors.list)){
    Authors.connection = ifelse(j==length(current.Authors.list)-1, ', & ',ifelse(j==length(current.Authors.list), '',', '))
    current.Author = current.Authors.list[[j]] %>% unlist()
    current.Author.surname = current.Author[1]
    current.Author.firstname = current.Author[2]
    current.Author.firstname.abbreviation = str_replace_all(current.Author.firstname, '[a-z]+', '.')
    
    current.Authors.abbreviation = paste0(current.Authors.abbreviation, current.Author.surname, ', ', current.Author.firstname.abbreviation, Authors.connection)
    
  }
  
  zoterotable$Authors.abbreviation[i] = current.Authors.abbreviation
  
  
  
}

zoterotable <- zoterotable %>%
  mutate(
    APAref=case_when(
      Item.Type=='journalArticle' ~ sprintf('%s (%s). %s. %s, %s(%s), %s. %s',Authors.abbreviation,Year,Title,Publication.Title,Volume,Issue,Pages,DOI),
      Item.Type=='bookSection' ~ sprintf('%s (%s). %s. In %s (S. %s). %s',Authors.abbreviation,Year,Title,Publication.Title,Pages,Publisher)
    ))

# check title ----
# not case-sensitive
a=setdiff(zoterotable$title,reviewtable$title) %>% sort()
b=setdiff(reviewtable$title,zoterotable$title) %>% sort()
df= data.frame(a=a,b=b)
# case-sensitive
a=setdiff(zoterotable$Title,reviewtable$Title) %>% sort()
b=setdiff(reviewtable$Title,zoterotable$Title) %>% sort()
df= data.frame(a=a,b=b)

# give zoterotable Article_ID
mergetable = merge(zoterotable, reviewtable %>% select(Article_ID,Year,title,Title), by = c('title')) %>%
  mutate(yeardiff= abs(as.numeric(Year.x)-as.numeric(Year.y))) %>%
  select(Article_ID, APAref) %>%
  arrange(Article_ID)

writexl::write_xlsx(mergetable, file.path(data_path, "citation.xlsx"))
