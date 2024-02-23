rm(list=ls())



directory = dirname(getSourceEditorContext()$path) %>% str_extract(pattern = '^(.*?)mapping_analysis')

{
  data_path = file.path(directory,'00_Data xlsx/')
  funcon_path = file.path(directory,'03_Best-script/00_FunCon/')
  script_path = file.path(directory,'03_Best-script/01_Hierarchical-Edge-Bundling/')
  result_path = file.path(directory,'04_Results/')
  descriptives_path = file.path(directory,'05_Descriptives/')
}

# determine identifier variables
identifier_var <- c("Entry_ID", "Authors","Title", "Year")

# Step 2. Create hierarchy and edges from the largest binarized data frame =======
df <- read_excel(file.path(data_path,"Lit_Review_binarized.xlsx"))

# 2.1. list all possible variables and leaves ----
name.variable <- df %>% #name of all variables
  select(-identifier_var)%>%
  colnames()%>%
  str_extract(".*(?=\\s)")%>%
  unique()
n.variable <- length(name.variable) #how many variables all together

name.leaves <- df %>% select(-identifier_var)%>% colnames() #name of all leaves
n.leaves <- length(name.leaves) #how many leaves all together

leaves.frequency<- df%>%
  select(-identifier_var)%>%
  colnames()%>%
  str_extract(".*(?=\\s)")%>%
  table()%>%
  as.data.frame()
n.leaves.per.variable <- leaves.frequency$Freq[match(name.variable,leaves.frequency$.)]

# 2.2. Define hierarchy and vertices ----
hierarchy <- rbind(
  data.frame(from=rep("Origin",n.variable),to=name.variable),
  data.frame(from=rep(name.variable,times=n.leaves.per.variable),to=name.leaves)
)

vertices <- data.frame(name=unique(c(as.character(hierarchy$from),as.character(hierarchy$to))))
vertices$group <- hierarchy$from[match(vertices$name,hierarchy$to)]
vertices$betweengroupid  <- match(vertices$group,unique(vertices$group))
vertices <- vertices %>%
  mutate(groupid = str_extract(name, "\\d+"), .after = betweengroupid)


which(unique(vertices$group)=='Age')


# Step 2'. Customize node labels (www.r-graph-gallery.com) ====

# Let's add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
# calculate the ANGLE of the labels
vertices$id <- NA
myleaves <- which(is.na( match(vertices$name, hierarchy$from) ))
nleaves <- length(myleaves)
vertices$id[ myleaves ] <- seq(1:nleaves)
vertices$angle <- 90 - 360*vertices$id/nleaves #x - 360, x is adjustable

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust <- ifelse( vertices$angle < -90, 1, 0) 

# if use df$name, flip angle BY to make them readable
# vertices$angle <- ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)


# 2.3. Save hierarchy and vertices in xlsx
write_xlsx(hierarchy, path = file.path(data_path,"Hierarchy.xlsx"))
message('Create ', file.path(data_path,"Hierarchy.xlsx"))

write_xlsx(vertices, path = file.path(data_path,"Vertices.xlsx"))
message('Create ', file.path(data_path,"Vertices.xlsx"))