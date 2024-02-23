# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if (interactive() ){ 
  
  # first, identify the working directory. This is different for each machine
  whichComputer <- readline('...running on computer (1 = lab PC, 2 = hiwi laptop) >>>> ')
  
  if (whichComputer == '1') {
    
    setwd("/media/locallab/DATA/HU Project/")
    PathRawData = '/media/locallab/DATA/HU Project/A_Data/01_Raw-Data/'
    PathPreprocessedData = '/media/locallab/DATA/HU Project/A_Data/02_Preprocessed-Data/'
    PathScripts = '/media/locallab/DATA/HU Project/B_Analysis/01-1_Scripts/A_Preprocessing/'
    PathFunctions = '/media/locallab/DATA/HU Project/B_Analysis/01-2_Functions/' } 
  
  else if (whichComputer == '2') {
    setwd("E:/HU Project/")
    PathRawData = 'E:/HU Project/A_Data/01_Raw-Data/'
    PathPreprocessedData = 'E:/HU Project/A_Data/02_Preprocessed-Data/'
    PathScripts = 'E:/HU Project/B_Analysis/01-1_Scripts/A_Preprocessing/'
    PathFunctions = 'E:/HU Project/B_Analysis/01-2_Functions/'}
}