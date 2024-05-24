
soil_abundance <- read.csv("~/Master/15hp_project/mGPS-master/mGPS-master/Data/Data/Soil/Dataset_01_22_2018_taxa.csv")
soil_meta <- read.csv("~/Master/15hp_project/mGPS-master/mGPS-master/Data/Data/Soil/Dataset_01_22_2018_enviro.csv")

Fig_2_x <- read.csv("~/Master/15hp_project/mGPS-master/mGPS-master/Data/Data/Soil/kaggle_dataset/Fig.2_x.csv")
Fig_2_y <- read.csv("~/Master/15hp_project/mGPS-master/mGPS-master/Data/Data/Soil/kaggle_dataset/Fig.2_y.csv")
colnames(Fig_2_y)[colnames(Fig_2_y) == "Y"] <- "Site"
colnames(Fig_2_x)[colnames(Fig_2_x) == "X"] <- "Site"


EDFig_6 <- read.csv("~/Master/15hp_project/mGPS-master/mGPS-master/Data/Data/Soil/kaggle_dataset/EDFig.6.csv")

kaggle <- merge(Fig_2_x, Fig_2_y, by = "Site")
