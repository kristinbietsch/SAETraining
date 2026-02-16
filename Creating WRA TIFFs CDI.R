# The population data was given by 5-year age groups
# We need to combine into one population file (per year) to have women 15-49

library(raster)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 2000

### Change this to the file location where your data is stored
setwd("C:/Users/KristinBietsch/files/Track20/Cote dIvoire/WorldPop 2025/TIFFs 100m")

# Change these file names to your files
age15<-'civ_f_15_2000.tif' 
age15_raster=raster(age15)

age20<-'civ_f_20_2000.tif' 
age20_raster=raster(age20)

age25<-'civ_f_25_2000.tif' 
age25_raster=raster(age25)

age30<-'civ_f_30_2000.tif' 
age30_raster=raster(age30)

age35<-'civ_f_35_2000.tif' 
age35_raster=raster(age35)

age40<-'civ_f_40_2000.tif' 
age40_raster=raster(age40)

age45<-'civ_f_45_2000.tif' 
age45_raster=raster(age45)

s <- raster::stack(age15_raster, age20_raster, age25_raster, age30_raster, age35_raster, age40_raster, age45_raster)

#run the sum function on the raster stack - i.e. add (non-cumulatively) the rasters together
r <- sum(s)
writeRaster(r, filename = "C:/Users/KristinBietsch/Avenir Health Dropbox/Kristin kbietsch@avenirhealth.org/Avenir Track20/Training Materials/Small Area Estimates 2026/Cote dIvoire/Data/Population/CI_2000_1549", format="GTiff", overwrite=TRUE) 
# This file is now saved to the folder you identified above

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# need to download more

# 2011

# Change this to the file location where your data is stored
setwd("C:/Users/KristinBietsch/files/Track20/Cote dIvoire/WorldPop 2025/TIFFs 100m")


age15<-'civ_f_15_2011.tif' 
age15_raster=raster(age15)

age20<-'civ_f_20_2011.tif' 
age20_raster=raster(age20)

age25<-'civ_f_25_2011.tif' 
age25_raster=raster(age25)

age30<-'civ_f_30_2011.tif' 
age30_raster=raster(age30)

age35<-'civ_f_35_2011.tif' 
age35_raster=raster(age35)

age40<-'civ_f_40_2011.tif' 
age40_raster=raster(age40)

age45<-'civ_f_45_2011.tif' 
age45_raster=raster(age45)

s <- raster::stack(age15_raster, age20_raster, age25_raster, age30_raster, age35_raster, age40_raster, age45_raster)

#run the sum function on the raster stack - i.e. add (non-cumulatively) the rasters together
r <- sum(s)
writeRaster(r, filename = "C:/Users/KristinBietsch/Avenir Health Dropbox/Kristin kbietsch@avenirhealth.org/Avenir Track20/Training Materials/Small Area Estimates 2026/Cote dIvoire/Data/Population/CI_2011_1549", format="GTiff", overwrite=TRUE) 
# This file is now saved to the folder you identified above

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# 2021

# Change this to the file location where your data is stored
setwd("C:/Users/KristinBietsch/files/Track20/Cote dIvoire/WorldPop 2025/TIFFs 1KM")


age15<-'civ_f_15_2021_CN_1km_R2025A_UA_v1.tif' 
age15_raster=raster(age15)

age20<-'civ_f_20_2021_CN_1km_R2025A_UA_v1.tif' 
age20_raster=raster(age20)

age25<-'civ_f_25_2021_CN_1km_R2025A_UA_v1.tif' 
age25_raster=raster(age25)

age30<-'civ_f_30_2021_CN_1km_R2025A_UA_v1.tif' 
age30_raster=raster(age30)

age35<-'civ_f_35_2021_CN_1km_R2025A_UA_v1.tif' 
age35_raster=raster(age35)

age40<-'civ_f_40_2021_CN_1km_R2025A_UA_v1.tif' 
age40_raster=raster(age40)

age45<-'civ_f_45_2021_CN_1km_R2025A_UA_v1.tif' 
age45_raster=raster(age45)

s <- raster::stack(age15_raster, age20_raster, age25_raster, age30_raster, age35_raster, age40_raster, age45_raster)

#run the sum function on the raster stack - i.e. add (non-cumulatively) the rasters together
r <- sum(s)
writeRaster(r, filename =  "C:/Users/KristinBietsch/Avenir Health Dropbox/Kristin kbietsch@avenirhealth.org/Avenir Track20/Training Materials/Small Area Estimates 2026/Cote dIvoire/Data/Population/CI_2021_1549", format="GTiff", overwrite=TRUE) 
# This file is now saved to the folder you identified above

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
