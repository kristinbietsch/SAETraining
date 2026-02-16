# The population data was given by 5-year age groups
# We need to combine into one population file (per year) to have women 15-49

library(raster)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 2003

### Change this to the file location where your data is stored
setwd("C:/Users/KristinBietsch/Avenir Health Dropbox/Kristin kbietsch@avenirhealth.org/Avenir Track20/Training Materials/Small Area Estimates 2026/Burkina Faso/Data/Population/2003")

# Change these file names to your files
age15<-'bfa_f_15_2003.tif' 
age15_raster=raster(age15)

age20<-'bfa_f_20_2003.tif' 
age20_raster=raster(age20)

age25<-'bfa_f_25_2003.tif' 
age25_raster=raster(age25)

age30<-'bfa_f_30_2003.tif' 
age30_raster=raster(age30)

age35<-'bfa_f_35_2003.tif' 
age35_raster=raster(age35)

age40<-'bfa_f_40_2003.tif' 
age40_raster=raster(age40)

age45<-'bfa_f_45_2003.tif' 
age45_raster=raster(age45)

s <- raster::stack(age15_raster, age20_raster, age25_raster, age30_raster, age35_raster, age40_raster, age45_raster)

#run the sum function on the raster stack - i.e. add (non-cumulatively) the rasters together
r <- sum(s)
writeRaster(r, filename = "BF_2003_1549", format="GTiff", overwrite=TRUE) 
# This file is now saved to the folder you identified above

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# 2010

# Change this to the file location where your data is stored
setwd("C:/Users/KristinBietsch/Avenir Health Dropbox/Kristin kbietsch@avenirhealth.org/Avenir Track20/Training Materials/Small Area Estimates 2026/Burkina Faso/Data/Population/2010")


age15<-'bfa_f_15_2010.tif' 
age15_raster=raster(age15)

age20<-'bfa_f_20_2010.tif' 
age20_raster=raster(age20)

age25<-'bfa_f_25_2010.tif' 
age25_raster=raster(age25)

age30<-'bfa_f_30_2010.tif' 
age30_raster=raster(age30)

age35<-'bfa_f_35_2010.tif' 
age35_raster=raster(age35)

age40<-'bfa_f_40_2010.tif' 
age40_raster=raster(age40)

age45<-'bfa_f_45_2010.tif' 
age45_raster=raster(age45)

s <- raster::stack(age15_raster, age20_raster, age25_raster, age30_raster, age35_raster, age40_raster, age45_raster)

#run the sum function on the raster stack - i.e. add (non-cumulatively) the rasters together
r <- sum(s)
writeRaster(r, filename = "BF_2010_1549", format="GTiff", overwrite=TRUE) 
# This file is now saved to the folder you identified above

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# 2021

# Change this to the file location where your data is stored
setwd("C:/Users/KristinBietsch/Avenir Health Dropbox/Kristin kbietsch@avenirhealth.org/Avenir Track20/Training Materials/Small Area Estimates 2026/Burkina Faso/Data/Population/2021")


age15<-'bfa_f_15_2021_CN_1km_R2025A_UA_v1.tif' 
age15_raster=raster(age15)

age20<-'bfa_f_20_2021_CN_1km_R2025A_UA_v1.tif' 
age20_raster=raster(age20)

age25<-'bfa_f_25_2021_CN_1km_R2025A_UA_v1.tif' 
age25_raster=raster(age25)

age30<-'bfa_f_30_2021_CN_1km_R2025A_UA_v1.tif' 
age30_raster=raster(age30)

age35<-'bfa_f_35_2021_CN_1km_R2025A_UA_v1.tif' 
age35_raster=raster(age35)

age40<-'bfa_f_40_2021_CN_1km_R2025A_UA_v1.tif' 
age40_raster=raster(age40)

age45<-'bfa_f_45_2021_CN_1km_R2025A_UA_v1.tif' 
age45_raster=raster(age45)

s <- raster::stack(age15_raster, age20_raster, age25_raster, age30_raster, age35_raster, age40_raster, age45_raster)

#run the sum function on the raster stack - i.e. add (non-cumulatively) the rasters together
r <- sum(s)
writeRaster(r, filename = "BF_2021_1549", format="GTiff", overwrite=TRUE) 
# This file is now saved to the folder you identified above

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
