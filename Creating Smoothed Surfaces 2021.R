# Burkina Faso 2021

library(prevR)
library(foreign)
library(tidyverse)
library(sf)
library(haven)
library(sjlabelled)
library(stars)

# Change this to where you want your files saved
setwd("C:/Users/KristinBietsch/Avenir Health Dropbox/Kristin kbietsch@avenirhealth.org/Avenir Track20/Training Materials/Small Area Estimates 2026/Burkina Faso/Data/PrevR Smoothed Surfaces")


# Change this folder and file to the name of your administrative boundaries 
# We are using admin level 0, which is the outline of the entire country
bounds <- sf::st_read("C:/Users/KristinBietsch/Avenir Health Dropbox/Kristin kbietsch@avenirhealth.org/Avenir Track20/Training Materials/Small Area Estimates 2026/Burkina Faso/Data/Admin Boundaries/bfa_admin0.shp")


# Change this folder and file to the name and location of your GPS data from DHS.  Make sure to keep the DBF extension
cl <- read.dbf("C:/Users/KristinBietsch/Avenir Health Dropbox/Kristin kbietsch@avenirhealth.org/Avenir Track20/Training Materials/Small Area Estimates 2026/Burkina Faso/Data/DHS/BFGE81FL/BFGE81FL.dbf") 

# Change this folder and file to the name and location of your women's data from DHS.  Make sure to keep the DTA extension
ch <- read_dta("C:/Users/KristinBietsch/Avenir Health Dropbox/Kristin kbietsch@avenirhealth.org/Avenir Track20/Training Materials/Small Area Estimates 2026/Burkina Faso/Data/DHS/BFIR81FL.DTA", col_select = any_of(c("v001", "v005", "v313", "v502", "v626a")))


# Change your year (last 2 digits).  For example "21" means "2021"
year <- 21

####################################################################################################3
# We are first looking at how the values of each variable are labeled.  These should be the same for all surveys, but it is good to confirm


Labels=get_labels(ch$v502)
Var1=get_values(ch$v502)
MaritalStatus=as.data.frame(cbind(Labels, Var1)) 
# We expect 0 to be never married, 1 to be married, and 2 to be formerly married

Labels=get_labels(ch$v313)
Var1=get_values(ch$v313)
MethodType=as.data.frame(cbind(Labels, Var1)) 
# We expect 0 to be no method, 1 is folk, 2 is traditional, and 3 is modern

Labels=get_labels(ch$v626a)
Var1=get_values(ch$v626a)
Unmet=as.data.frame(cbind(Labels, Var1)) 
# We expect 1 to be unmet for spacing and 2 to be unmet for limiting
# for older surveys, v626a does not exist.  See our file for 2003 with example code on how to add unmet need
####################################################################################################3
# If the values match what we expected above, we should not need to edit this next chunk of code

# Created a dataset to see if women are married
mar <- ch %>% mutate(im=case_when(v502== 1 ~ 1, v502!=1 ~ 0 ),
                     cluster=v001,
                     weight=v005/1000000) %>%
  dplyr::select(im, cluster, weight) %>%
  filter(!is.na(im)) %>%
  filter(!is.na(cluster)) %>%
  filter(!is.na(weight)) 

# Created a dataset to see if women are unmarried
unmar <- ch %>% mutate(im=case_when(v502== 1 ~ 0, v502!=  1 ~ 1 ),
                       cluster=v001,
                       weight=v005/1000000) %>%
  dplyr::select(im, cluster, weight)

# Created a dataset (just for married women) to see if they are using modern methods
mcpr_mar <- ch %>% filter(v502== 1) %>%
  mutate(im=case_when(v313== 3    ~ 1, v313!= 3   ~ 0 ),
         cluster=v001,
         weight=v005/1000000) %>%
  dplyr::select(im, cluster, weight)

# Created a dataset (just for married women) to see if they are using traditional or folk methods
trad_mar <- ch %>% filter(v502==  1) %>%
  mutate(im=case_when(v313 %in% c(1,2)  ~ 1, !v313 %in% c(1,2) ~ 0 ),
         cluster=v001,
         weight=v005/1000000) %>%
  dplyr::select(im, cluster, weight)

# Created a dataset (just for married women) to see if they have unmet need
unmet_mar <- ch %>% filter(v502== 1) %>%
  mutate(im=case_when(v626a %in% c(1, 2)    ~ 1, !v626a  %in% c(1, 2)     ~ 0 ),
         cluster=v001,
         weight=v005/1000000) %>%
  dplyr::select(im, cluster, weight)

# Created a dataset (just for unmarried women) to see if they are using modern methods
mcpr_unmar <- ch %>% filter(v502!=1) %>%
  mutate(im=case_when(v313== 3    ~ 1, v313!= 3   ~ 0 ),
         cluster=v001,
         weight=v005/1000000) %>%
  dplyr::select(im, cluster, weight)

# Created a dataset (just for unmarried women) to see if they are using traditional or folk methods
trad_unmar <- ch %>% filter(v502!= 1) %>%
  mutate(im=case_when(v313 %in% c(1,2)  ~ 1, !v313 %in% c(1,2) ~ 0 ) ,
         cluster=v001,
         weight=v005/1000000) %>%
  dplyr::select(im, cluster, weight)

# Created a dataset (just for unmarried women) to see if they have unmet need
unmet_unmar <- ch %>% filter(v502!= 1) %>%
  mutate(im=case_when(v626a %in% c(1, 2)    ~ 1, !v626a  %in% c(1, 2)     ~ 0 ),
         cluster=v001,
         weight=v005/1000000) %>%
  dplyr::select(im, cluster, weight)

####################################################################################################3
# This code should not need to be edited, we are changing variable names
cl$id <- cl$DHSCLUST
cl$x <- cl$LONGNUM
cl$y <- cl$LATNUM
cl <- cl[cl$SOURCE!="MIS",]

####################################################################################################3
# This code should not need to be edited
# This is creating a smoothed surface for the percent of women married
cl_clean <- cl

for (i in cl_clean$id) {
  temp <- mar[mar$cluster==i,c("im","weight")]
  cl_clean[cl_clean$id==i,"n"] <- nrow(temp)
  cl_clean[cl_clean$id==i,"wn"] <- sum(temp$weight)
  temp <- temp[temp$im==1,]
  cl_clean[cl_clean$id==i,"pos"] <- nrow(temp)
  cl_clean[cl_clean$id==i,"wpos"] <- sum(temp$weight)
}

col <- c(id="id", x="x", y="y", n="n", pos="pos", wn="wn", wpos="wpos")
bf <- as.prevR(cl_clean, col, bounds)

bf <- rings(bf, N=300)
bf.map <- kde(bf, N=300, nb.cells = 200)
plot(bf.map)

married<-st_rasterize(bf.map %>% dplyr::select(k.wprev.N300.RInf, geometry))

married <- married/100
plot(married)

write_stars(married, paste("married", year,".tif", sep=""))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# This code should not need to be edited
# This is creating a smoothed surface for the percent of women unmarried
cl_clean <- cl

for (i in cl_clean$id) {
  temp <- unmar[unmar$cluster==i,c("im","weight")]
  cl_clean[cl_clean$id==i,"n"] <- nrow(temp)
  cl_clean[cl_clean$id==i,"wn"] <- sum(temp$weight)
  temp <- temp[temp$im==1,]
  cl_clean[cl_clean$id==i,"pos"] <- nrow(temp)
  cl_clean[cl_clean$id==i,"wpos"] <- sum(temp$weight)
}

col <- c(id="id", x="x", y="y", n="n", pos="pos", wn="wn", wpos="wpos")
bf <- as.prevR(cl_clean, col, bounds)

bf <- rings(bf, N=300)
bf.map <- kde(bf, N=300, nb.cells = 200)

unmarried <-st_rasterize(bf.map %>% dplyr::select(k.wprev.N300.RInf, geometry))
unmarried <- unmarried/100
plot(unmarried)

write_stars(unmarried, paste("unmarried", year,".tif", sep=""))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# This code should not need to be edited
# This is creating a smoothed surface for the percent of married women using modern methods

cl_clean <- cl

for (i in cl_clean$id) {
  temp <- mcpr_mar[mcpr_mar$cluster==i,c("im","weight")]
  cl_clean[cl_clean$id==i,"n"] <- nrow(temp)
  cl_clean[cl_clean$id==i,"wn"] <- sum(temp$weight)
  temp <- temp[temp$im==1,]
  cl_clean[cl_clean$id==i,"pos"] <- nrow(temp)
  cl_clean[cl_clean$id==i,"wpos"] <- sum(temp$weight)
}

col <- c(id="id", x="x", y="y", n="n", pos="pos", wn="wn", wpos="wpos")
bf <- as.prevR(cl_clean, col, bounds)

bf <- rings(bf, N=300)
bf.map <- kde(bf, N=300, nb.cells = 200)

mcpr_mar <-st_rasterize(bf.map %>% dplyr::select(k.wprev.N300.RInf, geometry))
mcpr_mar <- mcpr_mar/100
plot(mcpr_mar)

write_stars(mcpr_mar, paste("mcpr_mar", year,".tif", sep=""))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# This code should not need to be edited
# This is creating a smoothed surface for the percent of married women using traditional methods

cl_clean <- cl

for (i in cl_clean$id) {
  temp <- trad_mar[trad_mar$cluster==i,c("im","weight")]
  cl_clean[cl_clean$id==i,"n"] <- nrow(temp)
  cl_clean[cl_clean$id==i,"wn"] <- sum(temp$weight)
  temp <- temp[temp$im==1,]
  cl_clean[cl_clean$id==i,"pos"] <- nrow(temp)
  cl_clean[cl_clean$id==i,"wpos"] <- sum(temp$weight)
}

col <- c(id="id", x="x", y="y", n="n", pos="pos", wn="wn", wpos="wpos")
bf <- as.prevR(cl_clean, col, bounds)

bf <- rings(bf, N=300)
bf.map <- kde(bf, N=300, nb.cells = 200)

trad_mar <-st_rasterize(bf.map %>% dplyr::select(k.wprev.N300.RInf, geometry))
trad_mar <- trad_mar/100
plot(trad_mar)

write_stars(trad_mar,  paste("tcpr_mar", year,".tif", sep=""))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# This code should not need to be edited
# This is creating a smoothed surface for the percent of married women with unmet need

cl_clean <- cl

for (i in cl_clean$id) {
  temp <- unmet_mar[unmet_mar$cluster==i,c("im","weight")]
  cl_clean[cl_clean$id==i,"n"] <- nrow(temp)
  cl_clean[cl_clean$id==i,"wn"] <- sum(temp$weight)
  temp <- temp[temp$im==1,]
  cl_clean[cl_clean$id==i,"pos"] <- nrow(temp)
  cl_clean[cl_clean$id==i,"wpos"] <- sum(temp$weight)
}

col <- c(id="id", x="x", y="y", n="n", pos="pos", wn="wn", wpos="wpos")
bf <- as.prevR(cl_clean, col, bounds)

bf <- rings(bf, N=300)
bf.map <- kde(bf, N=300, nb.cells = 200)

unmet_mar <-st_rasterize(bf.map %>% dplyr::select(k.wprev.N300.RInf, geometry))
unmet_mar <- unmet_mar/100
plot(unmet_mar)

write_stars(unmet_mar,  paste("unmet_mar", year,".tif", sep=""))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# This code should not need to be edited
# This is creating a smoothed surface for the percent of unmarried women using modern methods

cl_clean <- cl

for (i in cl_clean$id) {
  temp <- mcpr_unmar[mcpr_unmar$cluster==i,c("im","weight")]
  cl_clean[cl_clean$id==i,"n"] <- nrow(temp)
  cl_clean[cl_clean$id==i,"wn"] <- sum(temp$weight)
  temp <- temp[temp$im==1,]
  cl_clean[cl_clean$id==i,"pos"] <- nrow(temp)
  cl_clean[cl_clean$id==i,"wpos"] <- sum(temp$weight)
}

col <- c(id="id", x="x", y="y", n="n", pos="pos", wn="wn", wpos="wpos")
bf <- as.prevR(cl_clean, col, bounds)

bf <- rings(bf, N=300)
bf.map <- kde(bf, N=300, nb.cells = 200)

mcpr_unmar <-st_rasterize(bf.map %>% dplyr::select(k.wprev.N300.RInf, geometry))
mcpr_unmar <- mcpr_unmar/100
plot(mcpr_unmar)

write_stars(mcpr_unmar, paste("mcpr_unmar", year,".tif", sep=""))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# This code should not need to be edited
# This is creating a smoothed surface for the percent of unmarried women using traditional methods

cl_clean <- cl

for (i in cl_clean$id) {
  temp <- trad_unmar[trad_unmar$cluster==i,c("im","weight")]
  cl_clean[cl_clean$id==i,"n"] <- nrow(temp)
  cl_clean[cl_clean$id==i,"wn"] <- sum(temp$weight)
  temp <- temp[temp$im==1,]
  cl_clean[cl_clean$id==i,"pos"] <- nrow(temp)
  cl_clean[cl_clean$id==i,"wpos"] <- sum(temp$weight)
}

col <- c(id="id", x="x", y="y", n="n", pos="pos", wn="wn", wpos="wpos")
bf <- as.prevR(cl_clean, col, bounds)

bf <- rings(bf, N=300)
bf.map <- kde(bf, N=300, nb.cells = 200)

trad_unmar <-st_rasterize(bf.map %>% dplyr::select(k.wprev.N300.RInf, geometry))
trad_unmar <- trad_unmar/100
plot(trad_unmar)

write_stars(trad_unmar, paste("tcpr_unmar", year,".tif", sep=""))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# This code should not need to be edited
# This is creating a smoothed surface for the percent of unmarried women with unmet need

cl_clean <- cl

for (i in cl_clean$id) {
  temp <- unmet_unmar[unmet_unmar$cluster==i,c("im","weight")]
  cl_clean[cl_clean$id==i,"n"] <- nrow(temp)
  cl_clean[cl_clean$id==i,"wn"] <- sum(temp$weight)
  temp <- temp[temp$im==1,]
  cl_clean[cl_clean$id==i,"pos"] <- nrow(temp)
  cl_clean[cl_clean$id==i,"wpos"] <- sum(temp$weight)
}

col <- c(id="id", x="x", y="y", n="n", pos="pos", wn="wn", wpos="wpos")
bf <- as.prevR(cl_clean, col, bounds)

bf <- rings(bf, N=300)
bf.map <- kde(bf, N=300, nb.cells = 200)

unmet_unmar <-st_rasterize(bf.map %>% dplyr::select(k.wprev.N300.RInf, geometry))
unmet_unmar <- unmet_unmar/100
plot(unmet_unmar)
write_stars(unmet_unmar, paste("unmet_unmar", year,".tif", sep=""))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


##################################################################################
