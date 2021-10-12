# Setting a distribution fitting function
fitting_dist1 <- function (x){
  
  Max_value <- max(x)
  Length_record <- length(x)
  # Using package 'extRemes' # install.packages('extRemes')
  library(extRemes)

  # Fitting distribution 
  fit.gumbel <- fevd(x, type = "Gumbel")
  fit.GEV <- fevd(x, type = "GEV")
 # fit.GP <- fevd(x, type = "GP", threshold = min(x))
  
  #plot(fit.gumbel)
  # Confidence interval and return levels
  P100_gumbel <- ci(fit.gumbel, alpha = .05)[2]
  P100_GEV <- ci(fit.GEV, alpha = .05)[2]
 # P100_GP <- ci(fit.GP, alpha = .5)[2]
  
  CI_P100_gumbel1 <- ci(fit.gumbel, alpha = .05)[1]
  CI_P100_gumbel2 <- ci(fit.gumbel, alpha = .05)[3]
  
  CI_P100_GEV1 <- ci(fit.GEV, alpha = .05)[1]
  CI_P100_GEV2 <- ci(fit.GEV, alpha = .05)[3]
  
  
  return_100_year <- c(Length_record,Max_value,  CI_P100_gumbel1, P100_gumbel, CI_P100_gumbel2,CI_P100_GEV1, P100_GEV, CI_P100_GEV2)#, P100_GP)
  names(return_100_year) <- c("Length","Maximum",  "CI_Gumbel1","Gumbel", "CI_Gumbel2", "CI_GEV1","GEV","CI_GEV2")#, "GP")
  return(return_100_year)
}


fevd(x, type = "Gumbel")

fitting_dist2 <- function (x,thres){
  # Using package 'extRemes' # install.packages('extRemes')
  library(extRemes)
  
  # Fitting distribution 
  fit.GP <- fevd(x, type = "GP", threshold = thres)
  

  # Confidence interval and return levels
  P100_GP <- ci(fit.GP, alpha = .05)[2]
  
  CI_P100_GP1 <- ci(fit.GP, alpha = .05)[1]
  CI_P100_GP2 <- ci(fit.GP, alpha = .05)[3]

  return_100_year <- c(CI_P100_GP1, P100_GP, CI_P100_GP2)#, P100_GP)
  names(return_100_year) <- c("CI_GP1","GP", "CI_GP2")#, "GP")
  return(return_100_year)
}

x <- file_x_max_nz

Elevation_range <- function(x){
  y <- NULL
  if(x>2000){
    y <- c("1. Above 2000 m")
  } else if ((1000< x) & (x<2000)){
    y <- c("2. 1000-2000 m")
  } else if (x <1000){
    y <- c ("3. Below 1000 m")
  }
return(y)
}


# # Example dataset
# data(ftcanmax) #Annual maximum precipitation amounts at one rain gauge in Fort Collins, Colorado.
# x <- ftcanmax$Prec
# y <- ftcanmax$Prec*1.5
# z <- list(x,y)
# Ans <- sapply(z, fitting_dist)
# colnames(Ans) <- c("a1", "a2") 


# Reading in the precipitation dataset for different ground stations in Nepal
Ground_station_folder <- "E:\\My_papers\\Drafts\\PMP_Upper_Arun\\Data_sources\\Ground_station_data\\Local_Precipitation_data\\"
list.files(Ground_station_folder)

library(dplyr)
library(tidyr)
library(readxl)

sheet_names <- excel_sheets(paste0(Ground_station_folder,"rainfall_Nepal_20180712.xlsx" ))
#Max_list <- list()
All <- NULL

selected_sheets <- c(1:22, 24:length(sheet_names))

for (i in selected_sheets){
  file_x <- read_xlsx(paste0(Ground_station_folder,"rainfall_Nepal_20180712.xlsx" ), sheet = sheet_names[i])
  #file_x[is.na(file_x)] = 0
  #file_x_mean <- summarize_all(file_x, mean,na.rm = T)[1, 2:ncol(file_x)] %>% as.numeric()
  file_x_max <- summarize_all(file_x, max, na.rm = T)[1, 2:ncol(file_x)] %>% as.numeric()  
  file_x_max_nz <- file_x_max[which(file_x_max>0)]
  
  thres <- min(file_x_max_nz)
  file_x_all <- sort(as.numeric(unlist((file_x[,2:ncol(file_x)]))), decreasing =T)
  #file_x_POT <- sort(as.numeric(unlist((file_x[,2:ncol(file_x)]))), decreasin = T)[1:100]
  file_x_POT <- file_x_all[file_x_all >thres]
  
  #Max_list <- append(Max_list,list(file_x_max_nz))
  print(i)
  All <- rbind(All, data.frame(sheet_names[i],
                               round(as.numeric(fitting_dist1(file_x_max_nz)[1]),2),
                               round(as.numeric(fitting_dist1(file_x_max_nz)[2]),2),
                               round(as.numeric(fitting_dist1(file_x_max_nz)[3]),2),
                               round(as.numeric(fitting_dist1(file_x_max_nz)[4]),2),
                               round(as.numeric(fitting_dist1(file_x_max_nz)[5]),2),
                               round(as.numeric(fitting_dist1(file_x_max_nz)[6]),2),
                               round(as.numeric(fitting_dist1(file_x_max_nz)[7]),2),
                               round(as.numeric(fitting_dist1(file_x_max_nz)[8]),2),
                               
                               round(as.numeric(fitting_dist2(file_x_all, thres)[1]),2),
                               round(as.numeric(fitting_dist2(file_x_all, thres)[2]),2),
                               round(as.numeric(fitting_dist2(file_x_all, thres)[3]),2)))
}
names(All) <- c("Index_no","Length_record", "Max_value","CI1_Gumbel","Gumbel", "CI2_Gumbel", "CI1_GEV","GEV", "CI2_GEV", "CI1_GP","GP", "CI2_GP")

# Loading in the list of lat long to plot them together
Station_list <- read_xlsx(paste0("E:\\My_papers\\Drafts\\PMP_Upper_Arun\\Data_sources\\Ground_station_data\\Local_Precipitation_data\\","met_station_list_final.xlsx"))

Complete_list <- merge(Station_list, All, all.y = T)
Complete_list$Elevation <- as.numeric(Complete_list$Elevation)
Complete_list <- Complete_list %>% arrange(desc(Elevation))
Complete_list$Elev <- sapply(Complete_list$Elevation, FUN = Elevation_range)

write.csv(Complete_list, "E:\\My_papers\\Drafts\\PMP_Upper_Arun\\GIS\\100_year_precip.csv")
write.csv(Complete_list, "E:\\My_papers\\Drafts\\PMP_Upper_Arun\\Analaysis\\100_year_precipitation\\Ground_observation.csv")




library(ggplot2)
#Now plotting them
ggplot(Complete_list, aes(x = as.factor(Index_no), y = Max_value)) +
  geom_point(aes(group = Elev)) +
  geom_point(aes(y = GEV), col = "blue", shape = 8)+
  geom_point(aes(y = CI1_GEV), col = "blue")+
  geom_point(aes(y = CI2_GEV), col = "blue")+
  facet_grid(Elev~.)+
  xlab("Station Number")+
  ylab("100 year Precipitation")

ggplot(Complete_list, aes(x = as.factor(Index_no), y = Max_value)) +
  geom_point() +
  geom_point(aes(y = Gumbel), col = "red", shape = 8)+
  geom_point(aes(y = CI1_Gumbel), col = "red")+
  geom_point(aes(y = CI2_Gumbel), col = "red")

ggplot(Complete_list, aes(x = as.factor(Index_no), y = Max_value)) +
  geom_point() +
  geom_point(aes(y = GP), col = "green", shape = 8)+
  geom_point(aes(y = CI1_GP), col = "green")+
  geom_point(aes(y = CI2_GP), col = "green")

ggplot(Complete_list, aes(x = as.factor(Index_no), y = Max_value)) +
  geom_point() +
  geom_point(aes(y = GEV), col = "blue", shape = 8)+
  geom_point(aes(y = Gumbel), col = "red", shape = 8)+
  geom_point(aes(y = GP), col = "green", shape = 8)+
  facet_grid(Elev~.)+
  xlab("Blue = GEV, Red = Gumbel, Green = GP")+
  ylab("100 year Precipitation")

# Reading the ERA 5 data
ERA5_dir <-  "E:\\My_papers\\Drafts\\PMP_Upper_Arun\\Data_sources\\ERA_5\\Daily_vals\\"
load(paste0(ERA5_dir, "ERA5_Precipitation.Rdata"))
  
