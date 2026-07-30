#####Process Dendrometer Data in Biosphere 2
#Justin Beslity (thanks Justin!)
#2024

#Install and call libraries
#install.packages("dendRoAnalyst")
# install.packages("schoolmath")
library(dendRoAnalyst)
library(devtools)
library(treenetproc)
library(zoo)
library(schoolmath)


#Dendrometer 1 = Middle, co located with R1192b
#Dendrometer 2 = Top, co located with R1176
#Dendrometer 8 = Bottom, co located with R1111
#Dendrometer 10 = Top, co located with R1182
#Dendrometer 11 = Top, co located with R1181
#Dendrometer 12 = Top, co located with R1171 (stayed the same)
#Dendrometer 13 = Top, co located with R1176
#Dendrometer 14 = Middle, co located with R1132 (stayed the same)
#Dendrometer 15 = Bottom, co located with R1111 (stayed the same)


#Place the csv file names in "alldata" vector, do not include ".csv"
#Name of dendrometer will be changed to Bio_# for convenience

alldata <- list()
files <- list.files("data/dendro_data/raw_dendros")
for (i in 1:length(files)) {
  tempfile <- files[i]
  tempsplit <- strsplit(tempfile, "[.]")
  filename <- tempsplit[[1]][1]
  
  alldata[[i]] <- filename
}
alldata <- unlist(alldata)

#Read in all dendro data, fix datetime, and name tree in dataframe based off of csv file
par(mfrow = c(1,1))
den_data <- data.frame(NULL)
for(q in 1:length(alldata)){
    #input text file
    csv <- paste(alldata[q], ".csv", sep = "")
    den <- read.csv(paste0("data/dendro_data/raw_dendros/", csv), header = T, quote = "", skip = 3)
    
    ##Fix Name
    name1 <- substr(alldata[q], start = 1, stop = 3)
    name2 <- substr(alldata[q], start = nchar(alldata[q])-4, stop = nchar(alldata[q]))
    name <- paste(name1, name2, sep = "")
    den$Tree_ID <- name
    
    den <- den[, colSums(is.na(den)) != nrow(den)]
    
    #Fix Date time
    if(grepl(pattern = "T", den$time_local[1])){
      x <- strsplit(den$time_local,split = "T")
    }else{
      x <- strsplit(den$time_local,split = " ")
    }
       
    date <- as.data.frame(t(as.data.frame(x)))
    date$V2 <- gsub("Z", "", date$V2)
    datetime <- paste(date$V1, date$V2, sep = " ")
    if(is.na(as.POSIXct(datetime[1], format = "%Y-%m-%d %H:%M:%S"))){
      datetime <- as.POSIXct(datetime, format = "%m/%d/%Y %H:%M")
    }else{
      datetime <- as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S")
    }
    den$datetime <- datetime
    den$julian <- format(den$datetime, "%j")
    den$julian <- as.numeric(den$julian)

    #Remove data from other locations
    # 
    # if("WiFi.SSID" %in% colnames(den)){
    #   den <- den[which(den$WiFi.SSID == "UAGuest"),]
    # }else{
    #   den <- den[which(den$SSID == "UAGuest"),]
    # }
    # 
    
    
    #Calculate time difference between each measurement
    den$timediff <- NA
    for(i in 2:nrow(den)){
      den$timediff[i] <- as.numeric(strsplit(as.character(difftime(den$datetime[i], den$datetime[i-1], units = "min")), split = ' '))
    }
    
    #Remove points that are less than 15 minutes apart and fix first value
    den <- den[-which(den$timediff < 14),]
    den$timediff[1] <- 15
    
    plot(den$datetime, den$um)
    mtext(text = alldata[q], side = 3, line = 0.5)
    den_data <- rbind(den_data,den)  
}

den_data$minute <- lubridate::minute(den_data$datetime)

#Reassign minute to closes 15 minute interval to match up with all other data collected in B2-TRF
den_data$new_minute <- NULL
for(i in 1:nrow(den_data)){
  x <- seq(from = 0, to = 60, by = 15)
  your.number <- den_data$minute[i]
  target.index <- which(abs(x - your.number) == min(abs(x - your.number)))
  den_data$new_minute[i] <- x[target.index]
}
den_data$day <- lubridate::day(den_data$datetime)
den_data$hour <- lubridate::hour(den_data$datetime)
den_data$second <- 0
den_data$month <- lubridate::month(den_data$datetime)
den_data$year <- lubridate::year(den_data$datetime)

#Compensate for shifts in time that send datapoints into new hours, days, months or years.
for(i in 1:nrow(den_data)){
  if(den_data$new_minute[i] == 60){
    den_data$new_minute[i] = 0
    den_data$hour[i] <- den_data$hour[i]+1
    if(den_data$hour[i] == 24){
      den_data$hour[i] = 0
      den_data$day[i] = den_data$day[i]+1
      if(den_data$month[i+1] != den_data$month[i]){
        den_data$day[i] = 1
        den_data$month[i] = den_data$month[i] + 1
      }
      if(den_data$month[i] == 13){
        den_data$month[i] = 1
        den_data$year[i] = den_data$year[i] +1
      }
    }
  }
}

date <- paste(den_data$year, den_data$month, den_data$day, sep = "-")
date <- as.Date(date, format = "%Y-%m-%d")
times <- paste(den_data$hour, den_data$new_minute, den_data$second, sep = ":")
datetime <- paste(date,times, sep = " ")
datetime <- as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S")
den_data$datetime <- datetime

#####Now we will manually fix jumps

#####
den_number = 6

allden <- unique(den_data$Tree_ID)
bio <- den_data[which(den_data$Tree_ID == allden[den_number]),]
bio <- dplyr::select(bio, datetime, um)
plot(bio$datetime, bio$um)

####jump locator freaks out when you feed it negative numbers so bump all data up by minimum value
if(min(bio$um) < 0){
  bio$um <- bio$um + abs(min(bio$um))
}
plot(bio$datetime, bio$um)
bio <- bio[-c(1, 2), ]

## Taylor stuff ##
# Skip most dates because I wasn't here
bio <- bio |>
  filter(datetime >= as.POSIXct("2026-06-20 00:00:00"))
##

#Lauch jump locator program, move to the console and follow prompts
# v represents minimum jump length to investigate, this may change so see what the program highlights and adjust to taste
# I have found that 50 works well, but sometimes 100 is better
den_jump <- i.jump.locator(df = bio, TreeNum = 1, manual_threshold = 50)

#Once complete just finish this last bit of code
den <- den_data[which(den_data$Tree_ID == allden[den_number]),]
## Taylor stuff ##
den <- den |> 
  filter(datetime >= as.POSIXct("2026-06-20 00:00:00"))
##
# den <- den[-c(1, 2), ]
den <- cbind(den, den_jump)
colnames(den)[ncol(den)] <- "displacement.um.fixed"
csv <- paste(alldata[den_number], "_fixed.csv", sep = "")

#Write the csv with new name demonstrating that it has been fixed
write.csv(den, paste0("data/dendro_data/clean_dendros/", csv))

