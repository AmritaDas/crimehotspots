rm(list = ls());
# Load the raleigh_police_master_file.csv using read.csv
dataset <- read.csv("./ProjectDataFile/raleigh_police_master_file.csv",header = TRUE,na.strings=c(""," "));
options(stringsAsFactors = FALSE)

########  KEEPING ORIGINAL DATA AND REMOVING/OMMITING INVALID DATAPOINTS 
originalData <- dataset
#dataset <- originalData
#str(dataset)

#View(originalData)
CancelReport <- subset(dataset, LCR=="CAN") 
#View(CancelReport)

notrequired <- subset(originalData,LCR =="80Z")
#View(notrequired)
dim(dataset)
dataset <- subset(dataset,LCR !="80Z")
dim(dataset)
dataset <- na.omit(dataset)
##After ominting NA and null items
dim(dataset)


######## REMOVING DATAPOINT WHICH DOESN'T LIE IN RALEIGH POLICE BEAT but in out DATASET
library(ggplot2)
library(rgdal)
Raleigh_Police_Beats <- readOGR(dsn='./ProjectDataFile/Raleigh_Police_Beats', layer='Raleigh_Police_Beats')
#View(Raleigh_Police_Beats)

##
##CREATE BEAT VS DISTRICT
##

IntersectingBeats <- intersect(unique(Raleigh_Police_Beats$BEAT),unique(dataset$BEAT))
#IntersectingBeats
subsetcrimset <- subset(dataset,(subset = BEAT %in% IntersectingBeats))
#View(subsetcrimset)
dataset <- subsetcrimset
dim(dataset)
#######Taking out year,month,day,time from the INC.DATETIME
#dataset <- originalData
#View(originalData)
#View(dataset);
dataset<-transform(dataset, LCR.DESC = as.character(LCR.DESC), LCR = as.character(LCR) 
                   , INC.DATETIME = as.character(INC.DATETIME), INC.NO = as.character(INC.NO) , LOCATION = as.character(LOCATION));
#View(dataset);
class(dataset$LCR.DESC)
class(dataset$LCR)
class(dataset$INC.DATETIME)
class(dataset$BEAT)
class(dataset$INC.NO)
class(dataset$LOCATION)

#View(dataset$INC.DATETIME)
#dataset$INC.DATETIME
date<-substring(dataset$INC.DATETIME,1,10)
#date
class(date)
dateVar = as.Date(date,"%m/%d/%Y")
class(dateVar)
#View(date)
#View(dateVar)
dataset$INCDATE <- dateVar
#View(dataset$INCDATE);

datetimeVar = strptime(dataset$INC.DATETIME,format='%m/%d/%Y %H:%M')
class(datetimeVar)
#View(datetimeVar)
time <- strftime(datetimeVar, '%H:%M')
#View(time)
class(time)
dataset$INCTIME <- time
dataset$INCYear <- as.numeric(strftime(datetimeVar, '%Y'))
dataset$INCMonth <- as.numeric(strftime(datetimeVar, '%m'))
dataset$INCDay <- as.numeric(strftime(datetimeVar, '%d'))
dataset$INCHour <- as.numeric(strftime(datetimeVar, '%H'))
dataset$INCMin <- as.numeric(strftime(datetimeVar, '%M'))
dataset$INCDt <- strftime(datetimeVar, '%m/%d')
dataset$INCWeekDay <- as.numeric(strftime(datetimeVar, '%u'))

#dataset$INC.DATETIME <- NULL;
#View(dataset);
class(dataset$INCDATE)
class(dataset$INCTIME)

#Taking out LANG and LATI

Loc <- strsplit(gsub("[()]",'',dataset$LOCATION),"," )
Lang <- gsub("(,.*)$", "", (gsub("[()]",'',dataset$LOCATION)))
Lati <- gsub("^(.*,)", "", (gsub("[()]",'',dataset$LOCATION)))

Lang <- as.numeric(Lang)
Lati <- as.numeric(Lati)

class(Lang)
class(Lati)

dataset$LOCATION <- Loc
dataset$LONGITUDE <- Lang
dataset$LATITUDE <- Lati

#dataset$LOCATION <- NULL;
dataset$INCIDENTID <- dataset$INC.NO
dataset$INC.NO <- NULL
dataset$CRIMETYPE <- dataset$LCR
dataset$LCR <- NULL
dataset$CRIMEDESC <- dataset$LCR.DESC
dataset$LCR.DESC <- NULL
Totaldatapoints = NROW(dataset)
Totaldatapoints

##Get List of Holidays

#View(dataset);
#install.packages("timeDate")
library(timeDate)
x <- na.omit(as.character(holidayNYSE(2010:getRmetricsOptions("currentYear"))))
HolidayList = as.Date(x,"%Y-%m-%d")
class(HolidayList)
#View(HolidayList)
dataset$ISHOLIDAY <- c(dataset$INCDATE %in% HolidayList)
#View(dataset)
#summary(dataset)
#str(dataset)

##############################################################################
################## FINDING UNIQUE CRIME GROUPS IN DATA SET AND CREATING CRIME GROUPS
#install.packages("sqldf")
library(sqldf)

myTable <- dataset
myTable$LOCATION <- NULL
colnames(myTable)
#sqldf("select * from myTable")
UniqueCrimetypeDescription <- sqldf("select [CRIMETYPE], [CRIMEDESC] from myTable 
                                    group by [CRIMETYPE] order by [CRIMETYPE]")
#View(UniqueCrimetypeDescription)

##############################################################################

###########################################################################
##crime group creation
valFraud <- dataset[grep("FRAUD", dataset$CRIMEDESC,ignore.case=TRUE), ]
dataset1 <- dataset[setdiff(rownames(dataset),rownames(valFraud)),]
#Subtracting one dataset from another
valForgery <- dataset1[grep("FORGERY", dataset1$CRIMEDESC,ignore.case=TRUE), ]
dataset2 <- dataset1[setdiff(rownames(dataset1),rownames(valForgery)),]

valBurglary  <- dataset2[grep("BURGLARY", dataset2$CRIMEDESC,ignore.case=TRUE), ]
dataset3 <- dataset2[setdiff(rownames(dataset2),rownames(valBurglary)),]
valLarceny  <- dataset3[grep("LARCENY", dataset3$CRIMEDESC,ignore.case=TRUE), ]
dataset4 <- dataset3[setdiff(rownames(dataset3),rownames(valLarceny)),]
valStolen  <- dataset4[grep("STOLEN", dataset4$CRIMEDESC,ignore.case=TRUE), ]
dataset5 <- dataset4[setdiff(rownames(dataset4),rownames(valStolen)),]
valTheft  <- dataset5[grep("THEFT", dataset5$CRIMEDESC,ignore.case=TRUE), ]
dataset6 <- dataset5[setdiff(rownames(dataset5),rownames(valTheft)),]
valRobbery  <- dataset6[grep("ROBBERY", dataset6$CRIMEDESC,ignore.case=TRUE), ]
dataset7 <- dataset6[setdiff(rownames(dataset6),rownames(valRobbery)),]
valRape  <- dataset7[grep("RAPE", dataset7$CRIMEDESC,ignore.case=TRUE), ]
dataset8 <- dataset7[setdiff(rownames(dataset7),rownames(valRape)),]
valChildAbuse  <- dataset8[grep("CHILD ABUSE", dataset8$CRIMEDESC,ignore.case=TRUE), ]
dataset9 <- dataset8[setdiff(rownames(dataset8),rownames(valChildAbuse)),]
valSexOffense  <- dataset9[grep("SEX OFFENSE", dataset9$CRIMEDESC,ignore.case=TRUE), ]
dataset10 <- dataset9[setdiff(rownames(dataset9),rownames(valSexOffense)),]
valMurder  <- dataset10[grep("MURDER", dataset10$CRIMEDESC,ignore.case=TRUE), ]
dataset11 <- dataset10[setdiff(rownames(dataset10),rownames(valMurder)),]
valDrugs  <- dataset11[grep("DRUG", dataset11$CRIMEDESC,ignore.case=TRUE), ]
dataset12 <- dataset11[setdiff(rownames(dataset11),rownames(valDrugs)),]
valDamage <-  dataset12[grep("DAMAGE", dataset12$CRIMEDESC,ignore.case=TRUE), ]
dataset13 <- dataset12[setdiff(rownames(dataset12),rownames(valDamage)),]
valGambling <- dataset13[grep("GAMBLING", dataset13$CRIMEDESC,ignore.case=TRUE), ]
dataset14 <- dataset13[setdiff(rownames(dataset13),rownames(valGambling)),]
valAlcohol <- dataset14[grep("ALCOHOL", dataset14$CRIMEDESC,ignore.case=TRUE), ]
dataset15 <- dataset14[setdiff(rownames(dataset14),rownames(valAlcohol)),]
valDisorderlyConduct <- dataset15[grep("DISORDERLY", dataset15$CRIMEDESC,ignore.case=TRUE), ]
dataset16 <- dataset15[setdiff(rownames(dataset15),rownames(valDisorderlyConduct)),]
valAssault <- dataset16[grep("ASSAULT", dataset16$CRIMEDESC,ignore.case=TRUE), ]
dataset17 <- dataset16[setdiff(rownames(dataset16),rownames(valAssault)),]
valArson <- dataset17[grep("ARSON", dataset17$CRIMEDESC,ignore.case=TRUE), ]
dataset18 <- dataset17[setdiff(rownames(dataset17),rownames(valArson)),]
valHomicide <- dataset18[grep("^12$", dataset18$CRIMETYPE,ignore.case=TRUE), ]
dataset19 <- dataset18[setdiff(rownames(dataset18),rownames(valHomicide)),]
valHumane <- dataset19[grep("HUMANE", dataset19$CRIMEDESC,ignore.case=TRUE), ]
dataset20 <- dataset19[setdiff(rownames(dataset19),rownames(valHumane)),]
valJuvenile <- dataset20[grep("JUVENILE", dataset20$CRIMEDESC,ignore.case=TRUE), ]
dataset21 <- dataset20[setdiff(rownames(dataset20),rownames(valJuvenile)),]
valWeapon <- dataset21[grep("WEAPON", dataset21$CRIMEDESC,ignore.case=TRUE), ]
dataset22 <- dataset21[setdiff(rownames(dataset21),rownames(valWeapon)),]
valCancelReport <- dataset22[grep("CANCEL", dataset22$CRIMEDESC,ignore.case=TRUE), ]
Misc <- dataset22[setdiff(rownames(dataset22),rownames(valCancelReport)),]
#View(Misc)

valSexOffense <- rbind(valSexOffense,valRape)
valMurder <- rbind(valMurder,valHomicide)

valFraud$CRIMEGROUP <- "FRAUD"
valForgery$CRIMEGROUP <- "FORGERY"
valBurglary$CRIMEGROUP <- "BURGLARY / THEFT"
valStolen$CRIMEGROUP <- "STOLEN PROPERTY"
valLarceny$CRIMEGROUP <- "LARCENY"
valTheft$CRIMEGROUP <- "BURGLARY / THEFT"
valDrugs$CRIMEGROUP <- "DRUGS RELATED"
valRobbery$CRIMEGROUP <- "ROBBERY"
valRape$CRIMEGROUP <- "SEX OFFENSE"
valChildAbuse$CRIMEGROUP <- "CHILD ABUSE"
valMurder$CRIMEGROUP <- "MURDER"
valSexOffense$CRIMEGROUP <- "SEX OFFENSE"
valGambling$CRIMEGROUP <- "GAMBLING"
valDamage$CRIMEGROUP <- "DAMAGE TO PROPERTY"
valAlcohol$CRIMEGROUP <- "ALCOHOL ABUSE"
valDisorderlyConduct$CRIMEGROUP <- "DISORDERLY CONDUCT"
valAssault$CRIMEGROUP <- "ASSAULT"
valArson$CRIMEGROUP <- "ARSON"
valHomicide$CRIMEGROUP <- "MURDER"
valHumane$CRIMEGROUP <- "HUMANE"
valJuvenile$CRIMEGROUP <- "JUVENILE"
valWeapon$CRIMEGROUP <- "WEAPON"
valCancelReport$CRIMEGROUP <- "FALSE REPORT"

Misc$CRIMEGROUP <- "All Others"

valFraudForgery <- rbind(valFraud,valForgery)
valBurglaryLarceny <- rbind(valBurglary,valLarceny)
valTheftStolen <- rbind(valTheft,valStolen)
valRobberyChildAbuse <- rbind(valRobbery,valChildAbuse)
valMurderSexOffense <- rbind(valMurder,valSexOffense)
valDrugDamage <- rbind(valDrugs,valDamage)
valGamblingAlcohol <- rbind(valGambling,valAlcohol)
valDisorderlyAssault<- rbind(valDisorderlyConduct,valAssault)
valArsonHumane <- rbind(valArson,valHumane)
valJuvenileWeapon <- rbind(valJuvenile,valWeapon)
valFraudLarceny <- rbind(valFraudForgery,valBurglaryLarceny)
valTheftRobbery <- rbind(valTheftStolen,valRobberyChildAbuse)
valMurderDrug <- rbind(valMurderSexOffense,valDrugDamage)
valGamblingAssault <- rbind(valGamblingAlcohol,valDisorderlyAssault)
valArsonWeapon <- rbind(valArsonHumane,valJuvenileWeapon)
valArsonCancel <- rbind(valArsonWeapon, valCancelReport)
valMurderAssault <- rbind(valMurderDrug,valGamblingAssault)
valFraudRobbery <- rbind(valFraudLarceny,valTheftRobbery)
valArsonAssault <- rbind(valArsonCancel,valMurderAssault)
valArsonFraud  <- rbind(valArsonAssault,valFraudRobbery)

totalCombined <- rbind(valArsonFraud,Misc)

#View(totalCombined)
#View(totalViolent)
#View(totalDrugs)

NROW(totalCombined)
NROW(dataset)
dataset <- totalCombined
#View(dataset)

myTable <- dataset
myTable$LOCATION <- NULL
colnames(myTable)
#sqldf("select * from myTable")
ListOfCrimeGroup <- sqldf("select DISTINCT(CRIMEGROUP) as ListOfCrimeGroup from myTable")
View(ListOfCrimeGroup)
##########################add WEEKDAY and plot ADDED by amrita
myTable <- dataset
myTable$LOCATION <- NULL

#View(myTable)
myTable<-transform(myTable, INCDay = as.integer(INCDay), INCYear = as.integer(INCYear), INCHour =  as.integer(INCHour), INCMonth = as.integer(INCMonth), INCDt = as.character(INCDt));
CRIMEVSDAY <- sqldf("select [CRIMEGROUP], [INCWeekDay], count(*) as COUNT from myTable group by [INCWeekDay], [CRIMEGROUP]")
crimeday <- sqldf("select sum(COUNT) as TOT, [CRIMEGROUP] from CRIMEVSDAY where [CRIMEGROUP] <> 'All Others' 
                  group by [CRIMEGROUP] order by TOT desc")
crimeday2 <- sqldf("select * from crimeday limit 10")
CRIMEVSDAY <- sqldf("select * from CRIMEVSDAY WHERE [CRIMEGROUP] IN (SELECT [CRIMEGROUP] FROM crimeday2) order by COUNT")
p <- ggplot(CRIMEVSDAY, aes(x = INCWeekDay, y = COUNT, fill = CRIMEGROUP)) + 
  geom_bar(stat="identity", position = position_dodge(), color = "black") + 
  xlab("\nDay") + 
  ylab("Crime Count") + 
  scale_x_discrete(limits = c(1,2,3,4,5,6,7), breaks = c(1,2,3,4,5,6,7), 
                   labels = c("1" = "Monday", "2" = "Tuesday", "3" = "Wednesday", "4" = "Thursday", "5" = "Friday", "6" = "Saturday", "7" = "Sunday")) + 
  theme(text = element_text(size=17), axis.text.x = element_text(size=16, color = "black"), axis.text.y = element_text(size=15, color = "black")) +
  guides(fill=guide_legend(title="CrimeGroup")) 

p + ggsave(file.path('./ProjectImages',paste("Crime_Vs_WeekDay",'.png',sep='')))

p1 <- ggplot(CRIMEVSDAY, aes(x = CRIMEVSDAY$INCWeekDay, y = CRIMEVSDAY$COUNT, fill = CRIMEVSDAY$CRIMEGROUP)) + 
  geom_bar(stat="identity", position = position_fill()) + 
  xlab("\nDay") + 
  ylab("Crime Count") + 
  scale_x_discrete(limits = c(1,2,3,4,5,6,7), breaks = c(1,2,3,4,5,6,7), 
                   labels = c("1" = "Monday", "2" = "Tuesday", "3" = "Wednesday", "4" = "Thursday", "5" = "Friday", "6" = "Saturday", "7" = "Sunday")) +
  guides(fill=guide_legend(title="CrimeGroup"))

p1 + ggsave(file.path('./ProjectImages',paste("Crime_Vs_WeekDay_FILL",'.png',sep='')))

###VARIATION IN CRIME over YEARS,MONTHS, and HOUR

#install.packages("sqldf")
library(sqldf)

myTable <- dataset
myTable$LOCATION <- NULL
colnames(myTable)

## WHERE [CRIMEGROUP] IN (SELECT [CRIMEGROUP] FROM TOP5)

CRIMEVSYEARCOUNT <- sqldf("select [INCYear],[CRIMEGROUP],count(*) as COUNT from myTable where INCYear > 2009 and INCYear < 2015 group by [INCYear],[CRIMEGROUP] order by [INCYear],[CRIMEGROUP]")
Aggrigate <- sqldf("select sum(COUNT) as TOT, [CRIMEGROUP] from CRIMEVSYEARCOUNT 
                   where [CRIMEGROUP] <> 'All Others' 
                   group by [CRIMEGROUP] order by TOT desc")
TOP5 <- sqldf("select * from Aggrigate limit 10")
CRIMEVSYEARCOUNT <- sqldf("select * from CRIMEVSYEARCOUNT WHERE [CRIMEGROUP] IN (SELECT [CRIMEGROUP] FROM TOP5) order by COUNT")

Aggrigate <- NULL
CRIMEVSMONTHCOUNT <- sqldf("select [INCMonth],[CRIMEGROUP],count(*) as COUNT  from myTable group by [INCMonth],[CRIMEGROUP] order by [CRIMEGROUP]")
Aggrigate <- sqldf("select sum(COUNT) as TOT, [CRIMEGROUP] from CRIMEVSMONTHCOUNT 
                   where [CRIMEGROUP] <> 'All Others' 
                   group by [CRIMEGROUP] order by TOT desc")
TOP5 <- sqldf("select * from Aggrigate limit 10")
CRIMEVSMONTHCOUNT <- sqldf("select * from CRIMEVSMONTHCOUNT WHERE [CRIMEGROUP] IN (SELECT [CRIMEGROUP] FROM TOP5) order by COUNT")

Aggrigate <- NULL
CRIMEVSHOURCOUNT <- sqldf("select [INCHour],[CRIMEGROUP],count(*) as COUNT  from myTable group by [INCHour],[CRIMEGROUP] order by [CRIMEGROUP]")
Aggrigate <- sqldf("select sum(COUNT) as TOT, [CRIMEGROUP] from CRIMEVSHOURCOUNT 
                   where [CRIMEGROUP] <> 'All Others' 
                   group by [CRIMEGROUP] order by TOT desc")
TOP5 <- sqldf("select * from Aggrigate limit 10")
CRIMEVSHOURCOUNT <- sqldf("select * from CRIMEVSHOURCOUNT WHERE [CRIMEGROUP] IN (SELECT [CRIMEGROUP] FROM TOP5) order by COUNT")
Aggrigate <- NULL


#View(CRIMEVSYEARCOUNT)
#View(CRIMEVSMONTHCOUNT)
#View(CRIMEVSHOURCOUNT)

str(CRIMEVSYEARCOUNT)
str(CRIMEVSMONTHCOUNT)
str(CRIMEVSHOURCOUNT)

qplot(data = CRIMEVSYEARCOUNT, x = INCYear, y = log(COUNT), color = CRIMEGROUP, geom = "line",
      ylab = "Crime Count(log)\n", xlab = "\nYear", size = I(.7)) +
  geom_point(size = I(3), shape = 17) +
  theme(text = element_text(size=17), axis.text.x = element_text(size=16, color = "black"), axis.text.y = element_text(size=15, color = "black")) +
  ggsave(file.path('./ProjectImages',paste("TOP10_crimeCount_Vs_Year",'.png',sep='')))


qplot(data = CRIMEVSMONTHCOUNT, x = INCMonth, y = log(COUNT), color = CRIMEGROUP, geom = "line",
      ylab = "Crime Count(log)\n", xlab = "\nMonth", size = I(.7)) +
  geom_point(size = I(3), shape = 17) +
  ggsave(file.path('./ProjectImages',paste("TOP10_crimeCount_Vs_Month",'.png',sep='')))


qplot(data = CRIMEVSMONTHCOUNT, x = INCMonth, y = log(COUNT), color = CRIMEGROUP, geom = "line",
      ylab = "Crime Count(log)\n", xlab = "\nMonth", size = I(.7)) +
  geom_point(size = I(3), shape = 17) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12), labels=c("1"="January", "2"="February", 
                                                                    "3"="March", "4"="April", "5"="May", "6"="June", "7"="July", "8"="August", 
                                                                    "9"="September", "10"="October", "11"="November", "12"="December")) + 
  theme(text = element_text(size=17), axis.text.x = element_text(size=16, color = "black"), axis.text.y = element_text(size=15, color = "black")) +
  ggsave(file.path('./ProjectImages',paste("TOP10_crimeCount_Vs_Month_With_Names",'.png',sep='')))


qplot(data = CRIMEVSHOURCOUNT, x = INCHour, y = log(COUNT), color = CRIMEGROUP, geom = "line",
      ylab = "Crime Count(log)\n", xlab = "\nHOUR", size = I(.7)) +
  geom_point(size = I(3), shape = 17) +
  ggsave(file.path('./ProjectImages',paste("TOP10_crimeCount_Vs_Hour",'.png',sep='')))


qplot(data = CRIMEVSHOURCOUNT, x = INCHour, y = log(COUNT), color = CRIMEGROUP, geom = "line",
      ylab = "Crime Count(log)\n", xlab = "\nHOUR", size = I(.7)) +
  geom_point(size = I(3), shape = 17) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), 
                     labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) + 
  theme(text = element_text(size=17), axis.text.x = element_text(size=16, color = "black"), axis.text.y = element_text(size=15, color = "black")) +  
  ggsave(file.path('./ProjectImages',paste("TOP10_crimeCount_Vs_Hour_With_Names",'.png',sep='')))

####################################################################################
####################################################################################
###REF: http://data.ral.opendata.arcgis.com/datasets/9a5733e13dd14e2f80f8517738ce8cc6_2

library(ggplot2)
library(rgdal)

#Raleigh_Police_Beats <- readOGR(dsn='./ProjectDataFile/Raleigh_Police_Beats', layer='Raleigh_Police_Beats')
#View(Raleigh_Police_Beats)
#Raleigh_Police_Beats$BEAT

origProj <- Raleigh_Police_Beats@proj4string ## Store original projection
geoattri <- Raleigh_Police_Beats@data
geofeat <- Raleigh_Police_Beats@polygons

CityBeatsBoundary <- fortify(Raleigh_Police_Beats)
base_plot <- ggplot(data=CityBeatsBoundary,aes(x=long, y=lat, group=group)) 

#base_plot <- base_plot + geom_polygon(color='black', fill='lightblue')+ 
#  coord_equal() + theme_grey()

#base_plot <- base_plot + geom_polygon(color='bisque3', fill='#F6F5F5')+ 
#  coord_equal() + theme(panel.background = element_rect(fill = "lightblue"))

base_plot <- base_plot + geom_polygon(color='black', fill='white')+ 
  coord_equal() + 
  theme(panel.background = element_rect(fill = "lightblue"))

plot(base_plot)

####assigning some random colors to crime type
library(RColorBrewer)
crimeCols <- brewer.pal(12,'Paired')
crimeTypes <- list('ASSAULT'=c(crimeCols[1],crimeCols[2]),
                   'WEAPON'=c(crimeCols[2],crimeCols[3]),
                   'MURDER'=c(crimeCols[2],crimeCols[3]),
                   'BURGLARY / THEFT'=c(crimeCols[3],crimeCols[4]),
                   'STOLEN PROPERTY'=c(crimeCols[5],crimeCols[6]),
                   'JUVENILE'=c(crimeCols[7],crimeCols[8]),
                   'SEX OFFENSE'=c(crimeCols[9],crimeCols[10]),
                   'ALCOHOL ABUSE'=c(crimeCols[9],crimeCols[10]),
                   'ARSON'=c(crimeCols[2],crimeCols[3]),
                   'HUMANE'=c(crimeCols[11],crimeCols[12]),
                   'DRUGS RELATED'=c(crimeCols[3],crimeCols[4]),
                   'DAMAGE TO PROPERTY'=c(crimeCols[5],crimeCols[6]),
                   'DISORDERLY CONDUCT'=c(crimeCols[9],crimeCols[10]),
                   'FRAUD'=c(crimeCols[2],crimeCols[3]),
                   'FORGERY'=c(crimeCols[7],crimeCols[8]),
                   'GAMBLING'=c(crimeCols[3],crimeCols[4]),
                   'LARCENY'=c(crimeCols[5],crimeCols[6]),
                   'CHILD ABUSE'=c(crimeCols[7],crimeCols[8]),
                   'FALSE REPORT'=c(crimeCols[9],crimeCols[10]),
                   'All Others'=c(crimeCols[11],crimeCols[12])
)

crimeTypeNames <- names(crimeTypes)
crimeTypeNames

####start for LOOP HERE 
mapdataset<-NULL
mapdataset$Long<-dataset$LATITUDE
mapdataset$Lat<-dataset$LONGITUDE
mapdataset$Crime<-as.factor(dataset$CRIMEGROUP)
mapdataset<- data.frame(mapdataset)
str(mapdataset)

NROW(mapdataset)
#View(mapdataset)
mapdataset <- sqldf("select * from mapdataset where [Long] > -78.84 and [Long] < -78.44 and [Lat] > 35.64 and [Lat] < 36")

NROW(mapdataset)
myTable<-dataset
myTable$LOCATION <- NULL

#View(mapdataset)
str(myTable)
CrimeCountPerGroup <- NULL
CrimeCountPerGroup <- sqldf("select CRIMEGROUP,count(*) as COUNT from myTable where [CRIMEGROUP] <> 'All Others' group by CRIMEGROUP order by COUNT DESC")

library(stringr)
#str_replace_all(str_replace_all("SEX OFFENSE", ' / ', '-'),'\\s','_')
for (crimeType in crimeTypeNames){
  filename <- str_replace_all(str_replace_all(crimeType, ' / ', '-'),'\\s','_')
  mapdataset1 <- subset(mapdataset, (mapdataset$Crime %in% crimeType))
  base_plot + geom_point(aes( x = Long, y = Lat,
                              group=Crime),
                         data = mapdataset1 ,
                         color= crimeTypes[[crimeType]][[1]],
                         shape='x',alpha='1',guide=F) +
    stat_density2d(aes(x = Long, y = Lat, group=Crime,
                       alpha = ..level..,fill=..level..),
                   size = 1, data = mapdataset1,
                   color=crimeTypes[[crimeType]][[2]], geom = 'polygon') +
    annotate("text",x=-78.75,y=35.95,label=paste(filename, sep=''),size=3) +
    guides(fill=guide_legend(title="Crime Density")) +
    ggsave(file.path('./ProjectImages',paste(filename,'.png',sep='')))
}


##################
##################

##ARM
rpb <- Raleigh_Police_Beats
rpb <- data.frame(rpb)


#View(dataset)
armTable <- NULL
armTable$BEAT <- dataset$BEAT
armTable$INCWeekDay <- dataset$INCWeekDay
armTable$INCHour <- dataset$INCHour
armTable$Crime <- dataset$CRIMEGROUP
armTable$DayOfWeek[armTable$INCWeekDay == 1] <- "Monday"
armTable$DayOfWeek[armTable$INCWeekDay == 2] <- "Tuesday"
armTable$DayOfWeek[armTable$INCWeekDay == 3] <- "Wednesday"
armTable$DayOfWeek[armTable$INCWeekDay == 4] <- "Thursday"
armTable$DayOfWeek[armTable$INCWeekDay == 5] <- "Friday"
armTable$DayOfWeek[armTable$INCWeekDay == 6] <- "Saturday"
armTable$DayOfWeek[armTable$INCWeekDay == 7] <- "Sunday"

armTable$Day[armTable$DayOfWeek != "Saturday" & armTable$DayOfWeek != "Sunday"] <- "Weekday"
armTable$Day[armTable$DayOfWeek == "Saturday" | armTable$DayOfWeek == "Sunday"] <- "Weekend"

armTable$Time[armTable$INCHour >=0 & armTable$INCHour <=5] <- "Night"
armTable$Time[armTable$INCHour >=6 & armTable$INCHour <=11] <- "Morning"
armTable$Time[armTable$INCHour >=12 & armTable$INCHour <=17] <- "Afternoon"
armTable$Time[armTable$INCHour >=18 & armTable$INCHour <=23] <- "Evening"

armTable$BEAT<-as.factor(armTable$BEAT)
armTable$INCWeekDay<-NULL
armTable$INCHour<-NULL
armTable$Crime<-as.factor(armTable$Crime)
armTable$Time<-as.factor(armTable$Time)
armTable$DayOfWeek<-as.factor(armTable$DayOfWeek)
armTable$Day<-as.factor(armTable$Day)

str(armTable)

armTable<- data.frame(armTable)
#View(armTable)
unique(dataset$CRIMEGROUP)
#View(rpb)

armTable <- merge(armTable,rpb,by="BEAT")
armTable$OBJECTID <- NULL
armTable$FULLNAME <- NULL
armTable$DISTRICT <- as.factor(armTable$DISTRICT)
armTable <- subset(armTable,Crime != "All Others")

arm <- sqldf("select [Time],[Day],[Crime],[DISTRICT] from armTable")
library(arules)
#View(arm)
rulesCrime <- apriori(arm, parameter = list(minlen=4, supp=0.005, conf=0.1), 
                      appearance = list(rhs=c("Crime=ARSON","Crime=HUMANE","Crime=JUVENILE","Crime=WEAPON","Crime=MURDER",
                                              "Crime=SEX OFFENSE","Crime=DRUGS RELATED","Crime=DAMAGE TO PROPERTY",
                                              "Crime=GAMBLING","Crime=ALCOHOL ABUSE","Crime=DISORDERLY CONDUCT","Crime=ASSAULT",
                                              "Crime=FRAUD","Crime=BURGLARY / THEFT","Crime=LARCENY","Crime=STOLEN PROPERTY",
                                              "Crime=CHILD ABUSE","Crime=FORGERY"), default="lhs"))
rulesCrime.sorted <- sort(rulesCrime, by="lift")
inspect(rulesCrime.sorted)



arm2 <- sqldf("select [Time],[Day],[Crime] from arm")

rulesCrimeMore <- apriori(arm2, parameter = list(minlen=3, supp=0.005, conf=0.2), 
                          appearance = list(rhs=c("Crime=ARSON","Crime=HUMANE","Crime=JUVENILE","Crime=WEAPON","Crime=MURDER",
                                                  "Crime=SEX OFFENSE","Crime=DRUGS RELATED","Crime=DAMAGE TO PROPERTY",
                                                  "Crime=GAMBLING","Crime=ALCOHOL ABUSE","Crime=DISORDERLY CONDUCT","Crime=ASSAULT",
                                                  "Crime=FRAUD","Crime=BURGLARY / THEFT","Crime=LARCENY","Crime=STOLEN PROPERTY",
                                                  "Crime=CHILD ABUSE","Crime=FORGERY"), default="lhs"))

rulesCrimeMore.sorted <- sort(rulesCrimeMore, by="lift")
inspect(rulesCrimeMore.sorted)
str(arm) 

#View(dataset)
