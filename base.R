#############
dataset <- read.csv("./ProjectDataFile/raleigh_police_master_file.csv",header = TRUE,na.strings=c(""," "));
options(stringsAsFactors = FALSE)

dataset <- subset(dataset,LCR !="80Z")
dataset <- na.omit(dataset)
library(ggplot2)
library(rgdal)
Raleigh_Police_Beats <- readOGR(dsn='./ProjectDataFile/Raleigh_Police_Beats', layer='Raleigh_Police_Beats')
IntersectingBeats <- intersect(unique(Raleigh_Police_Beats$BEAT),unique(dataset$BEAT))
subsetcrimset <- subset(dataset,(subset = BEAT %in% IntersectingBeats))
dataset <- subsetcrimset
subsetcrimset <- NULL
IntersectingBeats <- NULL

dataset<-transform(dataset, LCR.DESC = as.character(LCR.DESC), LCR = as.character(LCR) 
                   , INC.DATETIME = as.character(INC.DATETIME), INC.NO = as.character(INC.NO) , LOCATION = as.character(LOCATION));

date<-substring(dataset$INC.DATETIME,1,10)
dateVar = as.Date(date,"%m/%d/%Y")
dataset$INCDATE <- dateVar

datetimeVar = strptime(dataset$INC.DATETIME,format='%m/%d/%Y %I:%M:%S %p')
time <- strftime(datetimeVar, '%H:%M')
dataset$INCTIME <- time

dataset$INCYear <- as.numeric(strftime(datetimeVar, '%Y'))
dataset$INCMonth <- as.numeric(strftime(datetimeVar, '%m'))
dataset$INCDay <- as.numeric(strftime(datetimeVar, '%d'))
dataset$INCHour <- as.numeric(strftime(datetimeVar, '%H'))
dataset$INCMin <- as.numeric(strftime(datetimeVar, '%M'))

Loc <- strsplit(gsub("[()]",'',dataset$LOCATION),"," )
Lang <- gsub("(,.*)$", "", (gsub("[()]",'',dataset$LOCATION)))
Lati <- gsub("^(.*,)", "", (gsub("[()]",'',dataset$LOCATION)))
Lang <- as.numeric(Lang)
Lati <- as.numeric(Lati)
dataset$LOCATION <- Loc
dataset$LONGITUDE <- Lang
dataset$LATITUDE <- Lati

dataset$INCIDENTID <- dataset$INC.NO
dataset$INC.NO <- NULL
dataset$CRIMETYPE <- dataset$LCR
dataset$LCR <- NULL
dataset$CRIMEDESC <- dataset$LCR.DESC
dataset$LCR.DESC <- NULL
Totaldatapoints = NROW(dataset)

library(timeDate)
x <- na.omit(as.character(holidayNYSE(2010:getRmetricsOptions("currentYear"))))
HolidayList = as.Date(x,"%Y-%m-%d")

dataset$ISHOLIDAY <- c(dataset$INCDATE %in% HolidayList)

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

NROW(totalCombined)
NROW(dataset)
dataset <- totalCombined

#########
my <- NULL

myTable <- dataset
myTable$LOCATION <- NULL

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

my$plot3 <- qplot(data = CRIMEVSYEARCOUNT, x = INCYear, y = log(COUNT), color = CRIMEGROUP, geom = "line",
                  ylab = "Crime Count(log)\n", xlab = "\nYear", size = I(.7)) +
  geom_point(size = I(3), shape = 17)  
#+ ggsave(paste("TOP10_crimeCount_Vs_Year",'.png',sep=''))

# my$plot2 <- qplot(data = CRIMEVSMONTHCOUNT, x = INCMonth, y = log(COUNT), color = CRIMEGROUP, geom = "line",
#                   ylab = "Crime Count(log)\n", xlab = "\nMonth", size = I(.7)) +
#   geom_point(size = I(3), shape = 17) 
# #+  ggsave(paste("TOP10_crimeCount_Vs_Month",'.png',sep=''))

my$plot2 <- qplot(data = CRIMEVSMONTHCOUNT, x = INCMonth, y = log(COUNT), color = CRIMEGROUP, geom = "line",
      ylab = "Crime Count(log)\n", xlab = "\nMonth", size = I(.7)) +
  geom_point(size = I(3), shape = 17) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12), labels=c("1"="January", "2"="February", 
                                                                    "3"="March", "4"="April", "5"="May", "6"="June", "7"="July", "8"="August", 
                                                                    "9"="September", "10"="October", "11"="November", "12"="December")) + 
  theme(text = element_text(size=16), axis.text.x = element_text(size=11, angle=25), axis.text.y = element_text(size=16)) 


# my$plot4 <- qplot(data = CRIMEVSHOURCOUNT, x = INCHour, y = log(COUNT), color = CRIMEGROUP, geom = "line",
#                   ylab = "Crime Count(log)\n", xlab = "\nHOUR", size = I(.7)) +
#   geom_point(size = I(3), shape = 17) 
# #+ ggsave(paste("TOP10_crimeCount_Vs_Hour",'.png',sep=''))

my$plot4 <- qplot(data = CRIMEVSHOURCOUNT, x = INCHour, y = log(COUNT), color = CRIMEGROUP, geom = "line",
      ylab = "Crime Count(log)\n", xlab = "\nHOUR", size = I(.7)) +
  geom_point(size = I(3), shape = 17) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), 
                     labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) + 
  theme(text = element_text(size=16), axis.text.x = element_text(size=11, angle=0), axis.text.y = element_text(size=14)) 


myTable1 <-NULL
myTable1 <- dataset
myTable1$LOCATION <- NULL

myTable1$INCWeekDay <- as.numeric(strftime(myTable1$INCDATE, '%u'))
myTable1$INCDt <- strftime(datetimeVar, '%m/%d')

myTable1<-transform(myTable1, INCDay = as.integer(INCDay), INCYear = as.integer(INCYear), INCHour =  as.integer(INCHour), INCMonth = as.integer(INCMonth), INCDt = as.character(INCDt));

# View(myTable1)
# View(dataset)

rough <- sqldf("select [INCWeekDay],[CRIMEGROUP],count(*) as COUNT from myTable1 group by [INCWeekDay],[CRIMEGROUP] order by [INCWeekDay],[CRIMEGROUP]")
Aggrigate <- sqldf("select sum(COUNT) as TOT, [CRIMEGROUP] from rough 
                   where [CRIMEGROUP] <> 'All Others' 
                   group by [CRIMEGROUP] order by TOT desc")
TOP5 <- sqldf("select * from Aggrigate limit 10")
rough <- sqldf("select * from [rough] WHERE [CRIMEGROUP] IN (SELECT [CRIMEGROUP] FROM TOP5) order by COUNT")


my$plot1 <- qplot(data = rough, x = INCWeekDay, y = log(COUNT), color = CRIMEGROUP, geom = "line",
                  ylab = "Crime Count(log)\n", xlab = "\nWeekDay", size = I(.7)) +
  geom_point(size = I(3), shape = 17) + 
  scale_x_discrete(limits = c(1,2,3,4,5,6,7), breaks = c(1,2,3,4,5,6,7), 
                   labels = c("1" = "Monday", "2" = "Tuesday", "3" = "Wednesday", "4" = "Thursday", "5" = "Friday", "6" = "Saturday", "7" = "Sunday")) +
  guides(fill=guide_legend(title="CrimeGroup"))

my$plot1

##############DENSITY PLOT

mapdataset<-NULL
mapdataset$Long<-dataset$LATITUDE
mapdataset$Lat<-dataset$LONGITUDE
mapdataset$Crime<-as.factor(dataset$CRIMEGROUP)
mapdataset<- data.frame(mapdataset)

library(ggplot2)
library(rgdal)
origProj <- Raleigh_Police_Beats@proj4string ## Store original projection
geoattri <- Raleigh_Police_Beats@data
geofeat <- Raleigh_Police_Beats@polygons
CityBeatsBoundary <- fortify(Raleigh_Police_Beats)
base_plot <- ggplot(data=CityBeatsBoundary,aes(x=long, y=lat, group=group)) 
base_plot <- base_plot + geom_polygon(color='black', fill='white')+ 
  coord_equal() + theme(panel.background = element_rect(fill = "lightblue"))


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
library(stringr)

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
    annotate("text",x=-78.75,y=35.95,label=paste(filename, sep=''),size=2) +
    guides(fill=guide_legend(title="Crime Density")) +
    ggsave(file.path('./ProjectImages',paste(filename,'.png',sep='')))
}



##############
