## Jacob Wills GDAD code is for predicting college football Bowl game.
## Made in tribute of Reid Wills. I can Only hope the model predicts football game as well as he did.


install.packages("tidyverse")
library(tidyverse)
install.packages("XML")
library(XML)
install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")
library(caret)
library(rpart)
library(rpart.plot)

## READING IN OFFENSE

Offense2018 = "http://www.cfbstats.com/2018/leader/national/team/offense/split01/category10/sort01.html"
OffenseTable2018 = readHTMLTable(Offense2018, header=T, which=1, stringsAsFactors=T)

Offense2017 = "http://www.cfbstats.com/2017/leader/national/team/offense/split01/category10/sort01.html"
OffenseTable2017 = readHTMLTable(Offense2017, header=T, which=1, stringsAsFactors=F)

Offense2016 = "http://www.cfbstats.com/2016/leader/national/team/offense/split01/category10/sort01.html"
OffenseTable2016 = readHTMLTable(Offense2016, header=T, which=1, stringsAsFactors=F)

Offense2015 = "http://www.cfbstats.com/2015/leader/national/team/offense/split01/category10/sort01.html"
OffenseTable2015 = readHTMLTable(Offense2015, header=T, which=1, stringsAsFactors=F)

## READING IN DEFENSE

defense2018 = "http://www.cfbstats.com/2018/leader/national/team/defense/split01/category10/sort01.html"
defenseTable2018 = readHTMLTable(defense2018, header=T, which=1, stringsAsFactors=F)


defense2017 = "http://www.cfbstats.com/2017/leader/national/team/defense/split01/category10/sort01.html"
defenseTable2017 = readHTMLTable(defense2017, header=T, which=1, stringsAsFactors=F)

defense2016 = "http://www.cfbstats.com/2016/leader/national/team/defense/split01/category10/sort01.html"
defenseTable2016 = readHTMLTable(defense2016, header=T, which=1, stringsAsFactors=F)

defense2015 = "http://www.cfbstats.com/2015/leader/national/team/defense/split01/category10/sort01.html"
defenseTable2015 = readHTMLTable(defense2015, header=T, which=1, stringsAsFactors=F)


## Reading in Turnovers

TOmargin2018 = "http://www.cfbstats.com/2018/leader/national/team/offense/split01/category12/sort01.html"
TOmarginTable2018 = readHTMLTable(TOmargin2018, header=T, which=1, stringsAsFactors=F)

TOmargin2017 = "http://www.cfbstats.com/2017/leader/national/team/offense/split01/category12/sort01.html"
TOmarginTable2017 = readHTMLTable(TOmargin2017, header=T, which=1, stringsAsFactors=F)

TOmargin2016 = "http://www.cfbstats.com/2016/leader/national/team/offense/split01/category12/sort01.html"
TOmarginTable2016 = readHTMLTable(TOmargin2016, header=T, which=1, stringsAsFactors=F)

TOmargin2015 = "http://www.cfbstats.com/2015/leader/national/team/offense/split01/category12/sort01.html"
TOmarginTable2015 = readHTMLTable(TOmargin2015, header=T, which=1, stringsAsFactors=F)

## READING IN OFFENSIVE POINTS

Opoints2018 = "http://www.cfbstats.com/2018/leader/national/team/offense/split01/category09/sort01.html"
OpointsTable2018 = readHTMLTable(Opoints2018, header=T, which=1, stringsAsFactors=F)

Opoints2017 = "http://www.cfbstats.com/2017/leader/national/team/offense/split01/category09/sort01.html"
OpointsTable2017 = readHTMLTable(Opoints2017, header=T, which=1, stringsAsFactors=F)

Opoints2016 = "http://www.cfbstats.com/2016/leader/national/team/offense/split01/category09/sort01.html"
OpointsTable2016 = readHTMLTable(Opoints2016, header=T, which=1, stringsAsFactors=F)

Opoints2015 = "http://www.cfbstats.com/2015/leader/national/team/offense/split01/category09/sort01.html"
OpointsTable2015 = readHTMLTable(Opoints2015, header=T, which=1, stringsAsFactors=F)

## READING IN DEFENSIVE POINTS

Dpoints2018 = "http://www.cfbstats.com/2018/leader/national/team/defense/split01/category09/sort01.html"
DpointsTable2018 = readHTMLTable(Dpoints2018, header=T, which=1, stringsAsFactors=F)

Dpoints2017 = "http://www.cfbstats.com/2017/leader/national/team/defense/split01/category09/sort01.html"
DpointsTable2017 = readHTMLTable(Dpoints2017, header=T, which=1, stringsAsFactors=F)

Dpoints2016 = "http://www.cfbstats.com/2016/leader/national/team/defense/split01/category09/sort01.html"
DpointsTable2016 = readHTMLTable(Dpoints2016, header=T, which=1, stringsAsFactors=F)

Dpoints2015 = "http://www.cfbstats.com/2015/leader/national/team/defense/split01/category09/sort01.html"
DpointsTable2015 = readHTMLTable(Dpoints2015, header=T, which=1, stringsAsFactors=F)

## Colley Ranking


colley2018 <- read.table('colleymatrix2018.txt',header = TRUE, sep="\t",strip.white = T )
colley2017 <- read.table('colleymatrix2017.txt',header = TRUE, sep="\t",strip.white = T )
colley2016 <- read.table('colleymatrix2016.txt',header = TRUE, sep="\t",strip.white = T )
colley2015 <- read.table('colleymatrix2015.txt',header = TRUE, sep="\t",strip.white = T )

## READ IN BOWL GAMES

bowls2018 <- read.csv('bowls2018.txt')
bowls2017 <- read.csv('bowls2017.txt')
bowls2016 <- read.csv('bowls2016.txt')
bowls2015 <- read.csv('bowls2015.txt')

## BRINGING DATA TOGETHER 2018



NewBowls2018 <- left_join( bowls2018, OpointsTable2018, by = c("X"= "Name"))
NewBowls2018 <- select(NewBowls2018, X, X.1, "Points/G" )
NewBowls2018 <- left_join( NewBowls2018, OpointsTable2018, by = c("X.1"= "Name"))
NewBowls2018 <- select(NewBowls2018, X, X.1, "Points/G.x","Points/G.y" )

NewBowls2018 <- left_join( NewBowls2018, OffenseTable2018, by = c("X"= "Name"))
NewBowls2018 <- select(NewBowls2018, X, X.1, "Points/G.x", "Points/G.y" , "Yards/Play")
NewBowls2018 <- left_join( NewBowls2018, OffenseTable2018, by = c("X.1"= "Name"))
NewBowls2018 <- select(NewBowls2018, X, X.1, "Points/G.x","Points/G.y", "Yards/Play.x","Yards/Play.y" )

NewBowls2018 <- left_join( NewBowls2018, DpointsTable2018, by = c("X"= "Name"))
NewBowls2018 <- select(NewBowls2018, X, X.1, "Points/G.x","Points/G.y", "Yards/Play.x","Yards/Play.y", "Points/G" )
NewBowls2018 <- left_join( NewBowls2018, DpointsTable2018, by = c("X.1"= "Name"))
NewBowls2018 <- select(NewBowls2018, X, X.1, "Points/G.x","Points/G.y", "Yards/Play.x","Yards/Play.y", "Points/G.x.x", "Points/G.y.y")

NewBowls2018 <- rename(NewBowls2018,  OPointsPG1 = "Points/G.x" ,  OPointsPG2 = "Points/G.y" , OYPP1 = "Yards/Play.x", OYPP2 = "Yards/Play.y", DPointsPG1 = "Points/G.x.x", DPointsPG2 = "Points/G.y.y")

NewBowls2018 <- left_join( NewBowls2018, defenseTable2018, by = c("X"= "Name"))
NewBowls2018 <- select(NewBowls2018, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, "Yards/Play" )
NewBowls2018 <- left_join( NewBowls2018, defenseTable2018, by = c("X.1"= "Name"))
NewBowls2018 <- select(NewBowls2018, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2,"Yards/Play.x","Yards/Play.y")

NewBowls2018 <- rename(NewBowls2018, DYPP1 = "Yards/Play.x", DYPP2 = "Yards/Play.y")


NewBowls2018 <- left_join( NewBowls2018, TOmarginTable2018, by = c("X"= "Name"))
NewBowls2018 <- select(NewBowls2018, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, DYPP1, DYPP2, "Margin/G")
NewBowls2018 <- left_join( NewBowls2018, TOmarginTable2018, by = c("X.1"= "Name"))
NewBowls2018 <- select(NewBowls2018, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, DYPP1, DYPP2, "Margin/G.x", "Margin/G.y" )

NewBowls2018 <- rename(NewBowls2018, TOPG1 = "Margin/G.x", TOPG2 = "Margin/G.y")


NewBowls2018 <- left_join( NewBowls2018, colley2018, by = c("X"= "team"))
NewBowls2018 <- select(NewBowls2018, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, DYPP1, DYPP2, TOPG1, TOPG2, "rating","top.50..wins")
NewBowls2018 <- left_join( NewBowls2018, colley2018, by = c("X.1"= "team"))
NewBowls2018 <- select(NewBowls2018, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, DYPP1, DYPP2, TOPG1, TOPG2, "rating.x","top.50..wins.x", "rating.y","top.50..wins.y")

NewBowls2018 <- rename(NewBowls2018, colley1 = "rating.x",top501 = "top.50..wins.x",colley2 = "rating.y",top502 ="top.50..wins.y")

NewBowls2018 <- na.omit(NewBowls2018)
NewBowls2018 <- add_column(NewBowls2018, Result = 1)

## 2017 #############################################################################################

NewBowls2017 <- left_join( bowls2017, OpointsTable2017, by = c("X.1"= "Name"))
NewBowls2017 <- select(NewBowls2017, X, X.1, "Points/G" )
NewBowls2017 <- left_join( NewBowls2017, OpointsTable2017, by = c("X"= "Name"))
NewBowls2017 <- select(NewBowls2017, X, X.1, "Points/G.x","Points/G.y" )

NewBowls2017 <- left_join( NewBowls2017, OffenseTable2017, by = c("X.1"= "Name"))
NewBowls2017 <- select(NewBowls2017, X, X.1, "Points/G.x", "Points/G.y" , "Yards/Play")
NewBowls2017 <- left_join( NewBowls2017, OffenseTable2017, by = c("X"= "Name"))
NewBowls2017 <- select(NewBowls2017, X, X.1, "Points/G.x","Points/G.y", "Yards/Play.x","Yards/Play.y" )

NewBowls2017 <- left_join( NewBowls2017, DpointsTable2017, by = c("X.1"= "Name"))
NewBowls2017 <- select(NewBowls2017, X, X.1, "Points/G.x","Points/G.y", "Yards/Play.x","Yards/Play.y", "Points/G" )
NewBowls2017 <- left_join( NewBowls2017, DpointsTable2017, by = c("X"= "Name"))
NewBowls2017 <- select(NewBowls2017, X, X.1, "Points/G.x","Points/G.y", "Yards/Play.x","Yards/Play.y", "Points/G.x.x", "Points/G.y.y")

NewBowls2017 <- rename(NewBowls2017,  OPointsPG2 = "Points/G.x" ,  OPointsPG1 = "Points/G.y" , OYPP2 = "Yards/Play.x", OYPP1 = "Yards/Play.y", DPointsPG2 = "Points/G.x.x", DPointsPG1 = "Points/G.y.y")

NewBowls2017 <- left_join( NewBowls2017, defenseTable2017, by = c("X.1"= "Name"))
NewBowls2017 <- select(NewBowls2017, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, "Yards/Play" )
NewBowls2017 <- left_join( NewBowls2017, defenseTable2017, by = c("X"= "Name"))
NewBowls2017 <- select(NewBowls2017, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2,"Yards/Play.x","Yards/Play.y")

NewBowls2017 <- rename(NewBowls2017, DYPP2 = "Yards/Play.x", DYPP1 = "Yards/Play.y")


NewBowls2017 <- left_join( NewBowls2017, TOmarginTable2017, by = c("X.1"= "Name"))
NewBowls2017 <- select(NewBowls2017, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, DYPP1, DYPP2, "Margin/G")
NewBowls2017 <- left_join( NewBowls2017, TOmarginTable2017, by = c("X"= "Name"))
NewBowls2017 <- select(NewBowls2017, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, DYPP1, DYPP2, "Margin/G.x", "Margin/G.y" )

NewBowls2017 <- rename(NewBowls2017, TOPG2 = "Margin/G.x", TOPG1 = "Margin/G.y")


NewBowls2017 <- left_join( NewBowls2017, colley2017, by = c("X.1"= "team"))
NewBowls2017 <- select(NewBowls2017, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, DYPP1, DYPP2, TOPG1, TOPG2, "rating","top.50..wins")
NewBowls2017 <- left_join( NewBowls2017, colley2017, by = c("X"= "team"))
NewBowls2017 <- select(NewBowls2017, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, DYPP1, DYPP2, TOPG1, TOPG2, "rating.x","top.50..wins.x", "rating.y","top.50..wins.y")

NewBowls2017 <- rename(NewBowls2017, colley2 = "rating.x",top502 = "top.50..wins.x",colley1 = "rating.y",top501 ="top.50..wins.y")

NewBowls2017 <- na.omit(NewBowls2017)
NewBowls2017 <- add_column(NewBowls2017, Result = 0)

## 2016 ###################################################################################################################

NewBowls2016 <- left_join( bowls2016, OpointsTable2016, by = c("X"= "Name"))
NewBowls2016 <- select(NewBowls2016, X, X.1, "Points/G" )
NewBowls2016 <- left_join( NewBowls2016, OpointsTable2016, by = c("X.1"= "Name"))
NewBowls2016 <- select(NewBowls2016, X, X.1, "Points/G.x","Points/G.y" )

NewBowls2016 <- left_join( NewBowls2016, OffenseTable2016, by = c("X"= "Name"))
NewBowls2016 <- select(NewBowls2016, X, X.1, "Points/G.x", "Points/G.y" , "Yards/Play")
NewBowls2016 <- left_join( NewBowls2016, OffenseTable2016, by = c("X.1"= "Name"))
NewBowls2016 <- select(NewBowls2016, X, X.1, "Points/G.x","Points/G.y", "Yards/Play.x","Yards/Play.y" )

NewBowls2016 <- left_join( NewBowls2016, DpointsTable2016, by = c("X"= "Name"))
NewBowls2016 <- select(NewBowls2016, X, X.1, "Points/G.x","Points/G.y", "Yards/Play.x","Yards/Play.y", "Points/G" )
NewBowls2016 <- left_join( NewBowls2016, DpointsTable2016, by = c("X.1"= "Name"))
NewBowls2016 <- select(NewBowls2016, X, X.1, "Points/G.x","Points/G.y", "Yards/Play.x","Yards/Play.y", "Points/G.x.x", "Points/G.y.y")

NewBowls2016 <- rename(NewBowls2016,  OPointsPG1 = "Points/G.x" ,  OPointsPG2 = "Points/G.y" , OYPP1 = "Yards/Play.x", OYPP2 = "Yards/Play.y", DPointsPG1 = "Points/G.x.x", DPointsPG2 = "Points/G.y.y")

NewBowls2016 <- left_join( NewBowls2016, defenseTable2016, by = c("X"= "Name"))
NewBowls2016 <- select(NewBowls2016, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, "Yards/Play" )
NewBowls2016 <- left_join( NewBowls2016, defenseTable2016, by = c("X.1"= "Name"))
NewBowls2016 <- select(NewBowls2016, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2,"Yards/Play.x","Yards/Play.y")

NewBowls2016 <- rename(NewBowls2016, DYPP1 = "Yards/Play.x", DYPP2 = "Yards/Play.y")


NewBowls2016 <- left_join( NewBowls2016, TOmarginTable2016, by = c("X"= "Name"))
NewBowls2016 <- select(NewBowls2016, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, DYPP1, DYPP2, "Margin/G")
NewBowls2016 <- left_join( NewBowls2016, TOmarginTable2016, by = c("X.1"= "Name"))
NewBowls2016 <- select(NewBowls2016, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, DYPP1, DYPP2, "Margin/G.x", "Margin/G.y" )

NewBowls2016 <- rename(NewBowls2016, TOPG1 = "Margin/G.x", TOPG2 = "Margin/G.y")


NewBowls2016 <- left_join( NewBowls2016, colley2016, by = c("X"= "team"))
NewBowls2016 <- select(NewBowls2016, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, DYPP1, DYPP2, TOPG1, TOPG2, "rating","top.50..wins")
NewBowls2016 <- left_join( NewBowls2016, colley2016, by = c("X.1"= "team"))
NewBowls2016 <- select(NewBowls2016, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, DYPP1, DYPP2, TOPG1, TOPG2, "rating.x","top.50..wins.x", "rating.y","top.50..wins.y")

NewBowls2016 <- rename(NewBowls2016, colley1 = "rating.x",top501 = "top.50..wins.x",colley2 = "rating.y",top502 ="top.50..wins.y")

NewBowls2016 <- na.omit(NewBowls2016)
NewBowls2016 <- add_column(NewBowls2016, Result = 1)

## 2015 ####################################################################################################################


NewBowls2015 <- left_join( bowls2015, OpointsTable2015, by = c("X.1"= "Name"))
NewBowls2015 <- select(NewBowls2015, X, X.1, "Points/G" )
NewBowls2015 <- left_join( NewBowls2015, OpointsTable2015, by = c("X"= "Name"))
NewBowls2015 <- select(NewBowls2015, X, X.1, "Points/G.x","Points/G.y" )

NewBowls2015 <- left_join( NewBowls2015, OffenseTable2015, by = c("X.1"= "Name"))
NewBowls2015 <- select(NewBowls2015, X, X.1, "Points/G.x", "Points/G.y" , "Yards/Play")
NewBowls2015 <- left_join( NewBowls2015, OffenseTable2015, by = c("X"= "Name"))
NewBowls2015 <- select(NewBowls2015, X, X.1, "Points/G.x","Points/G.y", "Yards/Play.x","Yards/Play.y" )

NewBowls2015 <- left_join( NewBowls2015, DpointsTable2018, by = c("X.1"= "Name"))
NewBowls2015 <- select(NewBowls2015, X, X.1, "Points/G.x","Points/G.y", "Yards/Play.x","Yards/Play.y", "Points/G" )
NewBowls2015 <- left_join( NewBowls2015, DpointsTable2015, by = c("X"= "Name"))
NewBowls2015 <- select(NewBowls2015, X, X.1, "Points/G.x","Points/G.y", "Yards/Play.x","Yards/Play.y", "Points/G.x.x", "Points/G.y.y")

NewBowls2015 <- rename(NewBowls2015,  OPointsPG2 = "Points/G.x" ,  OPointsPG1 = "Points/G.y" , OYPP2 = "Yards/Play.x", OYPP1 = "Yards/Play.y", DPointsPG2 = "Points/G.x.x", DPointsPG1 = "Points/G.y.y")

NewBowls2015 <- left_join( NewBowls2015, defenseTable2015, by = c("X.1"= "Name"))
NewBowls2015 <- select(NewBowls2015, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, "Yards/Play" )
NewBowls2015 <- left_join( NewBowls2015, defenseTable2015, by = c("X"= "Name"))
NewBowls2015 <- select(NewBowls2015, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2,"Yards/Play.x","Yards/Play.y")

NewBowls2015 <- rename(NewBowls2015, DYPP2 = "Yards/Play.x", DYPP1 = "Yards/Play.y")


NewBowls2015 <- left_join( NewBowls2015, TOmarginTable2015, by = c("X.1"= "Name"))
NewBowls2015 <- select(NewBowls2015, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, DYPP1, DYPP2, "Margin/G")
NewBowls2015 <- left_join( NewBowls2015, TOmarginTable2015, by = c("X"= "Name"))
NewBowls2015 <- select(NewBowls2015, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, DYPP1, DYPP2, "Margin/G.x", "Margin/G.y" )

NewBowls2015 <- rename(NewBowls2015, TOPG2 = "Margin/G.x", TOPG1 = "Margin/G.y")

NewBowls2015 <- left_join( NewBowls2015, colley2015, by = c("X.1"= "team"))
NewBowls2015 <- select(NewBowls2015, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, DYPP1, DYPP2, TOPG1, TOPG2, "rating","top.50..wins")
NewBowls2015 <- left_join( NewBowls2015, colley2015, by = c("X"= "team"))
NewBowls2015 <- select(NewBowls2015, X, X.1,  OPointsPG1,  OPointsPG2, OYPP1, OYPP2, DPointsPG1, DPointsPG2, DYPP1, DYPP2, TOPG1, TOPG2, "rating.x","top.50..wins.x", "rating.y","top.50..wins.y")

NewBowls2015 <- rename(NewBowls2015, colley2 = "rating.x",top502 = "top.50..wins.x",colley1 = "rating.y",top501 ="top.50..wins.y")

NewBowls2015 <- na.omit(NewBowls2015)

NewBowls2015 <- add_column(NewBowls2015, Result = 0)




BowlData <- bind_rows(NewBowls2018,NewBowls2017,NewBowls2016,NewBowls2015)



#### Creating the model ############################################################################
## The data is always team1stat/team2stat and it predicts the probability that team 1 is the winner
## A result of 1 means team 1 is the winnner and a result of 0 means team 2 is the winner.

BowlData <- mutate(BowlData, OpointsRatio = as.numeric(OPointsPG1) / as.numeric(OPointsPG2), DPointsRatio = as.numeric(DPointsPG1) / as.numeric(DPointsPG2) , OYPPRatio = as.numeric(OYPP1) / as.numeric(OYPP2), DYPPRatio = as.numeric(DYPP1) / as.numeric(DYPP2), TOPdiff = as.numeric(TOPG1) - as.numeric(TOPG2), colleyRatio = as.numeric(colley1) / as.numeric(colley2) , TOP50Diff = as.numeric(top501) - as.numeric(top502)) 

FinalBowlData1 <- select(BowlData, OpointsRatio, DPointsRatio  , OYPPRatio, DYPPRatio , TOPdiff, colleyRatio , TOP50Diff, Result) 

inTrain <- createDataPartition(y=FinalBowlData1$Result, p=0.8, list=F)
training <- FinalBowlData1[inTrain,] 
testing <- FinalBowlData1[-inTrain,]

BowlModel1 <- glm(Result ~ OpointsRatio + DPointsRatio  +OYPPRatio + DYPPRatio + TOPdiff +  colleyRatio + TOP50Diff ,family=binomial, data=training)

summary(BowlModel1)
round(exp(coef(BowlModel1)),digits=2)

## Testing The Model #############################################################################
guess <- (1:30)
testingreslts <- select(testing, Result)
correct <- (1:30)
# resultguess <- (1:30)
for (i in 1:30) {

profile.1 <- data.frame(OpointsRatio = testing[i,1], DPointsRatio = testing[i,2], OYPPRatio = testing[i,3], DYPPRatio = testing[i,4], TOPdiff = testing[i,5],  colleyRatio = testing[i,6], TOP50Diff = testing[i,7])

guess[i] <- as.numeric(predict(BowlModel1,newdata = profile.1,
                                 type="response",se.fit=TRUE)[1])

if (guess[i] < 0.5){
  
  guess[i] <- 0
}
else{
  guess[i] <- 1
}


if (guess[i] == testingreslts[i,1]){
  
  correct[i] <- 1
}
else{
  correct[i] <- 0
}



}

percentcorrect <- (sum(correct)/30)*100

cat(" Percent Predicted Correctly  =",percentcorrect,"%","\n")



###### Does Defense Win Championships? #############################################
DPRrange <- range(FinalBowlData1[,2])
DPR <- seq(DPRrange[1], DPRrange[2], 0.01)
D.probs <- matrix(NA,164,4)

for(i in 1:164){
  profile.D <- with(FinalBowlData1,
                     data.frame(OpointsRatio = mean(FinalBowlData1[,1]), 
                                DPointsRatio =DPR[i], 
                                OYPPRatio = mean(FinalBowlData1[,3]),
                                DYPPRatio = mean(FinalBowlData1[,4]),
                                TOPdiff = mean(FinalBowlData1[,5]), 
                                colleyRatio = mean(FinalBowlData1[,6]),
                                TOP50Diff = mean(FinalBowlData1[,7])))
  
  D.prob <- as.numeric(predict(BowlModel1,newdata = profile.D,
                                type="response",se.fit=TRUE)[1])
  D.prob.se <- as.numeric(predict(BowlModel1,newdata = profile.D,
                                   type="response",se.fit=TRUE)[2])
  D.prob.ci <- c(D.prob,D.prob-1.96*D.prob.se,
                  D.prob+1.96*D.prob.se)
  D.probs[i,] <- c(DPR[i],D.prob.ci)
}
### Plotting the DpointsRatio Probabilities #######################################
par(mar = c(4, 5, .1, .1))
plot(D.probs[,1], D.probs[,2], type = "n",
     ylim = c(-.025, 1.025), xlab = "",
     ylab = "", axes = FALSE)
lines(D.probs[,1], D.probs[,2], lwd = 4, lty = 1, col="red") 
lines(D.probs[,1], D.probs[,3], lwd = 2, lty = 2, col="blue")
lines(D.probs[,1], D.probs[,4], lwd = 2, lty = 2, col="blue")
title(ylab = expression("Predicted Prob. of Team 1 Winning"),
      line = 3.5, cex.lab = .8)
title(xlab = expression("Defensive Points Ratio (Team1 Scoring Defense/Team2 scoring Defense)"),
      line = 2.75, cex.lab = .8)
axis(1, at=seq(0,2,0.1),las=2,cex.axis=.8)
axis(2, at = seq(0, 1, .1), las = 2, cex.axis = .8)
box()
rug(jitter(FinalBowlData1[,2], factor=4), ticksize = .015)
legend("topleft", bty = "n", 
       c(expression("Point Est."), 
         expression("95% CI")),
       col=c("red","blue"), lty = c(1, 2), lwd = c(4, 2), cex = .8)
