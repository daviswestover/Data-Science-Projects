```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE,warning = FALSE, message = FALSE)
library(tidyverse)
library(tidymodels)
library(ggplot2)
library(corrplot)
library(ggthemes)
library(kableExtra)
library(kknn)
library(yardstick)
library(dplyr)
library(naniar)
library(zoo)
library(themis)
library(discrim)
set.seed(1738)
```


### The dataframe I used was Eduardo Tocco's 'NBA Player Stats' off data world. 
### It records the averages stats of a player each year in seasons from 1996-2022. 
### The data was scraped from basketball reference. 
### Basketball Reference is a well respected source and updates using official NBA data.

```{r, echo = FALSE}
data <- read_csv('NBA_Player_Stats.csv') %>% add_column(AS = 0) %>% filter(Year != '1998-1999')

data <- select(data, -c('3PA', '2P', '2PA', 'FTA','TRB', 'FGA'))

head(data)
```
### This file mainly consists of manually entering an All-Star column to the dataset to be able to train and test on.

```{r, echo = FALSE}
#the grind
#TD
data[data$Player =="Tim Duncan*","AS"] = 1
data[data$Player =="Tim Duncan*"& data$Year == '2011-2012',"AS"] = 0
data[data$Player =="Tim Duncan*"& data$Year == '2015-2016',"AS"] = 0
#MJ
data[data$Player =="Michael Jordan*","AS"] = 1
#LeBron
data[data$Player =="LeBron James" & data$Year != '2003-2004',"AS"] = 1
#Kobe
data[data$Player =="Kobe Bryant*","AS"] = 1
#SHAQ
data[data$Player =="Shaquille O'Neal*","AS"] = 1
data[data$Player =="Shaquille O'Neal*" & data$Year == '2010-2011',"AS"] = 0
data[data$Player =="Shaquille O'Neal*" & data$Year == '2009-2010',"AS"] = 0
#Wade
data[data$Player =="Dwyane Wade","AS"] = 1
data[data$Player =="Dwyane Wade" & data$Year == '2003-2004',"AS"] = 0
data[data$Player =="Dwyane Wade" & data$Year == '2016-2017',"AS"] = 0
data[data$Player =="Dwyane Wade" & data$Year == '2017-2018',"AS"] = 0
#KG
data[data$Player =="Kevin Garnett*","AS"] = 1
data[data$Player =="Kevin Garnett*" & data$Year == '2011-2012',"AS"] = 0
data[data$Player =="Kevin Garnett*" & data$Year == '2013-2014',"AS"] = 0
data[data$Player =="Kevin Garnett*" & data$Year == '2014-2015',"AS"] = 0
data[data$Player =="Kevin Garnett*" & data$Year == '2015-2016',"AS"] = 0
#Pierce
data[data$Player =="Paul Pierce*","AS"] = 1
data[data$Player =="Paul Pierce*" & data$Year == '1999-2000',"AS"] = 0
data[data$Player =="Paul Pierce*" & data$Year == '2000-2001',"AS"] = 0
data[data$Player =="Paul Pierce*" & data$Year == '2006-2007',"AS"] = 0
data[data$Player =="Paul Pierce*" & data$Year == '2012-2013',"AS"] = 0
data[data$Player =="Paul Pierce*" & data$Year == '2013-2014',"AS"] = 0
data[data$Player =="Paul Pierce*" & data$Year == '2014-2015',"AS"] = 0
data[data$Player =="Paul Pierce*" & data$Year == '2015-2016',"AS"] = 0
data[data$Player =="Paul Pierce*" & data$Year == '2016-2017',"AS"] = 0
#Ray Allen
data[data$Player =="Ray Allen*","AS"] = 1
data[data$Player =="Ray Allen*" & data$Year == '1997-1998',"AS"] = 0
data[data$Player =="Ray Allen*" & data$Year == '2002-2003',"AS"] = 0
data[data$Player =="Ray Allen*" & data$Year == '2009-2010',"AS"] = 0
data[data$Player =="Ray Allen*" & data$Year == '2011-2012',"AS"] = 0
data[data$Player =="Ray Allen*" & data$Year == '2012-2013',"AS"] = 0
data[data$Player =="Ray Allen*" & data$Year == '2013-2014',"AS"] = 0
#AI
data[data$Player =="Allen Iverson*","AS"] = 1
data[data$Player =="Allen Iverson*" & data$Year == '1997-1998',"AS"] = 0
#Malone
data[data$Player =="Karl Malone*","AS"] = 1
data[data$Player =="Karl Malone*" & data$Year == '2002-2003',"AS"] = 0
data[data$Player =="Karl Malone*" & data$Year == '2003-2004',"AS"] = 0
#kidd
data[data$Player =="Jason Kidd*","AS"] = 1
data[data$Player =="Jason Kidd*" & data$Year == '2004-2005',"AS"] = 0
data[data$Player =="Jason Kidd*" & data$Year == '2005-2006',"AS"] = 0
data[data$Player =="Jason Kidd*" & data$Year == '2008-2009',"AS"] = 0
data[data$Player =="Jason Kidd*" & data$Year == '2010-2011',"AS"] = 0
data[data$Player =="Jason Kidd*" & data$Year == '2011-2012',"AS"] = 0
data[data$Player =="Jason Kidd*" & data$Year == '2012-2013',"AS"] = 0
#payton
data[data$Player =="Gary Payton*","AS"] = 1
data[data$Player =="Gary Payton" & data$Year == '2003-2004',"AS"] = 0
data[data$Player =="Gary Payton" & data$Year == '2004-2005',"AS"] = 0
data[data$Player =="Gary Payton" & data$Year == '2005-2006',"AS"] = 0
#TMAC
data[data$Player =="Tracy McGrady*","AS"] = 1
data[data$Player =="Tracy McGrady*" & data$Year == '1997-1998',"AS"] = 0
data[data$Player =="Tracy McGrady*" & data$Year == '1999-2000',"AS"] = 0
data[data$Player =="Tracy McGrady*" & data$Year == '2007-2008',"AS"] = 0
data[data$Player =="Tracy McGrady*" & data$Year == '2008-2009',"AS"] = 0
data[data$Player =="Tracy McGrady*" & data$Year == '2009-2010',"AS"] = 0
data[data$Player =="Tracy McGrady*" & data$Year == '2010-2011',"AS"] = 0
data[data$Player =="Tracy McGrady*" & data$Year == '2011-2012',"AS"] = 0
#NASH
data[data$Player =="Steve Nash*","AS"] = 1
data[data$Player =="Steve Nash*" & data$Year == '1997-1998',"AS"] = 0
data[data$Player =="Steve Nash*" & data$Year == '1999-2000',"AS"] = 0
data[data$Player =="Steve Nash*" & data$Year == '2000-2001',"AS"] = 0
data[data$Player =="Steve Nash*" & data$Year == '2003-2004',"AS"] = 0
data[data$Player =="Steve Nash*" & data$Year == '2008-2009',"AS"] = 0
data[data$Player =="Steve Nash*" & data$Year == '2012-2013',"AS"] = 0
data[data$Player =="Steve Nash*" & data$Year == '2013-2014',"AS"] = 0
#DIRK
data[data$Player =="Dirk Nowitzki","AS"] = 1
data[data$Player =="Dirk Nowitzki" & data$Year == '1999-2000',"AS"] = 0
data[data$Player =="Dirk Nowitzki" & data$Year == '2000-2001',"AS"] = 0
data[data$Player =="Dirk Nowitzki" & data$Year == '2012-2013',"AS"] = 0
data[data$Player =="Dirk Nowitzki" & data$Year == '2015-2016',"AS"] = 0
data[data$Player =="Dirk Nowitzki" & data$Year == '2016-2017',"AS"] = 0
data[data$Player =="Dirk Nowitzki" & data$Year == '2017-2018',"AS"] = 0
#YAO
data[data$Player =="Yao Ming","AS"] = 1
#Bosh
data[data$Player =="Chris Bosh*","AS"] = 1
data[data$Player =="Chris Bosh*" & data$Year == '2003-2004',"AS"] = 0
data[data$Player =="Chris Bosh*" & data$Year == '2004-2005',"AS"] = 0
#CARMELO
data[data$Player =="Carmelo Anthony","AS"] = 1
data[data$Player =="Carmelo Anthony" & data$Year == '2003-2004',"AS"] = 0
data[data$Player =="Carmelo Anthony" & data$Year == '2004-2005',"AS"] = 0
data[data$Player =="Carmelo Anthony" & data$Year == '2005-2006',"AS"] = 0
data[data$Player =="Carmelo Anthony" & data$Year == '2017-2018',"AS"] = 0
data[data$Player =="Carmelo Anthony" & data$Year == '2018-2019',"AS"] = 0
data[data$Player =="Carmelo Anthony" & data$Year == '2019-2020',"AS"] = 0
data[data$Player =="Carmelo Anthony" & data$Year == '2020-2021',"AS"] = 0
data[data$Player =="Carmelo Anthony" & data$Year == '2021-2022',"AS"] = 0
#CP3
data[data$Player =="Chris Paul","AS"] = 1
data[data$Player =="Chris Paul" & data$Year == '2005-2006',"AS"] = 0
data[data$Player =="Chris Paul" & data$Year == '2006-2007',"AS"] = 0
data[data$Player =="Chris Paul" & data$Year == '2016-2017',"AS"] = 0
data[data$Player =="Chris Paul" & data$Year == '2017-2018',"AS"] = 0
data[data$Player =="Chris Paul" & data$Year == '2018-2019',"AS"] = 0
#durant
data[data$Player =="Kevin Durant","AS"] = 1
data[data$Player =="Kevin Durant" & data$Year == '2007-2008',"AS"] = 0
data[data$Player =="Kevin Durant" & data$Year == '2008-2009',"AS"] = 0
#Missed 19-20, but also excluded from dataset
#westbrook
data[data$Player =="Russell Westbrook","AS"] = 1
data[data$Player =="Russell Westbrook" & data$Year == '2008-2009',"AS"] = 0
data[data$Player =="Russell Westbrook" & data$Year == '2009-2010',"AS"] = 0
data[data$Player =="Russell Westbrook" & data$Year == '2013-2014',"AS"] = 0
data[data$Player =="Russell Westbrook" & data$Year == '2020-2021',"AS"] = 0
data[data$Player =="Russell Westbrook" & data$Year == '2021-2022',"AS"] = 0
#PG
data[data$Player =="Paul George","AS"] = 1
data[data$Player =="Paul George" & data$Year == '2010-2011',"AS"] = 0
data[data$Player =="Paul George" & data$Year == '2011-2012',"AS"] = 0
data[data$Player =="Paul George" & data$Year == '2014-2015',"AS"] = 0
data[data$Player =="Paul George" & data$Year == '2019-2020',"AS"] = 0
data[data$Player =="Paul George" & data$Year == '2021-2022',"AS"] = 0
#Harden
data[data$Player =="James Harden","AS"] = 1
data[data$Player =="James Harden" & data$Year == '2009-2010',"AS"] = 0
data[data$Player =="James Harden" & data$Year == '2010-2011',"AS"] = 0
data[data$Player =="James Harden" & data$Year == '2011-2012',"AS"] = 0
#Kyrie
data[data$Player =="Kyrie Irving","AS"] = 1
data[data$Player =="Kyrie Irving" & data$Year == '2011-2012',"AS"] = 0
data[data$Player =="Kyrie Irving" & data$Year == '2015-2016',"AS"] = 0
data[data$Player =="Kyrie Irving" & data$Year == '2019-2020',"AS"] = 0
data[data$Player =="Kyrie Irving" & data$Year == '2021-2022',"AS"] = 0
#Curry
data[data$Player =="Stephen Curry","AS"] = 1
data[data$Player =="Stephen Curry" & data$Year == '2009-2010',"AS"] = 0
data[data$Player =="Stephen Curry" & data$Year == '2010-2011',"AS"] = 0
data[data$Player =="Stephen Curry" & data$Year == '2011-2012',"AS"] = 0
data[data$Player =="Stephen Curry" & data$Year == '2012-2013',"AS"] = 0
data[data$Player =="Stephen Curry" & data$Year == '2019-2020',"AS"] = 0
#Anthony Davis
data[data$Player =="Anthony Davis","AS"] = 1
data[data$Player =="Anthony Davis" & data$Year == '2012-2013',"AS"] = 0
data[data$Player =="Anthony Davis" & data$Year == '2021-2022',"AS"] = 0
#Butler
data[data$Player =="Jimmy Butler","AS"] = 1
data[data$Player =="Jimmy Butler" & data$Year == '2011-2012',"AS"] = 0
data[data$Player =="Jimmy Butler" & data$Year == '2012-2013',"AS"] = 0
data[data$Player =="Jimmy Butler" & data$Year == '2013-2014',"AS"] = 0
data[data$Player =="Jimmy Butler" & data$Year == '2018-2019',"AS"] = 0
#Giannis
data[data$Player =="Giannis Antetokounmpo","AS"] = 1
data[data$Player =="Giannis Antetokounmpo" & data$Year == '2014-2015',"AS"] = 0
data[data$Player =="Giannis Antetokounmpo" & data$Year == '2015-2016',"AS"] = 0
#Embiid
data[data$Player =="Joel Embiid","AS"] = 1
data[data$Player =="Joel Embiid" & data$Year == '2016-2017',"AS"] = 0
data[data$Player =="Joel Embiid" & data$Year == '2017-2018',"AS"] = 0


#1998
data[data$Player =="Vin Baker" & data$Year == '1997-1998',"AS"] = 1
data[data$Player =="Anfernee Hardaway" & data$Year == '1997-1998',"AS"] = 1
data[data$Player =="Tim Hardaway*" & data$Year == '1997-1998',"AS"] = 1
data[data$Player =="Grant Hill*" & data$Year == '1997-1998',"AS"] = 1
data[data$Player =="Eddie Jones" & data$Year == '1997-1998',"AS"] = 1
data[data$Player =="Shawn Kemp" & data$Year == '1997-1998',"AS"] = 1
data[data$Player =="Reggie Miller*" & data$Year == '1997-1998',"AS"] = 1
data[data$Player =="Shawn Kemp" & data$Year == '1997-1998',"AS"] = 1
data[data$Player =="Dikembe Mutombo" & data$Year == '1997-1998',"AS"] = 1
data[data$Player =="Glen Rice" & data$Year == '1997-1998',"AS"] = 1
data[data$Player =="David Robinson*" & data$Year == '1997-1998',"AS"] = 1
data[data$Player =="Steve Smith" & data$Year == '1997-1998',"AS"] = 1
data[data$Player =="Rik Smits" & data$Year == '1997-1998',"AS"] = 1
data[data$Player =="Antoine Walker" & data$Year == '1997-1998',"AS"] = 1
data[data$Player =="Jayson Williams" & data$Year == '1997-1998',"AS"] = 1
#2000
data[data$Player =="Vince Carter" & data$Year == '1999-2000',"AS"] = 1
data[data$Player =="Dale Davis" & data$Year == '1999-2000',"AS"] = 1
data[data$Player =="Michael Finley" & data$Year == '1999-2000',"AS"] = 1
data[data$Player =="Grant Hill*" & data$Year == '1999-2000',"AS"] = 1
data[data$Player =="Allan Houston" & data$Year == '1999-2000',"AS"] = 1
data[data$Player =="Eddie Jones" & data$Year == '1999-2000',"AS"] = 1
data[data$Player =="Reggie Miller*" & data$Year == '1999-2000',"AS"] = 1
data[data$Player =="Alonzo Mourning" & data$Year == '1999-2000',"AS"] = 1
data[data$Player =="Dikembe Mutombo" & data$Year == '1999-2000',"AS"] = 1
data[data$Player =="David Robinson*" & data$Year == '1999-2000',"AS"] = 1
data[data$Player =="Jerry Stackhouse" & data$Year == '1999-2000',"AS"] = 1
data[data$Player =="John Stockton*" & data$Year == '1999-2000',"AS"] = 1
data[data$Player =="Rasheed Wallace" & data$Year == '1999-2000',"AS"] = 1
data[data$Player =="Chris Webber*" & data$Year == '1999-2000',"AS"] = 1
#2001
data[data$Player =="Vince Carter" & data$Year == '2000-2001',"AS"] = 1
data[data$Player =="Antonio Davis" & data$Year == '2000-2001',"AS"] = 1
data[data$Player =="Vlade Divac*" & data$Year == '2000-2001',"AS"] = 1
data[data$Player =="Michael Finley" & data$Year == '2000-2001',"AS"] = 1
data[data$Player =="Grant Hill*" & data$Year == '2000-2001',"AS"] = 1
data[data$Player =="Allan Houston" & data$Year == '2000-2001',"AS"] = 1
data[data$Player =="Stephon Marbury" & data$Year == '2000-2001',"AS"] = 1
data[data$Player =="Anthony Mason" & data$Year == '2000-2001',"AS"] = 1
data[data$Player =="Antonio McDyess" & data$Year == '2000-2001',"AS"] = 1
data[data$Player =="Alonzo Mourning" & data$Year == '2000-2001',"AS"] = 1
data[data$Player =="Dikembe Mutombo" & data$Year == '2000-2001',"AS"] = 1
data[data$Player =="Theo Retliff" & data$Year == '2000-2001',"AS"] = 1
data[data$Player =="David Robinson*" & data$Year == '2000-2001',"AS"] = 1
data[data$Player =="Glenn Robinson" & data$Year == '2000-2001',"AS"] = 1
data[data$Player =="Latrell Sprewell" & data$Year == '2000-2001',"AS"] = 1
#2002
data[data$Player =="Shareef Adbur-Rahim" & data$Year == '2001-2002',"AS"] = 1
data[data$Player =="Elton Brand" & data$Year == '2001-2002',"AS"] = 1
data[data$Player =="Vince Carter" & data$Year == '2001-2002',"AS"] = 1
data[data$Player =="Baron Davis" & data$Year == '2001-2002',"AS"] = 1
data[data$Player =="Alonzo Mourning" & data$Year == '2001-2002',"AS"] = 1
data[data$Player =="Dikembe Mutombo" & data$Year == '2001-2002',"AS"] = 1
data[data$Player =="Jermaine O'Neal" & data$Year == '2001-2002',"AS"] = 1
data[data$Player =="Peja Stojaković" & data$Year == '2001-2002',"AS"] = 1
data[data$Player =="Wally Szczerbiak" & data$Year == '2001-2002',"AS"] = 1
data[data$Player =="Antoine Walker" & data$Year == '2001-2002',"AS"] = 1
#2003
data[data$Player =="Vince Carter" & data$Year == '2002-2003',"AS"] = 1
data[data$Player =="Steve Francis" & data$Year == '2002-2003',"AS"] = 1
data[data$Player =="Zydrunas Ilgauskas" & data$Year == '2002-2003',"AS"] = 1
data[data$Player =="Stephon Marbury" & data$Year == '2002-2003',"AS"] = 1
data[data$Player =="Shawn Marion" & data$Year == '2002-2003',"AS"] = 1
data[data$Player =="Jamal Mashburn" & data$Year == '2002-2003',"AS"] = 1
data[data$Player =="Brad Miller" & data$Year == '2002-2003',"AS"] = 1
data[data$Player =="Jermaine O'Neal" & data$Year == '2002-2003',"AS"] = 1
data[data$Player =="Peja Stojaković" & data$Year == '2002-2003',"AS"] = 1
data[data$Player =="Antoine Walker" & data$Year == '2002-2003',"AS"] = 1
data[data$Player =="Chris Webber*" & data$Year == '2002-2003',"AS"] = 1
data[data$Player =="Ben Wakkace" & data$Year == '2002-2003',"AS"] = 1
#2004
data[data$Player =="Metta World Peace" & data$Year == '2003-2004',"AS"] = 1
data[data$Player =="Vince Carter" & data$Year == '2003-2004',"AS"] = 1
data[data$Player =="Sam Cassell" & data$Year == '2003-2004',"AS"] = 1
data[data$Player =="Baron Davis" & data$Year == '2003-2004',"AS"] = 1
data[data$Player =="Steve Francis" & data$Year == '2003-2004',"AS"] = 1
data[data$Player =="Andrei Kirilenko" & data$Year == '2003-2004',"AS"] = 1
data[data$Player =="Jamaal Magloire" & data$Year == '2003-2004',"AS"] = 1
data[data$Player =="Kenyon Martin" & data$Year == '2003-2004',"AS"] = 1
data[data$Player =="Brad Miller" & data$Year == '2003-2004',"AS"] = 1
data[data$Player =="Jermaine O'Neal" & data$Year == '2003-2004',"AS"] = 1
data[data$Player =="Michael Redd" & data$Year == '2003-2004',"AS"] = 1
data[data$Player =="Peja Stojaković" & data$Year == '2003-2004',"AS"] = 1
data[data$Player =="Ben Wallace" & data$Year == '2003-2004',"AS"] = 1
#2005
data[data$Player =="Gilbert Arenas" & data$Year == '2004-2005',"AS"] = 1
data[data$Player =="Vince Carter" & data$Year == '2004-2005',"AS"] = 1
data[data$Player =="Manu Ginóbili*" & data$Year == '2004-2005',"AS"] = 1
data[data$Player =="Grant Hill*" & data$Year == '2004-2005',"AS"] = 1
data[data$Player =="Zydrunas Ilgauskas" & data$Year == '2004-2005',"AS"] = 1
data[data$Player =="Antawn Jamison" & data$Year == '2004-2005',"AS"] = 1
data[data$Player =="Rashard Lewis" & data$Year == '2004-2005',"AS"] = 1
data[data$Player =="Shawn Marion" & data$Year == '2004-2005',"AS"] = 1
data[data$Player =="Zydrunas Ilgauskas" & data$Year == '2004-2005',"AS"] = 1
data[data$Player =="Jermaine O'Neal" & data$Year == '2004-2005',"AS"] = 1
data[data$Player =="Amar'e Stoudemire" & data$Year == '2004-2005',"AS"] = 1
data[data$Player =="Ben Wallace*" & data$Year == '2004-2005',"AS"] = 1
#2006
data[data$Player =="Gilbert Arenas" & data$Year == '2005-2006',"AS"] = 1
data[data$Player =="Chauncey Billups" & data$Year == '2005-2006',"AS"] = 1
data[data$Player =="Elton Brand" & data$Year == '2005-2006',"AS"] = 1
data[data$Player =="Vince Carter" & data$Year == '2005-2006',"AS"] = 1
data[data$Player =="Pau Gasol" & data$Year == '2005-2006',"AS"] = 1
data[data$Player =="Richard Hamilton" & data$Year == '2005-2006',"AS"] = 1
data[data$Player =="Shawn Marion" & data$Year == '2005-2006',"AS"] = 1
data[data$Player =="Jermaine O'Neal" & data$Year == '2005-2006',"AS"] = 1
data[data$Player =="Tony Parker" & data$Year == '2005-2006',"AS"] = 1
data[data$Player =="Ben Wallace*" & data$Year == '2005-2006',"AS"] = 1
data[data$Player =="Rasheed Wallace" & data$Year == '2005-2006',"AS"] = 1
#2007
data[data$Player =="Gilbert Arenas" & data$Year == '2006-2007',"AS"] = 1
data[data$Player =="Chauncey Billups" & data$Year == '2006-2007',"AS"] = 1
data[data$Player =="Carlos Boozer" & data$Year == '2006-2007',"AS"] = 1
data[data$Player =="Caron Butler" & data$Year == '2006-2007',"AS"] = 1
data[data$Player =="Vince Carter" & data$Year == '2006-2007',"AS"] = 1
data[data$Player =="Richard Hamilton" & data$Year == '2006-2007',"AS"] = 1
data[data$Player =="Dwight Howard" & data$Year == '2006-2007',"AS"] = 1
data[data$Player =="Josh Howard" & data$Year == '2006-2007',"AS"] = 1
data[data$Player =="Joe Johnson" & data$Year == '2006-2007',"AS"] = 1
data[data$Player =="Shawn Marion" & data$Year == '2006-2007',"AS"] = 1
data[data$Player =="Mehmet Okur" & data$Year == '2006-2007',"AS"] = 1
data[data$Player =="Jermaine O'Neal" & data$Year == '2006-2007',"AS"] = 1
#2008
data[data$Player =="Chauncey Billups" & data$Year == '2007-2008',"AS"] = 1
data[data$Player =="Carlos Boozer" & data$Year == '2007-2008',"AS"] = 1
data[data$Player =="Caron Butler" & data$Year == '2007-2008',"AS"] = 1
data[data$Player =="Richard Hamilton" & data$Year == '2007-2008',"AS"] = 1
data[data$Player =="Dwight Howard" & data$Year == '2007-2008',"AS"] = 1
data[data$Player =="Antawn Jamison" & data$Year == '2007-2008',"AS"] = 1
data[data$Player =="Joe Johnson" & data$Year == '2007-2008',"AS"] = 1
data[data$Player =="Brandon Roy" & data$Year == '2007-2008',"AS"] = 1
data[data$Player =="Amar'e Stoudemire" & data$Year == '2007-2008',"AS"] = 1
data[data$Player =="Rasheed Wallace" & data$Year == '2007-2008',"AS"] = 1
#2009
data[data$Player =="Chauncey Billups" & data$Year == '2008-2009',"AS"] = 1
data[data$Player =="Chauncey Billups" & data$Year == '2008-2009',"AS"] = 1
data[data$Player =="Pau Gasol" & data$Year == '2008-2009',"AS"] = 1
data[data$Player =="Danny Granger" & data$Year == '2008-2009',"AS"] = 1
data[data$Player =="Devin Harris" & data$Year == '2008-2009',"AS"] = 1
data[data$Player =="Dwight Howard" & data$Year == '2008-2009',"AS"] = 1
data[data$Player =="Joe Johnson" & data$Year == '2008-2009',"AS"] = 1
data[data$Player =="Rashard Lewis" & data$Year == '2008-2009',"AS"] = 1
data[data$Player =="Jameer Nelson" & data$Year == '2008-2009',"AS"] = 1
data[data$Player =="Tony Parker" & data$Year == '2008-2009',"AS"] = 1
data[data$Player =="Brandon Roy" & data$Year == '2008-2009',"AS"] = 1
data[data$Player =="Amar'e Stoudemire" & data$Year == '2008-2009',"AS"] = 1
data[data$Player =="David West" & data$Year == '2008-2009',"AS"] = 1
#2010
data[data$Player =="Chauncey Billups" & data$Year == '2009-2010',"AS"] = 1
data[data$Player =="Pau Gasol" & data$Year == '2009-2010',"AS"] = 1
data[data$Player =="Al Horford" & data$Year == '2009-2010',"AS"] = 1
data[data$Player =="Dwight Howard" & data$Year == '2009-2010',"AS"] = 1
data[data$Player =="Joe Johnson" & data$Year == '2009-2010',"AS"] = 1
data[data$Player =="Chris Kaman" & data$Year == '2009-2010',"AS"] = 1
data[data$Player =="David Lee" & data$Year == '2009-2010',"AS"] = 1
data[data$Player =="Zach Randolph" & data$Year == '2009-2010',"AS"] = 1
data[data$Player =="Rajon Rondo" & data$Year == '2009-2010',"AS"] = 1
data[data$Player =="Derrick Rose" & data$Year == '2009-2010',"AS"] = 1
data[data$Player =="Brandon Roy" & data$Year == '2009-2010',"AS"] = 1
data[data$Player =="Amar'e Stoudemire" & data$Year == '2009-2010',"AS"] = 1
#2011
data[data$Player =="Pau Gasol" & data$Year == '20010-2011',"AS"] = 1
data[data$Player =="Manu Ginóbili*" & data$Year == '2010-2011',"AS"] = 1
data[data$Player =="Blake Griffin" & data$Year == '2010-2011',"AS"] = 1
data[data$Player =="Al Horford" & data$Year == '2010-2011',"AS"] = 1
data[data$Player =="Dwight Horford" & data$Year == '2010-2011',"AS"] = 1
data[data$Player =="Joe Johnson" & data$Year == '2010-2011',"AS"] = 1
data[data$Player =="Kevin Love" & data$Year == '2010-2011',"AS"] = 1
data[data$Player =="Rajon Rondo" & data$Year == '2010-2011',"AS"] = 1
data[data$Player =="Derrick Rondo" & data$Year == '2010-2011',"AS"] = 1
data[data$Player =="Amar'e Stoudemire" & data$Year == '2010-2011',"AS"] = 1
data[data$Player =="Russell Westbrook" & data$Year == '2010-2011',"AS"] = 1
#2012
data[data$Player =="LaMarcus Aldridge" & data$Year == '2011-2012',"AS"] = 1
data[data$Player =="Andrew Bynum" & data$Year == '2011-2012',"AS"] = 1
data[data$Player =="Luol Deng" & data$Year == '2011-2012',"AS"] = 1
data[data$Player =="Marc Gasol" & data$Year == '2011-2012',"AS"] = 1
data[data$Player =="Blake Griffin" & data$Year == '2011-2012',"AS"] = 1
data[data$Player =="Roy Hibbert" & data$Year == '2011-2012',"AS"] = 1
data[data$Player =="Dwight Howard" & data$Year == '2011-2012',"AS"] = 1
data[data$Player =="Andre Iguodala" & data$Year == '2011-2012',"AS"] = 1
data[data$Player =="Joe Johnson" & data$Year == '2011-2012',"AS"] = 1
data[data$Player =="Kevin Love" & data$Year == '2011-2012',"AS"] = 1
data[data$Player =="Tony Parker" & data$Year == '2011-2012',"AS"] = 1
data[data$Player =="Rajon Rondo" & data$Year == '2011-2012',"AS"] = 1
data[data$Player =="Derrick Rose" & data$Year == '2011-2012',"AS"] = 1
data[data$Player =="Deron Williams" & data$Year == '2011-2012',"AS"] = 1
#2013
data[data$Player =="LaMarcus Aldridge" & data$Year == '2012-2013',"AS"] = 1
data[data$Player =="Tyson Chandler" & data$Year == '2012-2013',"AS"] = 1
data[data$Player =="Luol Deng" & data$Year == '2012-2013',"AS"] = 1
data[data$Player =="LaMarcus Aldridge" & data$Year == '2012-2013',"AS"] = 1
data[data$Player =="Blake Griffin" & data$Year == '2012-2013',"AS"] = 1
data[data$Player =="Jrue Holiday" & data$Year == '2012-2013',"AS"] = 1
data[data$Player =="Dwight Howard" & data$Year == '2012-2013',"AS"] = 1
data[data$Player =="David Lee" & data$Year == '2012-2013',"AS"] = 1
data[data$Player =="Brook Lopez" & data$Year == '2012-2013',"AS"] = 1
data[data$Player =="Joakim Noah" & data$Year == '2012-2013',"AS"] = 1
data[data$Player =="Tony Parker" & data$Year == '2012-2013',"AS"] = 1
data[data$Player =="Zach Randolph" & data$Year == '2012-2013',"AS"] = 1
data[data$Player =="Rajon Rondo" & data$Year == '2012-2013',"AS"] = 1
#2014
data[data$Player =="LaMarcus Aldridge" & data$Year == '2013-2014',"AS"] = 1
data[data$Player =="DeMar DeRozan" & data$Year == '2013-2014',"AS"] = 1
data[data$Player =="Blake Griffin" & data$Year == '2013-2014',"AS"] = 1
data[data$Player =="Roy Hibbert" & data$Year == '2013-2014',"AS"] = 1
data[data$Player =="Dwight Howard" & data$Year == '2013-2014',"AS"] = 1
data[data$Player =="Joe Johnson" & data$Year == '2013-2014',"AS"] = 1
data[data$Player =="Kevin Love" & data$Year == '2013-2014',"AS"] = 1
data[data$Player =="Paul Millsap" & data$Year == '2013-2014',"AS"] = 1
data[data$Player =="Joakim Noah" & data$Year == '2013-2014',"AS"] = 1
data[data$Player =="Damian Lillard" & data$Year == '2013-2014',"AS"] = 1
data[data$Player =="Tony Parker" & data$Year == '2013-2014',"AS"] = 1
data[data$Player =="John Wall" & data$Year == '2013-2014',"AS"] = 1
#2015
data[data$Player =="LaMarcus Aldridge" & data$Year == '2014-2015',"AS"] = 1
data[data$Player =="DeMarcus Cousins" & data$Year == '2014-2015',"AS"] = 1
data[data$Player =="Marc Gasol" & data$Year == '2014-2015',"AS"] = 1
data[data$Player =="Pau Gasol" & data$Year == '2014-2015',"AS"] = 1
data[data$Player =="Blake Griffin" & data$Year == '2014-2015',"AS"] = 1
data[data$Player =="Al Horford" & data$Year == '2014-2015',"AS"] = 1
data[data$Player =="Kyle Korver" & data$Year == '2014-2015',"AS"] = 1
data[data$Player =="Damian Lillard" & data$Year == '2014-2015',"AS"] = 1
data[data$Player =="Kyle Lowry" & data$Year == '2014-2015',"AS"] = 1
data[data$Player =="Paul Millsap" & data$Year == '2014-2015',"AS"] = 1
data[data$Player =="Jeff Teague" & data$Year == '2014-2015',"AS"] = 1
data[data$Player =="Klay Thompson" & data$Year == '2014-2015',"AS"] = 1
#2016
data[data$Player =="LaMarcus Aldridge" & data$Year == '2015-2016',"AS"] = 1
data[data$Player =="DeMarcus Cousins" & data$Year == '2015-2016',"AS"] = 1
data[data$Player =="DeMar DeRozan" & data$Year == '2015-2016',"AS"] = 1
data[data$Player =="Andre Drummond" & data$Year == '2015-2016',"AS"] = 1
data[data$Player =="Pau Gasol" & data$Year == '2015-2016',"AS"] = 1
data[data$Player =="Draymond Green" & data$Year == '2015-2016',"AS"] = 1
data[data$Player =="Al Horford" & data$Year == '2015-2016',"AS"] = 1
data[data$Player =="LaMarcus Aldridge" & data$Year == '2015-2016',"AS"] = 1
data[data$Player =="Kawhi Leonard" & data$Year == '2015-2016',"AS"] = 1
data[data$Player =="Kyle Lowry" & data$Year == '2015-2016',"AS"] = 1
data[data$Player =="Paul Millsap" & data$Year == '2015-2016',"AS"] = 1
data[data$Player =="Isaiah Thomas" & data$Year == '2015-2016',"AS"] = 1
data[data$Player =="Klay Thompson" & data$Year == '2015-2016',"AS"] = 1
data[data$Player =="John Wall" & data$Year == '2015-2016',"AS"] = 1
#2017
data[data$Player =="DeMarcus Cousins" & data$Year == '2016-2017',"AS"] = 1
data[data$Player =="DeMar DeRozan" & data$Year == '2016-2017',"AS"] = 1
data[data$Player =="Marc Gasol" & data$Year == '2016-2017',"AS"] = 1
data[data$Player =="Draymond Green" & data$Year == '2016-2017',"AS"] = 1
data[data$Player =="Gordon Hayward" & data$Year == '2016-2017',"AS"] = 1
data[data$Player =="DeAndre Jordan" & data$Year == '2016-2017',"AS"] = 1
data[data$Player =="Kawhi Leonard" & data$Year == '2016-2017',"AS"] = 1
data[data$Player =="Kevin Love" & data$Year == '2016-2017',"AS"] = 1
data[data$Player =="Kyle Lowry" & data$Year == '2016-2017',"AS"] = 1
data[data$Player =="Paul Millsap" & data$Year == '2016-2017',"AS"] = 1
data[data$Player =="Isaiah Thomas" & data$Year == '2016-2017',"AS"] = 1
data[data$Player =="Klay Thompson" & data$Year == '2016-2017',"AS"] = 1
data[data$Player =="Kemba Walker" & data$Year == '2016-2017',"AS"] = 1
data[data$Player =="John Wall" & data$Year == '2016-2017',"AS"] = 1
#2018
data[data$Player =="LaMarcus Aldridge" & data$Year == '2017-2018',"AS"] = 1
data[data$Player =="Bradley Beal" & data$Year == '2017-2018',"AS"] = 1
data[data$Player =="DeMar DeRozan" & data$Year == '2017-2018',"AS"] = 1
data[data$Player =="Goran Dragic" & data$Year == '2017-2018',"AS"] = 1
data[data$Player =="Andre Drummond" & data$Year == '2017-2018',"AS"] = 1
data[data$Player =="Draymond Green" & data$Year == '2017-2018',"AS"] = 1
data[data$Player =="Damian Lillard" & data$Year == '2017-2018',"AS"] = 1
data[data$Player =="Kevin Love" & data$Year == '2017-2018',"AS"] = 1
data[data$Player =="Kyle Lowry" & data$Year == '2017-2018',"AS"] = 1
data[data$Player =="Victor Oladipo" & data$Year == '2017-2018',"AS"] = 1
data[data$Player =="Klay Thompson" & data$Year == '2017-2018',"AS"] = 1
data[data$Player =="Kemba Walker" & data$Year == '2017-2018',"AS"] = 1
data[data$Player =="John Wall" & data$Year == '2017-2018',"AS"] = 1
#2019
data[data$Player =="LaMarcus Aldridge" & data$Year == '2018-2019',"AS"] = 1
data[data$Player =="Bradley Beal" & data$Year == '2018-2019',"AS"] = 1
data[data$Player =="Blake Griffin" & data$Year == '2018-2019',"AS"] = 1
data[data$Player =="Kawhi Leonard" & data$Year == '2018-2019',"AS"] = 1
data[data$Player =="Damian Lillard" & data$Year == '2018-2019',"AS"] = 1
data[data$Player =="Kyle Lowry" & data$Year == '2018-2019',"AS"] = 1
data[data$Player =="Khris Middleton" & data$Year == '2018-2019',"AS"] = 1
data[data$Player =="Victor Oladipo" & data$Year == '2018-2019',"AS"] = 1
data[data$Player =="Klay Thompson" & data$Year == '2018-2019',"AS"] = 1
data[data$Player =="Nikola Vucevic" & data$Year == '2018-2019',"AS"] = 1
data[data$Player =="Kemba Walker" & data$Year == '2018-2019',"AS"] = 1
data[data$Player =="Nikola Jokic" & data$Year == '2018-2019',"AS"] = 1
data[data$Player =="Karl-Anthony Towns" & data$Year == '2018-2019',"AS"] = 1
#2020
data[data$Player =="Rudy Gobert" & data$Year == '2019-2020',"AS"] = 1
data[data$Player =="Kawhi Leonard" & data$Year == '2019-2020',"AS"] = 1
data[data$Player =="Damian Lillard" & data$Year == '2019-2020',"AS"] = 1
data[data$Player =="Kyle Lowry" & data$Year == '2019-2020',"AS"] = 1
data[data$Player =="Khris Walker" & data$Year == '2019-2020',"AS"] = 1
data[data$Player =="Kemba Walker" & data$Year == '2019-2020',"AS"] = 1
data[data$Player =="Nikola Jokic" & data$Year == '2019-2020',"AS"] = 1
data[data$Player =="Devin Booker" & data$Year == '2019-2020',"AS"] = 1
data[data$Player =="Ben Simmons" & data$Year == '2019-2020',"AS"] = 1
data[data$Player =="Brandon Ingram" & data$Year == '2019-2020',"AS"] = 1
data[data$Player =="Domantas Sabonis" & data$Year == '2019-2020',"AS"] = 1
data[data$Player =="Pascal Siakam" & data$Year == '2019-2020',"AS"] = 1
data[data$Player =="Jayson Tatum" & data$Year == '2019-2020',"AS"] = 1
data[data$Player =="Donovan Mitchell" & data$Year == '2019-2020',"AS"] = 1
data[data$Player =="Bam Adebayo" & data$Year == '2019-2020',"AS"] = 1
data[data$Player =="Luka Doncic" & data$Year == '2019-2020',"AS"] = 1
data[data$Player =="Trae Young" & data$Year == '2019-2020',"AS"] = 1
#2021
data[data$Player =="Bradley Beal" & data$Year == '2020-2021',"AS"] = 1
data[data$Player =="Mike Conley" & data$Year == '2020-2021',"AS"] = 1
data[data$Player =="Rudy Gobert" & data$Year == '2020-2021',"AS"] = 1
data[data$Player =="Kawhi Leonard" & data$Year == '2020-2021',"AS"] = 1
data[data$Player =="Damian Lillard" & data$Year == '2020-2021',"AS"] = 1
data[data$Player =="Nikola Vucevic" & data$Year == '2020-2021',"AS"] = 1
data[data$Player =="Devin Booker" & data$Year == '2020-2021',"AS"] = 1
data[data$Player =="Ben Simmons" & data$Year == '2020-2021',"AS"] = 1
data[data$Player =="Jaylen Brown" & data$Year == '2020-2021',"AS"] = 1
data[data$Player =="Domantas Sabonis" & data$Year == '2020-2021',"AS"] = 1
data[data$Player =="Jayson Tatum" & data$Year == '2020-2021',"AS"] = 1
data[data$Player =="Donovan Mitchell" & data$Year == '2020-2021',"AS"] = 1
#2022
data[data$Player =="DeMar DeRozan" & data$Year == '2021-2022',"AS"] = 1
data[data$Player =="Rudy Gobert" & data$Year == '2021-2022',"AS"] = 1
data[data$Player =="Draymond Green" & data$Year == '2021-2022',"AS"] = 1
data[data$Player =="Khris Middleton" & data$Year == '2021-2022',"AS"] = 1
data[data$Player =="Andrew Wiggins" & data$Year == '2021-2022',"AS"] = 1
data[data$Player =="Zach LaVine" & data$Year == '2021-2022',"AS"] = 1
data[data$Player =="Nikola Jokic" & data$Year == '2021-2022',"AS"] = 1
data[data$Player =="Karl-Anthony Towns" & data$Year == '2021-2022',"AS"] = 1
data[data$Player =="Devin Booker" & data$Year == '2021-2022',"AS"] = 1
data[data$Player =="Dejounte Murray" & data$Year == '2021-2022',"AS"] = 1
data[data$Player =="Fred VanVleet" & data$Year == '2021-2022',"AS"] = 1
data[data$Player =="Jayson Tatum" & data$Year == '2021-2022',"AS"] = 1
data[data$Player =="Donovan Mitchell" & data$Year == '2021-2022',"AS"] = 1
data[data$Player =="Jarrett Allen" & data$Year == '2021-2022',"AS"] = 1
data[data$Player =="Luka Doncic" & data$Year == '2021-2022',"AS"] = 1
data[data$Player =="Trae Young" & data$Year == '2021-2022',"AS"] = 1
data[data$Player =="Ja Morant" & data$Year == '2021-2022',"AS"] = 1
```

```{r, echo = FALSE}
vis_miss(data)

data$`eFG%`[is.na(data$`eFG%`)]<- mean(data$`eFG%`,na.rm=TRUE)
data$`FT%`[is.na(data$`FT%`)]<- mean(data$`FT%`,na.rm=TRUE)
data$`3P%`[is.na(data$`3P%`)]<- mean(data$`3P%`,na.rm=TRUE)
data$`3P%`[is.na(data$`3P%`)]<- mean(data$`3P%`,na.rm=TRUE)
data$`FG%`[is.na(data$`FG%`)]<- mean(data$`FG%`,na.rm=TRUE)
data$`2P%`[is.na(data$`2P%`)]<- mean(data$`2P%`,na.rm=TRUE)
```

### The dataset is ready to be used
saveRDS(data, file = "all_stars.rds")

