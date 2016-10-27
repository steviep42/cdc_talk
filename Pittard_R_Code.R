
# Pittard, Steve Code in Support of CDC Talk for R Users Group
#
# Packages Required for this code to run include: 
#
# ggplot2, dplyr, readr, leaflet, googleVis
#
# Note that example that uses gunzip might have to be adjusted to use
# the MS Windows equivalent to gunzip.

library(ggplot2)
library(dplyr)
library(leaflet)
library(readr)
library(googleVis)

str(mtcars)

sapply(mtcars, function(x) {length(unique(x))})

mtcars$am <- factor(mtcars$am,labels=c("Auto","Manual"))

boxplot(mpg~am,data=mtcars,col=c(2,4),main="MPG by Transmission Type")

mtcars$cyl <- factor(mtcars$cyl)
boxplot(mpg~cyl,data=mtcars,
        col=c("orange","red","green"),
        main="MPG by Cylinders")
grid()

# Since we don't have much data in each group we will also plot
# the MPG points for each category

mydf <- split(mtcars,mtcars$cyl)
lapply(1:3,function(x) {
  points(rep(x,length(mydf[[x]]$mpg)),
         mydf[[x]]$mpg,
         pch=18)  
})


# Draw a histogram. We don't have many values it's still helpful

hist(mtcars$mpg,col="aquamarine",main="MPG Histogram",xlab="MPG")
grid()

# Make a scatterplot of all MPG and provide each cylinder
# group with its own color

plot(mpg~wt,data=mtcars,type="n",main="MPG vs. Weight",
     xlab="Weight in Lbs/1,000")

# Split the data frame by cylinder value and create
# a color vector

mys <- split(mtcars,mtcars$cyl)
colors <- rainbow(3)

# Loop through the split data frame and put up points for each
# cylinder group

lapply(seq_along(mys),function(x) {
  points(mys[[x]]$wt,
         mys[[x]]$mpg,
         col=colors[x],pch=19)
})

# Draw a legend
legend("topright",legend=sort(unique(mtcars$cyl)),pch=19,col=colors)
grid()

set.seed(123)  # Make Example Reproducible

# Let's flip a coin 1,000 times

coinsamp <- sample(c('H','T'),1000,T)

# Create a count of occurences 

table(coinsamp)  

# Is the resulting distribution reasonable ?

chisq.test(table(coinsamp),p=c(0.5,0.5))

# How many 4,6, or 8 cylinder cars are there for each transmission type ? 

table(Transmission=mtcars$am,Cylinders=mtcars$cyl)


prop.table(table(Transmission=mtcars$am,Cylinders=mtcars$cyl))

addmargins(prop.table(table(Transmission=mtcars$am,Cylinders=mtcars$cyl)))

# How many 4,6, or 8 cylinder cars are there for each transmission type ? 

myt <- table(Transmission=mtcars$am,Cylinders=mtcars$cyl)

plot(myt,color=rainbow(3),main="Transmissions vs Cylinders")

xtabs(~cyl,data=mtcars)

xtabs(~am+cyl,data=mtcars)

# We can subset out data

xtabs(~am+cyl,data=mtcars,subset=mpg < 25)

tapply(mtcars$mpg,mtcars$am,mean)

# For each cylinder group

tapply(mtcars$mpg,mtcars$cyl,mean)


# We can supply our own function

tapply(mtcars$mpg,mtcars$am,function(x) return(c(mean=mean(x),sd=sd(x))))


tapply(mtcars$mpg, list(mtcars$am, mtcars$vs), mean)


# But this does not. We cannot summarize multiple continuous quantities
# across categories

tapply(list(mtcars$mpg,mtcars$hp), list(mtcars$am, mtcars$vs), mean)    

# Here we summarize two continuous values in terms of two categories
                
aggregate(cbind(mpg,hp)~am+vs,data=mtcars,mean)
               
# We summarize mpg in terms of two categories
                
aggregate(mpg~cyl+am,data=mtcars,mean)
               
                
aggregate(mpg~cyl+am,data=mtcars,mean,subset=mpg>20)
               
                
aggregate(mpg~cyl,data=mtcars,range,subset=wt>mean(wt))
                
# dplyr

library(dplyr)

df <- data.frame(id = 1:5,
                gender = c("MALE","MALE","FEMALE","MALE","FEMALE"),
                age = c(70,76,60,64,68))
  
                
          
filter(df,gender == "FEMALE")
                
filter(df, id %in% c(1,3,5))
               
mutate(df,meanage = mean(age))
                 
mutate(df,old_young=ifelse(df$age>=mean(df$age),"Y","N"))
               
tmp <- mutate(df, color = ifelse(age > mean(age),"red","blue"))
plot(tmp$age,col=tmp$color,type="p",pch=19,main="Ages",ylab="Age")
grid()
abline(h=mean(tmp$age),lty=2)
legend("topright",c("Above Avg","Below Avg"),col=c("red","blue"),pch=19)
               
                
arrange(df, desc(age))
                
arrange(df, gender,desc(age))
                
                
select(df,gender,id,age)  # Reorder the columns
                
select(df,-age)   # Select all but the age column
                
select(df,id:age)  # Can use : to select a range
               
df
              
group_by(df,gender)   # Hmm. Did this really do anything ? 
                
                
gdf <- group_by(df,gender)    # Hmm. Did this really do anything ? 
                
               
summarize(group_by(df,gender),total=n())
               
summarize(group_by(df,gender),av_age=mean(age))
                
summarize(group_by(df,gender),av_age=mean(age),total=n())
                
head(select(mtcars, mpg, am))
               
mtcars %>% select(mpg, am) %>% head
                
df %>% group_by(gender) %>% summarize(avg=mean(age)) 
               
df %>% group_by(gender) %>% summarize(avg=mean(age),total=n()) 
                
df %>% filter(gender == "MALE") %>% summarize(med_age=median(age))
                
mtcars %>% filter(wt > 3.3)  %>% 
                mutate(ab_be=ifelse(mpg > mean(mpg),"Y","N")  ) %>% 
                group_by(ab_be) %>% summarize(mean_mpg=mean(mpg))
                
               
mtcars %>% filter(wt > 3.3)
                
               
mtcars %>% filter(wt > 3.3) %>% 
                  mutate(ab_be=ifelse(mpg > mean(mpg),"Y","N")  )
                
                
mtcars %>% filter(wt > 3.3)  %>% 
                  mutate(ab_be=ifelse(mpg > mean(mpg),"Y","N")  ) %>% 
                  group_by(ab_be) %>% summarize(mean_mpg=mean(mpg))
                
library(ggplot2)
mtcars %>% filter(wt > 3.3)  %>% 
                  mutate(ab_be=ifelse(mpg > mean(mpg),"Y","N")  ) %>% 
                  group_by(ab_be) %>% summarize(mean_mpg=mean(mpg)) %>%
                  ggplot(aes(x=ab_be,y=mean_mpg)) + geom_bar(stat="identity") +
                  ggtitle("Mean MPG") + labs(x = "ab_be", y = "Mean MPG")
               

# Note - the following is OS specific although Windows users should be able
# to unzip the file without any additional software

library(readr)
url <- "http://steviep42.bitbucket.org/YOUTUBE.DIR/combined_wiki.zip"
download.file(url,"combined_wiki.zip")
system("unzip combined_wiki.zip")
dt <- read_delim("combined_wiki.zip",delim=" ")
                
nrow(dt)
                
head(dt,5)
                
dt %>% mutate(MB=bytes/1000000) %>% 
      group_by(proj)%>% 
      summarize(avg=round(mean(MB),2)) %>% 
      arrange(desc(avg))
                
system.time( dt %>% mutate(MB=bytes/1000000) %>%
                group_by(proj)%>%
                summarize(avg=round(mean(MB),2)) %>%
                arrange(desc(avg)) )
                
               
url <- "http://steviep42.bitbucket.org/YOUTUBE.DIR/station.csv"
stations <- read_csv(url)
                
# Read in the trip date - you might get some messages about missing zipcodes
# but don't worry if you do

url <- "http://steviep42.bitbucket.org/YOUTUBE.DIR/trip.csv"                
trips <- read_csv(url)
                
str(stations)
                
str(trips)
            
trips %>% select(bike_id) %>% distinct()

# But if we wanted a single result we could do this
                
trips %>% select(bike_id) %>% distinct() %>% nrow()
            
# How many times was each bike used ? 
                
trips %>% group_by(bike_id) %>% 
          summarize(times_used=n()) %>% 
          arrange(desc(times_used))
                
# How many times was each bike used ? 
                
trips %>% count(bike_id,sort=TRUE)
                
# How many cities use this service ? How many stattions per city are there ?  
               
stations %>% count(city)
                
# We could also sort the result from highest count to lowest
                
stations %>% count(city,sort=TRUE)
                
                
library(leaflet)
m <- leaflet() %>% addTiles() %>% 
                  addMarkers(lng=stations$long,
                             lat=stations$lat,
                             popup=stations$name,
                             clusterOptions = markerClusterOptions())
                
m

#

trips %>% filter(substr(start_date,1,10) 
                        != substr(end_date,1,10)) %>% 
                        summarize(different_days=n())
                
# How many trips were there for each year ?
                
trips %>% count(substr(start_date,1,4))
                
library(googleVis)
                
trips %>% mutate(start_date=as.Date(start_date), 
                                 end_date=as.Date(end_date)) %>%
                  filter(start_date == end_date) %>% 
                  count(start_date) -> tripdates
                
# Create a Gvisplot and then plot it
                
plot( 
     gvisCalendar(data=tripdates, datevar="start_date", numvar="n",
                               options=list(
                                 title="Calendar Heat Map of Open Bike Trips",
                                 calendar="{cellSize:10,
                                 yearLabel:{fontSize:20, color:'#444444'},
                                 focusedCellColor:{stroke:'red'}}",
                                 width=590, height=320),
                               chartid="Calendar")
                )
      