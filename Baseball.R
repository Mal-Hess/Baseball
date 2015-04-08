createplayer <- function(playerfirstname, playerlastname, key=1){
  
  require(XML)
  
  require(RCurl)
  
  #check if key is valid entry
  
  if (class(key)!= "numeric"){
    
    stop("Invalid key: Requires number 1-9")
    
    stop}
  
  if (key > 9 | key < 1){
    
    stop("Invalid key: Requires number 1-9")
    
    stop}
  
  #keys are always two digit, so if less than 10 it makes
  
  #key to a string and adds a zero to the front
  
  if (key < 10){
    
    key <- substring(toString(key), 1, 1)
    
    key <- paste("0", key, sep="")
    
  }
  
  #checks to see if Baseball directory exists, and if not creates it.
  
  if(!file.exists("Baseball")){dir.create("/Baseball") }
  originalwd <- getwd()
  setwd("Baseball")
  
  playerfirstname <- as.character(playerfirstname)
  
  playerlastname <- as.character(playerlastname)
  
  #cleaning names and key to make player identity object
  
  subfirst <- substring(playerfirstname, 1, 2)
  
  sublast <- substring(playerlastname, 1, 5)
  
  identity <- paste(sublast, subfirst, key, sep="")
  
  identity <- tolower(identity)
  
  #checks to see if player already exists in local database
  
  filename <- paste0(identity, ".csv")
  
  if(file.exists(filename)){
    
    stop("Player already exists in database")
    
  }
  
  #making url to get to base page for the specified player
  
  url <- paste0("http://www.baseball-reference.com/players/gl.cgi?id=", identity)
  
  raw <- getURL(url)
  
  data <- htmlParse(raw)
  
  #making a list of all the years that this player has played in

  xpath <- "//*[@id='stats_sub_index']/ul/li[4]/ul/li/a"
  
  nodes <- getNodeSet(data, xpath)
  
  years <- sapply(nodes, xmlValue)
  
  #cleaning up the list of years, need to remove postseason and turn characters to numbers
  
  years <- years[!is.element(years, "Postseason")]
  
  years <- as.numeric(years)
  
  years<-sort(years)
  
  amountofyears <- length(years)
  
  getyeardata <- function(ident = identity, year=2014){
    
    #setting up URL to get data from a specific year
    
    url1<- "http://www.baseball-reference.com/players/gl.cgi?id="
    
    url2<- "&t=b&year="
    
    urlyear <- paste(url1, identity, url2, year, sep="")
    
    #downloading html site and taking out the table with the batting data
    
    html <- htmlTreeParse(urlyear, useInternal=TRUE)
    
    tables <- readHTMLTable(html)
    
    batlog<- tables$batting_gamelogs
    
    rows<- nrow(batlog)
    
    i<-1
    
    while(i<=rows){ #removes Month rows
      
      if(batlog[i,1]=="April" | batlog[i,1]=="May" | batlog[i,1]=="June"|
           
           batlog[i,1]=="July"| batlog[i,1]=="August"| batlog[i,1]=="September"|
           
           batlog[i,1]=="October"){
        
        batlog <- batlog[-i,]
        
        i <- i - 1
        
        rows <- nrow(batlog)
        
      }
      
      i <- i + 1
      
    }
    
    #adding a column to the front of the data that has identity and year on it
    
    #remove first column which is just row number, imported from html table.
    
    batlog <- batlog[,-1]
    
    batlog <- transform(batlog, Player=identity)
    
    batlog <- transform(batlog, Year=year)
    
    temp<- batlog[,36:37]
    
    batlog <- batlog[,-36:-37]
    
    batlog <- cbind(temp,batlog)
    
    #more data cleanup. Var.5 is currently the home/away column, away games signified with @
    
    #Date variable transformed to character so I can clean up the dates and later and a year to it.
    
    rows<- nrow(batlog)
    
    batlog <- transform(batlog, Var.5= as.character(Var.5), Date=as.character(Date), Gtm = as.character(Gtm))
    
    #double headers have extra symbols on them after the date, I need to
    
    #remove (1) or (2) from them to properly transform it to a date class.
    
    #Gtm will also have extra () based on amount of games a player missed
    
    #I am also adding the year to the end of the date.
    
    i <-1
    
    while(i<=rows){
      
      if(grepl("\\)", batlog$Date[i])) {
        
        nc <- nchar(batlog$Date[i])
        
        batlog$Date[i]<- substring(batlog$Date[i], 1, (nc-4))  
      }
        
        batlog$Date[i]<- paste(batlog$Date[i], year, sep=", ")
      
      i <- i +1 }
    
    batlog<- subset(batlog, Gcar != "Tm")
    
    batlog<- subset(batlog, H != "HR")
    
    #more data cleaning
    
    colnames(batlog)[7] <- "Home"
    
    batlog <- transform(batlog, DELTAAVG= NA, BA = as.numeric(as.character(BA)), Home = as.character(Home))
    
    #One K loop to do two things.  First, deal with issues of home/away, second create deltaavg.
    
    #adds "H" (symbolize home game) to blank entires, away games are "@" symbol
    
    k<-1
    
    while(k<=nrow(batlog)){
      
      if(batlog[k,7]!= "@"){
        
        batlog[k,7] <- "H"
        
      }
      
      ##Makes new variable (deltaavg) that is the difference of Batting average from day to day
      
      if((k+1) <= nrow(batlog)){
        batlog$DELTAAVG[(k+1)]<-(batlog$BA[(k+1)] - batlog$BA[k])
      }
      k<-k+1
    } #end of k while loop
    batlog
  }#end of getyeardata
  
  
  #initializing object (careerdata) which will become the main dataframe
  
  careerdata <- NULL
  
  j<-1
  
  #Go through all years and rbind the data together into the careerdata object
  
  while (j <= amountofyears){
    
    b<- getyeardata(identity, year = years[j])
    
    careerdata <- rbind(careerdata, b)
    
    j<-j+1
    
  }
  
  filename <- paste0(identity, ".csv")
  
  write.csv(careerdata, file= filename)
  
  setwd(originalwd)
  
} #End of create player function!

loadplayer <- function (playerfirstname, playerlastname, key=1){
  
  originalwd <- getwd()
  setwd("Baseball")
  
  if (key < 10){
    
    key <- substring(toString(key), 1, 1)
    
    key <- paste("0", key, sep="")
    
  }
  
  playerfirstname <- as.character(playerfirstname)
  
  playerlastname <- as.character(playerlastname)
  
  #cleaning names and key to make player identity object
  
  subfirst <- substring(playerfirstname, 1, 2)
  
  sublast <- substring(playerlastname, 1, 5)
  
  identity <- paste(sublast, subfirst, key, sep="")
  
  identity <- tolower(identity)
  
  filename <- paste0(identity, ".csv")
  
  dataframe <- read.csv(filename, header=TRUE)
  setwd("../")
  
  dataframe <- dataframe[,-1]
  dataframe
  
}

source('calendarheat.R')

currentplayer <- loadplayer("josh", "hamilton", 3)

simple <- transform(currentplayer, date = as.Date(Date, format = "%b %d, %Y"), h= as.numeric(H), Date=as.character(Date))

sub1 <- subset(simple, format(date, "%Y") %in% c("2012"))

calendarHeat(sub1$date,sub1$h, date.form = "%b %d, %Y" )