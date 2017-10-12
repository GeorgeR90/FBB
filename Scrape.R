##Packages
require(rvest)

##Object to string function for easy saving
o2s <- function(v1) {
  deparse(substitute(v1))
}

##ESPN Live Scrape Value
url <- 'https://games.espn.com/fba/livedraftresults'
webpage <- read_html(url)
rank.espn.live.players.html <- html_nodes(webpage, '.flexpop')  ## Read players
rank.espn.live.price.html <- html_nodes(webpage, '.tableSubHead~ tr+ tr td:nth-child(6)')  ##Read cost
rank.espn.live.players <- html_text(rank.espn.live.players.html)  ##Parse players
rank.espn.live.price <- html_text(rank.espn.live.price.html)  ##Parse cost
rank.espn.live <- cbind(rank.espn.live.players,rank.espn.live.price) ##Combine to one dataframe
names(rank.espn.live) <- c('player','price') #Rename columns

keep.list <- c('keep.list','o2s',o2s(rank.espn.live))  ##List for variables to not remove
rm(list=setdiff(ls(), keep.list))  #Empty work space

##ESPN Live Scrape Rating
url <- c('https://games.espn.com/fba/playerrater',
         'https://games.espn.com/fba/playerrater?startIndex=50',
         'https://games.espn.com/fba/playerrater?startIndex=100',
         'https://games.espn.com/fba/playerrater?startIndex=150',
         'https://games.espn.com/fba/playerrater?startIndex=200',
         'https://games.espn.com/fba/playerrater?startIndex=250')       ##50 players per page, looking at top 300 only

for(i in 1:length(url)){
  webpage <- read_html(url[i])
  rating.espn.live.player.html <- html_nodes(webpage, '.flexpop:nth-child(1)')  ##Players
  rating.espn.live.table.html <- html_nodes(webpage,'.playertableData')  ##Table
  rating.espn.live.player.temp <- html_text(rating.espn.live.player.html) #parse players
  rating.espn.live.table.temp <- html_text(rating.espn.live.table.html) #parse table
  rating.espn.live.table.temp.df <- as.data.frame(matrix(rating.espn.live.table.temp[-(1:10)], ncol = 10, byrow = T)) #convert table to dataframe
  
  if(i == 1){
    rating.espn.live.player <- rating.espn.live.player.temp
    rating.espn.live.table <- rating.espn.live.table.temp.df
  }
  else {
    rating.espn.live.player <- append(rating.espn.live.player,rating.espn.live.player.temp)
    rating.espn.live.table <- rbind(rating.espn.live.table,rating.espn.live.table.temp.df)
    }
}

rating.espn.live <- as.data.frame(cbind(rating.espn.live.player, rating.espn.live.table))
names(rating.espn.live) <- c('player','rank','fg','ft','3pm','reb','ast','stl','blk','pts','ovr')  #rename headers

keep.list <- append(keep.list,o2s(rating.espn.live))  ##List for variables to not remove
rm(list=setdiff(ls(), keep.list))  #Empty work space

##ESPN Projections
url <- c('https://games.espn.com/fba/tools/projections',
         'http://games.espn.com/fba/tools/projections?startIndex=40',
         'http://games.espn.com/fba/tools/projections?startIndex=80',
         'http://games.espn.com/fba/tools/projections?startIndex=120',
         'http://games.espn.com/fba/tools/projections?startIndex=160',
         'http://games.espn.com/fba/tools/projections?startIndex=200',
         'http://games.espn.com/fba/tools/projections?startIndex=240',
         'http://games.espn.com/fba/tools/projections?startIndex=280') ##40 players per page, looking at top 320 only


for(i in 1:length(url)){
  webpage <- read_html(url[i])
  projection.espn.live.player.html <- html_nodes(webpage, '.flexpop:nth-child(1)')
  projection.espn.live.table.html <- html_nodes(webpage,'.playertableStat')
  projection.espn.live.player.temp <- html_text(projection.espn.live.player.html)
  projection.espn.live.table.temp <- html_text(rating.espn.live.table.html)
  rating.espn.live.table.temp.df <- as.data.frame(matrix(rating.espn.live.table.temp[-(1:10)], ncol = 10, byrow = T))
  
  if(i == 1){
    rating.espn.live.player <- rating.espn.live.player.temp
    rating.espn.live.table <- rating.espn.live.table.temp.df
  }
  else {
    rating.espn.live.player <- append(rating.espn.live.player,rating.espn.live.player.temp)
    rating.espn.live.table <- rbind(rating.espn.live.table,rating.espn.live.table.temp.df)
  }
}

