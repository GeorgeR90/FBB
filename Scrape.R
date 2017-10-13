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
rank.espn.live.position.html <- html_nodes(webpage, 'td:nth-child(3)') ## primary position
rank.espn.live.players <- html_text(rank.espn.live.players.html)  ##Parse players
rank.espn.live.price <- html_text(rank.espn.live.price.html)  ##Parse cost
rank.espn.live.position <- html_text(rank.espn.live.position.html)[-(1:2)] #Parse positions, dropping some clutter at the start
rank.espn.live <- as.data.frame(cbind(rank.espn.live.players,rank.espn.live.price, rank.espn.live.position)) ##Combine to one dataframe
names(rank.espn.live) <- c('player','price','prim.pos') #Rename columns
rank.espn.live$draft <- 1:nrow(rank.espn.live)

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
  rating.espn.live.player.long.html <- html_nodes(webpage, '.playertablePlayerName') ## long name has multiple positions
  rating.espn.live.table.html <- html_nodes(webpage,'.playertableData')  ##Table
  rating.espn.live.player.temp <- html_text(rating.espn.live.player.html) #parse players
  rating.espn.live.table.temp <- html_text(rating.espn.live.table.html) #parse table
  rating.espn.live.player.long.temp <- html_text(rating.espn.live.player.long.html)  # long name parse
  rating.espn.live.table.temp.df <- as.data.frame(matrix(rating.espn.live.table.temp[-(1:10)], ncol = 10, byrow = T)) #convert table to dataframe
  
  if(i == 1){
    rating.espn.live.player <- rating.espn.live.player.temp
    rating.espn.live.table <- rating.espn.live.table.temp.df
    rating.espn.live.player.long <- rating.espn.live.player.long.temp
  }
  else {
    rating.espn.live.player <- append(rating.espn.live.player,rating.espn.live.player.temp)
    rating.espn.live.table <- rbind(rating.espn.live.table,rating.espn.live.table.temp.df)
    rating.espn.live.player.long <- append(rating.espn.live.player.long,rating.espn.live.player.long.temp)
    }
}

rating.espn.live <- as.data.frame(cbind(rating.espn.live.player, rating.espn.live.player.long, rating.espn.live.table))
names(rating.espn.live) <- c('player','player.long','rank','fg.r','ft.r','3pm.r','reb.r','ast.r','stl.r','blk.r','pts.r','ovr.r')  #rename headers

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

##This loop mirrors the one above it for ratings
for(i in 1:length(url)){
  webpage <- read_html(url[i])
  projection.espn.live.player.html <- html_nodes(webpage, '.flexpop:nth-child(1)')
  projection.espn.live.table.html <- html_nodes(webpage,'.playertableStat')
  projection.espn.live.player.temp <- html_text(projection.espn.live.player.html)
  projection.espn.live.table.temp <- html_text(projection.espn.live.table.html)
  projection.espn.live.table.temp.df <- as.data.frame(matrix(projection.espn.live.table.temp[-(1:8)], ncol = 8, byrow = T))
  
  if(i == 1){
    projection.espn.live.player <- projection.espn.live.player.temp
    projection.espn.live.table <- projection.espn.live.table.temp.df
  }
  else {
    projection.espn.live.player <- append(projection.espn.live.player,projection.espn.live.player.temp)
    projection.espn.live.table <- rbind(projection.espn.live.table,projection.espn.live.table.temp.df)
  }
}

projection.espn.live <- as.data.frame(cbind(projection.espn.live.player, projection.espn.live.table))
names(projection.espn.live) <- c('player','fg.p','ft.p','3pm.p','reb.p','ast.p','stl.p','blk.p','pts.p')  #rename headers
keep.list <- append(keep.list,o2s(projection.espn.live))  ##List for variables to not remove
rm(list=setdiff(ls(), keep.list))  #Empty work space



#Yahoo Fantasy
url <- c('https://basketball.fantasysports.yahoo.com/nba/draftanalysis?tab=AD&pos=ALL&sort=DA_AP',
         'https://basketball.fantasysports.yahoo.com/nba/draftanalysis?tab=AD&pos=ALL&sort=DA_AP&count=50',
         'https://basketball.fantasysports.yahoo.com/nba/draftanalysis?tab=AD&pos=ALL&sort=DA_AP&count=100',
         'https://basketball.fantasysports.yahoo.com/nba/draftanalysis?tab=AD&pos=ALL&sort=DA_AP&count=150',
         'https://basketball.fantasysports.yahoo.com/nba/draftanalysis?tab=AD&pos=ALL&sort=DA_AP&count=200') ##50 players per page, looking at top 250 only


##This loop mirrors the one above it for ratings
for(i in 1:length(url)){
  webpage <- read_html(url[i])
  cost.yahoo.player.html <- html_nodes(webpage, '.F-link')
  cost.yahoo.value.html <- html_nodes(webpage,'.Fz-xs+ td div')
  cost.yahoo.cost.html <- html_nodes(webpage,'td.Last')
  cost.yahoo.player.temp <- html_text(cost.yahoo.player.html)
  cost.yahoo.value.temp <- html_text(cost.yahoo.value.html)
  cost.yahoo.cost.temp <- html_text(cost.yahoo.cost.html)
  
  if(i == 1){
    cost.yahoo.player <- cost.yahoo.player.temp
    cost.yahoo.value <- cost.yahoo.value.temp
    cost.yahoo.cost <- cost.yahoo.cost.temp
  }
  else {
    cost.yahoo.player <- append(cost.yahoo.player, cost.yahoo.player.temp)
    cost.yahoo.value <- append(cost.yahoo.value, cost.yahoo.value.temp)
    cost.yahoo.cost <- append(cost.yahoo.cost, cost.yahoo.cost.temp)
  }
}


cost.yahoo <- as.data.frame(cbind(cost.yahoo.player,cost.yahoo.value,cost.yahoo.cost))
names(cost.yahoo) <- c('player','value.y','cost.y')
keep.list <- append(keep.list,o2s(cost.yahoo))  ##List for variables to not remove
rm(list=setdiff(ls(), keep.list))  #Empty work space

##FantasyPros (For Tiers)
fp.tiers.postions <- c('Overall','PG','SG','SF','PF','C')
fp.tier.size <-c(250,150,150,150,150,100)
url <- c('https://www.fantasypros.com/nba/rankings/overall',
         'https://www.fantasypros.com/nba/rankings/pg',
         'https://www.fantasypros.com/nba/rankings/sg',
         'https://www.fantasypros.com/nba/rankings/sf',
         'https://www.fantasypros.com/nba/rankings/pf',
         'https://www.fantasypros.com/nba/rankings/c')

ff.tier.list = list()

for(i in 1:length(url)){
  webpage <- read_html(url[i])
  tiers.fp.player.html <- html_nodes(webpage, '.player-name')
  tiers.fp.avg.html <- html_nodes(webpage, 'td:nth-child(5)')
  tiers.fp.sd.html <- html_nodes(webpage, 'td:nth-child(6)')
  
  tiers.fp.player <- as.character(html_text(tiers.fp.player.html)[1:fp.tier.size[i]])
  tiers.fp.avg <- as.numeric(as.character(html_text(tiers.fp.avg.html)[1:fp.tier.size[i]]))
  tiers.fp.sd <- as.numeric(as.character(html_text(tiers.fp.sd.html)[1:fp.tier.size[i]]))
  
  
  tiers.df.temp <- data.frame(cbind(tiers.fp.player,tiers.fp.avg,tiers.fp.sd), stringsAsFactors = F) ##combines to one DF
  tiers.df.temp[,2:3] <- sapply(tiers.df.temp[,2:3], as.numeric) ##converts numbersto numeric
  tiers.df.temp <- tiers.df.temp[complete.cases(tiers.df.temp),] ##complete cases only
  ff.tier.list[[fp.tiers.postions[i]]] <- tiers.df.temp ##output
}


keep.list <- append(keep.list,o2s(ff.tier.list))  ##List for variables to not remove
rm(list=setdiff(ls(), keep.list))  #Empty work space



## cost.yahoo DONE BELOW
## ff.tier.list DONE IN PULL
## projection.espn.live DONE
## rank.espn.live DONE
## rating.espn.live 


##Wrangling to be put in separate file later

cost.yahoo <- as.data.frame(sapply(cost.yahoo, as.character))  ##Convert from Factor to character (factors hard to work with)
cost.yahoo$value.y <- as.numeric(gsub('\\$', '', cost.yahoo$value.y))
cost.yahoo$cost.y <- as.numeric(gsub('\\$', '', cost.yahoo$cost.y))

projection.espn.live$player <- as.character(projection.espn.live$player)
projection.espn.live[,2:9] <- sapply(projection.espn.live[,2:9], function(x){ as.numeric(as.character(x))})

rank.espn.live$player <- as.character(rank.espn.live$player)
rank.espn.live$price <- as.numeric(as.character(rank.espn.live$price))
rank.espn.live$prim.pos <- as.character(rank.espn.live$prim.pos)

rating.espn.live <- as.data.frame(sapply(rating.espn.live, as.character), stringsAsFactors = F)
rating.espn.live[,3:12] <- sapply(rating.espn.live[,3:12], as.numeric)

rating.espn.live$temp <- unlist(regmatches(rating.espn.live$player.long, regexpr(", ", rating.espn.live$player.long), 
                                           invert = TRUE))[seq(2, nrow(rating.espn.live)*2, by =2)]
rating.espn.live$temp <- substr(rating.espn.live$temp,4, stop = 1000000)

rating.espn.live$pos.pg <-  grepl('PG', rating.espn.live$temp)
rating.espn.live$pos.sg <-  grepl('SG', rating.espn.live$temp)
rating.espn.live$pos.sf <-  grepl('SF', rating.espn.live$temp)
rating.espn.live$pos.pf <-  grepl('PF', rating.espn.live$temp)
rating.espn.live$pos.c <-  grepl('C', rating.espn.live$temp)

