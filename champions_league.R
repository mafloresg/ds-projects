Sys.setlocale("LC_ALL","en_GB.UTF-8");
#install.packages("engsoccerdata")
#install.packages("googleVis")
#install.packages("rvest")
#install.packages("colorspace")
library("engsoccerdata");
library("dplyr");
library("rvest");
library("countrycode");
library("treemap");
library("colorspace");
library("ggplot2");
library("googleVis");
library("circlize");

# Arranging the data
ch <- champs

## A couple of fixes for typos in the data
unique(champs$hcountry)
champs[is.na(champs$hcountry) | is.na(champs$vcountry),c("home","hcountry", "visitor", "vcountry")]
ch[is.na(ch$hcountry),]$hcountry <- 'SCO'
ch[is.na(ch$vcountry),]$vcountry <- 'SCO'

ch[ch$home == 'Vorwarts Berlin' | ch$visitor == 'Vorwarts Berlin' |
       ch$home == 'ASK Vorwarts Berlin' | ch$visitor == 'ASK Vorwarts Berlin',
   c("Season", "home", "visitor")]

champs[champs$home == 'Vorwarts Berlin' | champs$visitor == 'Vorwarts Berlin' |
           champs$home == 'ASK Vorwarts Berlin' | champs$visitor == 'ASK Vorwarts Berlin',
       c("Season", "home", "visitor")]

ch[ch$home == 'Vorwarts Berlin',]$home <- "ASK Vorwarts Berlin"
ch[ch$visitor == 'Vorwarts Berlin',]$visitor <- "ASK Vorwarts Berlin"

## Federation list and country list (with UK instead of Eng, Wal, Sco, etc)
fedList <- as.data.frame(cbind(code = unique(ch$hcountry),
                               name = countrycode(unique(ch$hcountry),
                                                  'ioc',
                                                  'country.name'),
                               region = countrycode(unique(ch$hcountry),
                                                    'ioc',
                                                    'region')),
                         stringsAsFactors = FALSE)

fedList[fedList$code == 'SCO',]$name <- 'Scotland'
fedList[fedList$code == 'ENG',]$name <- 'England'
fedList[fedList$code == 'WAL',]$name <- 'Wales'
fedList[fedList$code == 'NIR',]$name <- 'Northern Ireland'
fedList[fedList$code == 'GIB',]$name <- 'Gibraltar'
fedList[fedList$code == 'FRO',]$name <- 'Feroe Islands'
fedList[fedList$code == 'LVA',]$name <- 'Latvia'
fedList[fedList$code == 'SVN',]$name <- 'Slovenia'

fedList[fedList$code == 'SCO' | fedList$code == 'ENG' |
            fedList$code == 'WAL' | fedList$code == 'NIR' |
            fedList$code == 'GIB' | fedList$code == 'FRO' |
            fedList$code == 'LVA',]$region <- 'Northern Europe'

fedList[fedList$code == 'SVN',]$region <- 'Southern Europe'

countryList <- fedList

## Sorry, guys, GoogleVis maps works this way
countryList[countryList$code == 'SCO',]$name <- 'United Kingdom'
countryList[countryList$code == 'ENG',]$name <- 'United Kingdom'
countryList[countryList$code == 'WAL',]$name <- 'United Kingdom'
countryList[countryList$code == 'NIR',]$name <- 'United Kingdom'
countryList[countryList$code == 'GIB',]$name <- 'United Kingdom'

## Final matches
finals <- as.data.frame(ch[ch$round == 'final',])
max(finals$Season)

## Rvest
## Top Goalscorers per season
goalsc <- read_html("https://en.wikipedia.org/wiki/List_of_European_Cup_and_UEFA_Champions_League_top_scorers")

tGoalScPerSeason <- html_nodes(goalsc, xpath='//*[@id="mw-content-text"]/table[2]') %>%
    html_table() %>%
    as.data.frame()

tGoalScPerSeason$Season <- as.numeric(substr(tGoalScPerSeason$Season, 0, 4))

teamAltNames <- as.data.frame(cbind(
    Team = sort(unique(tGoalScPerSeason$Club)),
    Alias = 
        c('AFC Ajax', 'RSC Anderlecht', 'FC Ararat', 
          'Atletico Madrid', 'Barcelona', 'Bayern Munich', 
          'SL Benfica', 'Girondins Bordeaux', 'Borussia Monchengladbach', 
          'Celtic', 'Dinamo Minsk', 'Dinamo Kiev', 'Ferencvaros', 
          'Grasshoppers Zurich', 'IFK Goteborg', 'Internazionale', 
          'Juventus', 'Leeds United', 'Liverpool', 'Manchester United', 
          'Olympique Marseille', 'AC Milan', 'AS Monaco', 'Nurnberg', 
          'Panathinaikos', 'Paris Saint-Germain', 'Partizan Belgrade', 
          'FC Porto', 'PSV Eindhoven', 'Rangers', 'Real Madrid', 
          'Crvena Zvezda', 'Stade Reims', 'Standard Liege', 
          'Steaua Bucuresti', 'FC Tirol', 'Ujpesti Dozsa SC', 
          'ASK Vorwarts Berlin', 'FC Zurich')))

tGoalScPerSeason<- merge(tGoalScPerSeason, teamAltNames, by.x = 'Club', by.y = 'Team') %>%
    arrange(Season)

## All-time goalscorers
goalsc <- read_html("https://en.wikipedia.org/wiki/List_of_European_Cup_and_UEFA_Champions_League_top_scorers")

tGoalSc <- html_nodes(goalsc, xpath='//*[@id="mw-content-text"]/table[1]') %>%
    html_table() %>%
    as.data.frame() %>%
    transmute(Player, Goals)

tGoalSc$Goals <- as.numeric(sub("\\[\\w+\\]$","", tGoalSc$Goals))

## Records by team

records <- read_html("https://en.wikipedia.org/wiki/UEFA_Champions_League")

recTeams <- html_nodes(records, xpath='//*[@id="mw-content-text"]/table[3]') %>%
    html_table() %>%
    as.data.frame()

recTeams$Years.won <- sub("^\\s+","", recTeams$Years.won)
recTeams$Runners.up <- sub("^\\s+","", recTeams$Runners.up)

## Functions
winners <- function(seasonStart, seasonEnd, withDate){
    w <- ch[ch$round == 'final' & 
                      ch$Season >= seasonStart & 
                      ch$Season <= seasonEnd,]
    if(withDate){
        w <- transmute(w, Date, Season, Winner = tiewinner,
                             cCode = ifelse(tiewinner == home, hcountry, vcountry)) %>%
            group_by(Season, Winner, cCode) %>%
            summarise(Date = max(Date))
    }
    else{
        w <- transmute(w, Season, Winner = tiewinner,
                  cCode = ifelse(tiewinner == home, hcountry, vcountry))
    }
    return(w)
}

topGoalScorers <- function(seasonStart, seasonEnd){
    tgs <- as.data.frame(
    arrange(tGoalScPerSeason[tGoalScPerSeason$Season >= seasonStart & 
                                    tGoalScPerSeason$Season <= seasonEnd,] %>%
        transmute(Season = Season,
        "Top goalscorer" = paste0(Player, " (", 
                                  Alias, ")"),
        "Goals in the season" = Goals,
        "Goals per match" = round(Goals/
                                      mapply(howManyMatches, Season, Alias), 
                                  digits = 2))))
    return(tgs)
}

winnersWithCountry <- function(seasonStart, seasonEnd, withDate){
    r <- merge(winners(seasonStart, seasonEnd, withDate), 
          countryList, 
          by.x = 'cCode', by.y ='code', all.x = TRUE)
    if(withDate){
        return(arrange(transmute(r,
            Date, Season, Winner, Country = name, Region = region), Season))
    }
    else{
        return(arrange(transmute(r,
            Season, Winner, Country = name, Region = region), Season))
    }
}

countriesContending <- function(seasonStart, seasonEnd){
    c <- unique(ch[ch$Season >= seasonStart & 
                       ch$Season <= seasonEnd,]$hcountry)
    c <- merge(as.data.frame(cbind(code = c)), countryList, all.x = TRUE)
    return(c)
}

federationsContending <- function(seasonStart, seasonEnd){
    c <- unique(ch[ch$Season >= seasonStart & 
                       ch$Season <= seasonEnd,]$hcountry)
    c <- merge(as.data.frame(cbind(code = c)), fedList, all.x = TRUE)
    return(c)
}

teamsContending <- function(seasonStart, seasonEnd){
    t <- as.data.frame(
        cbind(Team = ch[ch$Season >= seasonStart & 
                            ch$Season <= seasonEnd,]$home, 
              code = ch[ch$Season >= seasonStart & 
                              ch$Season <= seasonEnd,]$hcountry)) %>%
        unique()
    t<- merge(t, fedList, all.x = TRUE) %>%
        transmute(Team, Country = name, Region = region) 
    return(t)
}

teamsAggContending <- function(seasonStart, seasonEnd){
    t <- teamsContending(seasonStart, seasonEnd)%>%
    group_by(Region, Country) %>%
    summarise(Teams = length(Team))
    return(t)
}

countriesAggContending <- function(seasonStart, seasonEnd){
    t <- teamsAggContending(seasonStart, seasonEnd) %>%
        group_by(Region) %>%
        summarise(Countries = length(Country), Teams = sum(Teams))
    return(t)
}

regionsContending <- function(seasonStart, seasonEnd){
    return(
        countriesContending(seasonStart, seasonEnd) %>%
            transmute(Region = region, Country = name) %>%
            group_by(Region) %>%
            summarise(Countries = length(unique(Country))) %>%
            arrange(Region)
    )
}

seasonsCountries <- function(seasonStart, seasonEnd){
    winTeams <- winners(seasonStart, seasonEnd, FALSE)
    counContending <- countriesContending(seasonStart, seasonEnd)
    counContending <- merge(counContending, winTeams, by.x = 'code', by.y = 'cCode', all.x = TRUE) %>%
               transmute(Country = name, Winner = ifelse(!is.na(Winner), 1, 2)) %>%
               unique()
    return(as.data.frame(
        group_by(counContending, Country) %>% 
            summarise(Winner = min(Winner))))
}

howManyMatches <- function(season, team){
    nrow(ch[ch$Season == season & (ch$home == team | ch$visitor == team),])
}

matchesToPlay <- function(seasonStart, seasonEnd){
    f <- finals[finals$Season >= seasonStart & finals$Season <= seasonEnd,]
    as.data.frame(
        cbind(Season = f$Season, 
              Winner = f$tiewinner,
              Matches = mapply(howManyMatches,f$Season,f$tiewinner)))
}

# Analysing the data

## Identify periods
dat <- winnersWithCountry(1955, 2015, TRUE) %>%
    transmute(Region = factor(Region), Season)
              
ggplot(dat, aes(x=Season, y=Region, colour=Region)) + 
    geom_point(size=3) +
    scale_x_continuous(breaks=1955:2015) +
    scale_colour_manual(values = terrain_hcl(length(unique(dat$Region)))) +
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          panel.grid.major = element_line(colour = "grey"),
          axis.text.x = element_text(size = 8, angle = 45),
          legend.key = element_rect(fill = "white")) +
    xlab("") + 
    ylab("")
####

## 55-65 Southern Europe dominance

####
tree <- teamsContending(1955,1955) %>%
    transmute(Team, Country, val = 1) %>%
    mutate(col = cumsum(val))

treemap(tree,
        index=c("Country", "Team"),
        vSize="val",
        type="index",
        palette = terrain_hcl(nrow(tree)),
        title = "",
        force.print.labels = TRUE,
        overlap.labels = 1,
        fontsize.labels=c(10, 8), 
        align.labels=list(c("left", "top"), c("center", "bottom")),
        lowerbound.cex.labels=1
)
####

ca <- regionsContending(1955,1965) %>%
    transmute(Region)

for(i in 1955:1965){
    reg <- countriesAggContending(i,i)[,c(1,3)]
    ca <- merge(ca, reg, all = TRUE)
    colnames(ca)[ncol(ca)] <- i
}
ca[is.na(ca)] <- 0

## Change table for ggplot box and whiskers

a <- c(0, 0, 0)
uca <- unname(ca)
for(j in 2:ncol(ca)){
    a <- rbind(a, cbind(uca[1], colnames(ca)[j], uca[j], stringsAsFactors = FALSE))
}
a <- a[2:nrow(a),]
colnames(a) <- c("Region", "Year", "Teams")

p <- ggplot(a, aes(Region, Teams)) + 
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          panel.grid.major = element_line(colour = "grey"),
          axis.text.x = element_text(size = 8))
p + geom_boxplot()

ca <- merge(ca, countriesAggContending(1955,1965)[,c(1,3)])
colnames(ca)[ncol(ca)] <- "Total"
plot(gvisTable(ca))

###
ca2 <- regionsContending(1955,1965) %>%
    transmute(Region)

for(i in 1955:1965){
    reg2 <- countriesAggContending(i,i) %>%
        transmute(Region = Region, Tpc = round(Teams/Countries, digits = 2))
    ca2 <- merge(ca2, reg2, all = TRUE)
    colnames(ca2)[ncol(ca2)] <- i
}
ca2[is.na(ca2)] <- 0
plot(gvisTable(ca2))


a2 <- c(0, 0, 0)
uca2 <- unname(ca2)
for(j in 2:ncol(ca2)){
    a2 <- rbind(a2, cbind(uca2[1], colnames(ca2)[j], uca2[j], stringsAsFactors = FALSE))
}
a2 <- a2[2:nrow(a2),]
colnames(a2) <- c("Region", "Year", "Tpc")

p <- ggplot(a2, aes(Region, Tpc)) + 
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          panel.grid.major = element_line(colour = "grey"),
          axis.text.x = element_text(size = 8))
p + geom_boxplot()

###
## Countries with more than one team in the same season
for(i in 1955:1965){
    print(i)
    print(teamsContending(i, i) %>%
              filter(Region == "Southern Europe") %>%
              group_by(Country) %>%
              mutate(nT = length(unique(Team))) %>%
              filter(nT >1))
}

####

countriesAndTeamsPerRegion <- countriesAggContending(1955,1965) %>%
    mutate("Teams per country" = round(Teams/Countries, digits = 2))

plot(gvisTable(countriesAndTeamsPerRegion))


treemap(teamsAggContending(1955,1965),
        index=c("Region", "Country"),
        vSize="Teams",
        type="index",
        palette = terrain_hcl(nrow(tree)),
        title = "",
        force.print.labels = TRUE,
        overlap.labels = 1,
        fontsize.labels=c(10, 8), 
        align.labels=list(c("left", "top"), c("center", "bottom")),
        lowerbound.cex.labels=1
)

winners5565 <- gvisTable(winnersWithCountry(1955, 1965, FALSE))

Geo5565 <- gvisGeoChart(seasonsCountries(1955, 1965),
                        locationvar="Country", 
                        colorvar="Winner",
                        options = list(region = 150,
                                       colorAxis = "{colors: ['green', 'lightgrey']}",
                                       legend = 'none'))

GT <- gvisMerge(Geo5565, winners5565, horizontal=TRUE)
plot(GT)
####

nTeams <- c(Year = "0", NumTeams = 0)

for(i in 1955:1965){
    nTeams <- rbind(nTeams, c(i,length(unique(countriesContending(i,i)[,2]))))
}

nTeams <- as.data.frame(nTeams[2:nrow(nTeams),])

p <- ggplot(nTeams, aes(x=Year, y=NumTeams)) + 
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          panel.grid.major = element_line(colour = "grey"),
          axis.text.x = element_text(size = 8))
p + geom_line(colour = "green")


nTeamsChart <- gvisLineChart(nTeams,
                             options = list(hAxis.format = "#"))

plot(nTeamsChart)

####
# Different number of matches to win

mtpTable <- gvisTable(matchesToPlay(1955, 1974))
mtpTable2 <- gvisTable(matchesToPlay(1975, 1994))
mtpTable3 <- gvisTable(matchesToPlay(1995, 2015))

TT <- gvisMerge(mtpTable, mtpTable2, horizontal=TRUE)
TTT <- gvisMerge(TT, mtpTable3, horizontal=TRUE)
plot(TTT)

finals[finals$Season=='1973',]
####
## Top goalscorers

plot(gvisTable(topGoalScorers(1955, 1965)))



# 66-83: Northern Europe dominance

plot(gvisTable(countriesAggContending(1966,1983)))

treemap(teamsAggContending(1966,1983),
        index=c("Region", "Country"),
        vSize="Teams",
        type="index",
        palette = terrain_hcl(nrow(tree)),
        title = "",
        force.print.labels = TRUE,
        overlap.labels = 1,
        fontsize.labels=c(10, 8), 
        align.labels=list(c("left", "top"), c("center", "bottom")),
        lowerbound.cex.labels=1
)


winners6683 <- gvisTable(winnersWithCountry(1966, 1983, FALSE))

Geo6683 <- gvisGeoChart(seasonsCountries(1966, 1983),
                        locationvar="Country", 
                        colorvar="Winner",
                        options = list(region = 150,
                                       colorAxis = "{colors: ['green', 'lightgrey']}",
                                       legend = 'none'))

GT <- gvisMerge(Geo6683, winners6683, horizontal=TRUE)
plot(GT)

ca <- regionsContending(1966,1983) %>%
    transmute(Region)

for(i in 1966:1983){
    c <- countriesAggContending(i,i)
    ca <- cbind.data.frame(ca, c[,3])
    colnames(ca)[ncol(ca)] <- i
}

ca <- cbind.data.frame(ca, countriesAggContending(1966,1983)[,3])
colnames(ca)[ncol(ca)] <- "Total"
plot(gvisTable(ca))

## Change table for ggplot box and whiskers

a <- c(0, 0, 0)
uca <- unname(ca)
for(j in 2:(ncol(ca)-1)){
    a <- rbind(a, cbind(uca[1], colnames(ca)[j], uca[j], stringsAsFactors = FALSE))
}
a <- a[2:nrow(a),]
colnames(a) <- c("Region", "Year", "Teams")

p <- ggplot(a, aes(Region, Teams)) + 
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          panel.grid.major = element_line(colour = "grey"),
          axis.text.x = element_text(size = 8))
p + geom_boxplot()


plot(gvisTable(topGoalScorers(1966, 1983)))

howManyMatches(1966,"ASK Vorwarts Berlin")
howManyMatches(1966,"Vorwarts Berlin")
howManyMatches(1966,"RSC Anderlecht")

ch[ch$home == "ASK Vorwarts Berlin" | ch$visitor == "ASK Vorwarts Berlin",]


# More equal times

plot(gvisTable(countriesAggContending(1984, 2015)))

treemap(teamsAggContending(1984, 2015),
        index=c("Region", "Country"),
        vSize="Teams",
        type="index",
        palette = terrain_hcl(nrow(tree)),
        title = "",
        force.print.labels = TRUE,
        overlap.labels = 1,
        fontsize.labels=c(10, 8), 
        align.labels=list(c("left", "top"), c("center", "bottom")),
        lowerbound.cex.labels=1
)


winners8416 <- gvisTable(winnersWithCountry(1984, 2016, FALSE))

Geo8416 <- gvisGeoChart(seasonsCountries(1984, 2016),
                        locationvar="Country", 
                        colorvar="Winner",
                        options = list(region = 150,
                                       colorAxis = "{colors: ['green', 'lightgrey']}",
                                       legend = 'none'))

GT <- gvisMerge(Geo8416, winners8416, horizontal=TRUE)
plot(GT)


evolutionReg <- regionsContending(1955,2015) %>%
    transmute(Region)

for(i in 1955:2015){
    reg <- countriesAggContending(i,i)[,c(1,3)]
    evolutionReg <- merge(evolutionReg, reg, all = TRUE)
    colnames(evolutionReg)[ncol(evolutionReg)] <- i
}

evolutionReg[is.na(evolutionReg)] <- 0
tEvolutionReg <- t(evolutionReg[,2:ncol(evolutionReg)])
colnames(tEvolutionReg) <- evolutionReg[,1]
tEvolutionReg <- as.data.frame(tEvolutionReg)
tEvolutionReg <- cbind(Year = rownames(tEvolutionReg), tEvolutionReg)

a <- c(0, 0, 0)
uevolutionReg <- unname(evolutionReg)
for(j in 2:(ncol(evolutionReg)-1)){
    a <- rbind(a, cbind(uevolutionReg[1], 
                        colnames(evolutionReg)[j], 
                        uevolutionReg[j], 
                        stringsAsFactors = FALSE))
}
a <- a[2:nrow(a),]
colnames(a) <- c("Region", "Year", "Teams")


p <- ggplot(a, aes(Year, Teams, group = Region, fill = Region)) +
    scale_fill_manual(values = terrain_hcl(length(unique(a$Region)))) +
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          panel.grid.major = element_line(colour = "grey"),
          axis.text.x = element_text(size = 8, angle = 45),
          legend.key = element_rect(fill = "white"))
p + geom_area(position = 'stack')

# Top goalscorers

t1 <- gvisTable(topGoalScorers(1984, 1990))
t2 <- gvisTable(topGoalScorers(1991, 2002))
t3 <- gvisTable(topGoalScorers(2003, 2016))

TT <- gvisMerge(t1, t2, horizontal=TRUE)
TTT <- gvisMerge(TT, t3, horizontal=TRUE)
plot(TTT)


# List of winners and runners-up
colnames(recTeams)[1] <- "Team"
recteamsT1 <- gvisTable(recTeams[1:22,])
recteamsT2 <- gvisTable(recTeams[23:39,])

TT <- gvisMerge(recteamsT1, recteamsT2, horizontal=TRUE)
plot(TT)

 

gscT1 <- gvisTable(tGoalSc[1:19,])
gscT2 <- gvisTable(tGoalSc[20:39,])
gscT3 <- gvisTable(tGoalSc[40:57,])

TT <- gvisMerge(gscT1, gscT2, horizontal=TRUE)
TTT <- gvisMerge(TT, gscT3, horizontal=TRUE)
plot(TTT)

gscB <- gvisBarChart(tGoalSc[1:10,], options = list(height = 400))

plot(gscB)
plot(gscT)

##

# Chord diagram of the finals

teamsFinals <- c(finals$home, finals$visitor) %>%
    unique() %>%
    sort()

nameToIndex <- function(name){
    which(teamsFinals == name)
}

datChord <- as.data.frame(
    cbind(from = mapply(nameToIndex, finals$home), 
          to = mapply(nameToIndex, finals$visitor),
          stringsAsFactors = FALSE)) %>%
    mutate(value = 1)

datChord <- aggregate(datChord$value, 
                      by = list(datChord$from, datChord$to), 
                      FUN=sum)

chordDiagram(datChord)
circos.clear()

cbind(1:length(teamsFinals), teamsFinals)

