install.packages("tidyverse")
library(readxl)
library(ggplot2)
install.packages("xaringan")
install.packages("dplyr")
library("dplyr")
install.packages("magrittr")
library("magrittr")




atleti<- read.csv("C:/Users/edomo/Desktop/olympic_athletes.csv")
medaglie<-read.csv("C:/Users/edomo/Desktop/olympic_medals.csv")
risultati<-read.csv("C:/Users/edomo/Desktop/olympic_results.csv")
host<-read.csv("C:/Users/edomo/Desktop/olympic_hosts.csv")
atletifull<-read.csv("C:/Users/edomo/Desktop/athlete_events.csv")

#chi ha vinto piu medaglie?
medaglienazioni=medaglie%>%
  group_by(country_3_letter_code) %>%
  summarise(count = n()) %>% 
  arrange(-count)%>%
  head(20)
ggplot(medaglienazioni,aes(x =country_3_letter_code , y = count)) +
  geom_bar(stat="identity")



#sport piu popolari
sportpopolari=atletifull%>%
  group_by(Sport) %>%
  summarise(count = n()) %>% 
  arrange(-count)%>%
  head(15)
ggplot(sportpopolari,aes(x =Sport , y = count)) +
  geom_bar(stat="identity")
#chi ha hostato di piu nestivo-invernale
tophostsum=host%>%
  filter(game_season !="Winter")%>%
  group_by(game_location) %>%
  summarise(count = n()) %>% 
  arrange(-count)%>%
  head(15)
ggplot(tophostsum,aes(x =game_location , y = count)) +
  geom_bar(stat="identity")

tophostwint=host%>%
  filter(game_season !="Summer")%>%
  group_by(game_location,game_season) %>%
  summarise(count = n()) %>% 
  arrange(-count)%>%
  head(15)
ggplot(tophostwint,aes(x =game_location , y = count)) +
  geom_bar(stat="identity")

#gareggiare in casa aiuta a vincere?
medaglieogniolimpiade=medaglie%>%
  group_by(country_3_letter_code,slug_game) %>%
  summarise(count = n()) %>%
  arrange(- count)
#andamento usa medaglie
medaglieogniolimpiadeusa=medaglieogniolimpiade%>%
  filter(country_3_letter_code == "USA")  %>% 
  head(20)

ggplot(medaglieogniolimpiadeusa, aes(x = slug_game, y = count)) +
  geom_point()   

#andamento ita medaglie
medaglieogniolimpiadeita=medaglieogniolimpiade%>%
  filter(country_3_letter_code == "ITA")  %>% 
  head(20)

ggplot(medaglieogniolimpiadeita, aes(x = slug_game, y = count)) +
  geom_point()   

#andamento GBR medaglie
medaglieogniolimpiadeGBR=medaglieogniolimpiade%>%
  filter(country_3_letter_code == "GBR")  %>% 
  head(20)

ggplot(medaglieogniolimpiadeGBR, aes(x = slug_game, y = count)) +
  geom_point()   
  
#top 10 sport italiani
topsportita=medaglie%>%
  filter(country_3_letter_code =="ITA")%>%
  group_by(discipline_title) %>%
  summarise(count = n()) %>% 
  arrange(-count)%>%
  head(10)
ggplot(topsportita,aes(x =country_3_letter_code , y = count)) +
  geom_bar(stat="identity")
#top 10 sport usa
topsportusa=medaglie%>%
  filter(country_3_letter_code =="USA")%>%
  group_by(discipline_title) %>%
  summarise(count = n()) %>% 
  arrange(-count)%>%
  head(10)
ggplot(topsportusa,aes(x =country_3_letter_code , y = count)) +
  geom_bar(stat="identity")
#età media maschile
età=atletifull[!is.na(atletifull$Age), ]
etàmediaM= età%>%
  filter(Sex=="M")
  etàM=mean(etàmediaM$Age)
  
  ggplot(data =etàmediaM) +
    geom_boxplot(mapping = aes(y = Age) ) 
  
#età media femminile
  etàmediaF= età%>%
    filter(Sex=="F")
  etàF=mean(etàmediaF$Age)
  
  ggplot(data =etàmediaF) +
    geom_boxplot(mapping = aes(y = Age) )
#atletitop
atletitop=atleti%>%
  arrange(athlete_medals)
  atletitopp= tail(atletitop, n=10)
  
  ggplot(atletitopp,aes(x =athlete_full_name , y = athlete_medals)) +
    geom_bar(stat="identity")
  
  
  #perchè alcuni partecipanti sono cosi vecchi?
  etàUP50= età%>%
    filter(Age > 50)%>%
    group_by(Sport) %>%
    summarise(count = n()) %>% 
    arrange(-count)%>%
    head(20)
  ggplot(etàUP50,aes(x = Sport , y = count)) +
    geom_bar(stat="identity")
  
    
  
    


