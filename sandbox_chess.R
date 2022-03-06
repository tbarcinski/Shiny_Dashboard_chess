library(dplyr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(stringr)
library(tidyr)
library(lubridate)
library(padr)
library(zoo)

# read.csv #########################################################################################################################

df_tymekkk_blitz_black_games <- read.csv("dane_pierwotne/tymekkk_blitz_black_games.csv", na.strings=c("","NA"))%>%
  mutate(gracz="tymekkk",rodzaj="blitz",color="black")
df_tymekkk_blitz_black_moves <- read.csv("dane_pierwotne/tymekkk_blitz_black_moves.csv", na.strings=c("","NA"))%>%
  mutate(gracz="tymekkk",rodzaj="blitz",color="black")
df_tymekkk_blitz_white_games <- read.csv("dane_pierwotne/tymekkk_blitz_white_games.csv", na.strings=c("","NA"))%>%
  mutate(gracz="tymekkk",rodzaj="blitz",color="white")
df_tymekkk_blitz_white_moves <- read.csv("dane_pierwotne/tymekkk_blitz_white_moves.csv", na.strings=c("","NA"))%>%
  mutate(gracz="tymekkk",rodzaj="blitz",color="white")
df_Mlodziak77_rapid_black_games <- read.csv("dane_pierwotne/Mlodziak77_rapid_black_games.csv", na.strings=c("","NA"))%>%
  mutate(gracz="Mlodziak77",rodzaj="rapid",color="black")
df_Mlodziak77_rapid_black_moves <- read.csv("dane_pierwotne/Mlodziak77_rapid_black_moves.csv", na.strings=c("","NA"))%>%
  mutate(gracz="Mlodziak77",rodzaj="rapid",color="black")
df_Mlodziak77_rapid_white_games <- read.csv("dane_pierwotne/Mlodziak77_rapid_white_games.csv", na.strings=c("","NA"))%>%
  mutate(gracz="Mlodziak77",rodzaj="rapid",color="white")
df_Mlodziak77_rapid_white_moves <- read.csv("dane_pierwotne/Mlodziak77_rapid_white_moves.csv", na.strings=c("","NA"))%>%
  mutate(gracz="Mlodziak77",rodzaj="rapid",color="white")
df_Mlodziak77_blitz_black_games <- read.csv("dane_pierwotne/Mlodziak77_blitz_black_games.csv", na.strings=c("","NA"))%>%
  mutate(gracz="Mlodziak77",rodzaj="blitz",color="black")
df_Mlodziak77_blitz_black_moves <- read.csv("dane_pierwotne/Mlodziak77_blitz_black_moves.csv", na.strings=c("","NA"))%>%
  mutate(gracz="Mlodziak77",rodzaj="blitz",color="black")
df_Mlodziak77_blitz_white_games <- read.csv("dane_pierwotne/Mlodziak77_blitz_white_games.csv", na.strings=c("","NA"))%>%
  mutate(gracz="Mlodziak77",rodzaj="blitz",color="white")
df_Mlodziak77_blitz_white_moves <- read.csv("dane_pierwotne/Mlodziak77_blitz_white_moves.csv", na.strings=c("","NA"))%>%
  mutate(gracz="Mlodziak77",rodzaj="blitz",color="white")
df_Mlodziak77_bullet_black_games <- read.csv("dane_pierwotne/Mlodziak77_bullet_black_games.csv", na.strings=c("","NA"))%>%
  mutate(gracz="Mlodziak77",rodzaj="bullet",color="black")
df_Mlodziak77_bullet_black_moves <- read.csv("dane_pierwotne/Mlodziak77_bullet_black_moves.csv", na.strings=c("","NA"))%>%
  mutate(gracz="Mlodziak77",rodzaj="bullet",color="black")
df_Mlodziak77_bullet_white_games <- read.csv("dane_pierwotne/Mlodziak77_bullet_white_games.csv", na.strings=c("","NA"))%>%
  mutate(gracz="Mlodziak77",rodzaj="bullet",color="white")
df_Mlodziak77_bullet_white_moves <- read.csv("dane_pierwotne/Mlodziak77_bullet_white_moves.csv", na.strings=c("","NA"))%>%
  mutate(gracz="Mlodziak77",rodzaj="bullet",color="white")
df_grooney_rapid_black_games <- read.csv("dane_pierwotne/grooney_rapid_black_games.csv", na.strings=c("","NA"))%>%
  mutate(gracz="grooney",rodzaj="rapid",color="black")
df_grooney_rapid_black_moves <- read.csv("dane_pierwotne/grooney_rapid_black_moves.csv", na.strings=c("","NA"))%>%
  mutate(gracz="grooney",rodzaj="rapid",color="black")
df_grooney_rapid_white_games <- read.csv("dane_pierwotne/grooney_rapid_white_games.csv", na.strings=c("","NA"))%>%
  mutate(gracz="grooney",rodzaj="rapid",color="white")
df_grooney_rapid_white_moves <- read.csv("dane_pierwotne/grooney_rapid_white_moves.csv", na.strings=c("","NA"))%>%
  mutate(gracz="grooney",rodzaj="rapid",color="white")
df_grooney_blitz_black_games <- read.csv("dane_pierwotne/grooney_blitz_black_games.csv", na.strings=c("","NA"))%>%
  mutate(gracz="grooney",rodzaj="blitz",color="black")
df_grooney_blitz_black_moves <- read.csv("dane_pierwotne/grooney_blitz_black_moves.csv", na.strings=c("","NA"))%>%
  mutate(gracz="grooney",rodzaj="blitz",color="black")
df_grooney_blitz_white_games <- read.csv("dane_pierwotne/grooney_blitz_white_games.csv", na.strings=c("","NA"))%>%
  mutate(gracz="grooney",rodzaj="blitz",color="white")
df_grooney_blitz_white_moves <- read.csv("dane_pierwotne/grooney_blitz_white_moves.csv", na.strings=c("","NA"))%>%
  mutate(gracz="grooney",rodzaj="blitz",color="white")
df_grooney_bullet_black_games <- read.csv("dane_pierwotne/grooney_bullet_black_games.csv", na.strings=c("","NA"))%>%
  mutate(gracz="grooney",rodzaj="bullet",color="black")
df_grooney_bullet_black_moves <- read.csv("dane_pierwotne/grooney_bullet_black_moves.csv", na.strings=c("","NA"))%>%
  mutate(gracz="grooney",rodzaj="bullet",color="black")
df_grooney_bullet_white_games <- read.csv("dane_pierwotne/grooney_bullet_white_games.csv", na.strings=c("","NA"))%>%
  mutate(gracz="grooney",rodzaj="bullet",color="white")
df_grooney_bullet_white_moves <- read.csv("dane_pierwotne/grooney_bullet_white_moves.csv", na.strings=c("","NA"))%>%
  mutate(gracz="grooney",rodzaj="bullet",color="white")

# df_plot2 ######################################################################################################################

df_plot2<-rbind(df_tymekkk_blitz_black_moves,
                df_tymekkk_blitz_white_moves,
                df_Mlodziak77_rapid_black_moves,
                df_Mlodziak77_rapid_white_moves,
                df_Mlodziak77_blitz_black_moves,
                df_Mlodziak77_blitz_white_moves,
                df_Mlodziak77_bullet_black_moves,
                df_Mlodziak77_bullet_white_moves,
                df_grooney_rapid_black_moves,
                df_grooney_rapid_white_moves,
                df_grooney_blitz_black_moves,
                df_grooney_blitz_white_moves,
                df_grooney_bullet_black_moves,
                df_grooney_bullet_white_moves) %>%
  mutate(move=ifelse(color=="black",black,white)) %>% 
  select(gracz,rodzaj,color,move)

df_plot2<-rbind(df_plot2,mutate(df_plot2,color="both")) %>%
  filter(move!=".") %>% 
  filter(substr(move, 1, 1)!="O") %>% 
  na.omit()%>% 
  mutate(Pawns_moves = ifelse(substr(move, 1, 1) != toupper(substr(move, 1, 1)),substr(move, 1, 1),NA),
         Pawns_captures = ifelse(substr(move, 1, 1) != toupper(substr(move, 1, 1)) & grepl("x", move),substr(move, 1, 1),NA),
         Pawns_promotions = ifelse(substr(move, 1, 1) != toupper(substr(move, 1, 1)) & grepl("=", move),substr(move, 1, 1),NA),
         Pieces_moves = ifelse(substr(move, 1, 1) == toupper(substr(move, 1, 1)),substr(move, 1, 1),NA),
         Pieces_captures = ifelse(substr(move, 1, 1) == toupper(substr(move, 1, 1)) & grepl("x", move),substr(move, 1, 1),NA)) 
df_plot2_Pawns_moves<- df_plot2 %>%  
  group_by(rodzaj,gracz,color,Pawns_moves)%>%
  summarise(n = n())%>% 
  na.omit()
df_plot2_Pawns_captures<- df_plot2 %>%  
  group_by(rodzaj,gracz,color,Pawns_captures)%>%
  summarise(n = n())%>% 
  na.omit()
df_plot2_Pawns_promotions<- df_plot2 %>%  
  group_by(rodzaj,gracz,color,Pawns_promotions)%>%
  summarise(n = n())%>% 
  na.omit()
df_plot2_Pieces_moves<- df_plot2 %>%  
  group_by(rodzaj,gracz,color,Pieces_moves)%>%
  summarise(n = n())%>% 
  na.omit()
df_plot2_Pieces_captures<- df_plot2 %>%  
  group_by(rodzaj,gracz,color,Pieces_captures)%>%
  summarise(n = n())%>% 
  na.omit()
Pieces_moves <- rep(c("R", "N", "B", "K", "Q", "B ", "N ", "R "),times=27,each=1)
Pieces_captures <- rep(c("R", "N", "B", "K", "Q", "B ", "N ", "R "),times=27,each=1)
Pawns_moves <- rep(letters[1:8],times=27,each=1)
Pawns_captures <- rep(letters[1:8],times=27,each=1)
Pawns_promotions <- rep(letters[1:8],times=27,each=1)
rodzaj<-rep(c("blitz","bullet","rapid"),times=1,each=72)
gracz<-rep(c("grooney","Mlodziak77","tymekkk"),times=3,each=24)
color<-rep(c("black","white","both"),each=8,times=9)
n<-rep(0,216)

df_plot2_Pawns_moves_0<-data.frame(rodzaj,gracz,color,Pawns_moves,n)
df_plot2_Pawns_captures_0<-data.frame(rodzaj,gracz,color,Pawns_captures,n)
df_plot2_Pawns_promotions_0<-data.frame(rodzaj,gracz,color,Pawns_promotions,n)
df_plot2_Pieces_moves_0<-data.frame(rodzaj,gracz,color,Pieces_moves,n)
df_plot2_Pieces_captures_0<-data.frame(rodzaj,gracz,color,Pieces_captures,n)

df_plot2_Pawns_moves<-rbind(df_plot2_Pawns_moves,df_plot2_Pawns_moves_0) %>% 
  distinct(rodzaj,gracz,color,Pawns_moves,.keep_all = TRUE)%>% 
  mutate(typ="Pawns_moves") %>% 
  rename(nazwa=Pawns_moves)
df_plot2_Pawns_captures<-rbind(df_plot2_Pawns_captures,df_plot2_Pawns_captures_0) %>% 
  distinct(rodzaj,gracz,color,Pawns_captures,.keep_all = TRUE)%>% 
  mutate(typ="Pawns_captures")%>% 
  rename(nazwa=Pawns_captures)
df_plot2_Pawns_promotions<-rbind(df_plot2_Pawns_promotions,df_plot2_Pawns_promotions_0) %>% 
  distinct(rodzaj,gracz,color,Pawns_promotions,.keep_all = TRUE)%>% 
  mutate(typ="Pawns_promotions")%>% 
  rename(nazwa=Pawns_promotions)

df_plot2_Pieces_moves<-rbind(df_plot2_Pieces_moves,df_plot2_Pieces_moves_0) %>% 
  distinct(rodzaj,gracz,color,Pieces_moves,.keep_all = TRUE)
df_plot2_Pieces_captures<-rbind(df_plot2_Pieces_captures,df_plot2_Pieces_captures_0) %>% 
  distinct(rodzaj,gracz,color,Pieces_captures,.keep_all = TRUE)

df_plot2_Pieces_moves<-df_plot2_Pieces_moves %>% 
  mutate(Pieces_moves1=substr(Pieces_moves, 1, 1)) %>% 
  group_by(rodzaj,gracz,color,Pieces_moves1)%>%
  mutate(n1=max(n))%>%
  mutate(n=ifelse(Pieces_moves1=='R' |
                    Pieces_moves1=='N' |
                    Pieces_moves1=='B',
                  floor(n1/2),
                  n)) %>% 
  ungroup() %>%
  select(-c(Pieces_moves1,n1)) %>% 
  mutate(typ="Pieces_moves")%>% 
  rename(nazwa=Pieces_moves) 


df_plot2_Pieces_captures<-df_plot2_Pieces_captures %>% 
  mutate(Pieces_captures1=substr(Pieces_captures, 1, 1)) %>% 
  group_by(rodzaj,gracz,color,Pieces_captures1) %>%
  mutate(n1=max(n))%>%
  mutate(n=ifelse(Pieces_captures1=='R' |
                    Pieces_captures1=='N' |
                    Pieces_captures1=='B',
                  floor(n1/2),
                  n)) %>%
  ungroup() %>% 
  select(-c(Pieces_captures1,n1))%>% 
  mutate(typ="Pieces_captures")%>% 
  rename(nazwa=Pieces_captures)

df_plot2<-rbind(df_plot2_Pawns_moves,
                df_plot2_Pawns_captures,
                df_plot2_Pawns_promotions,
                df_plot2_Pieces_moves,
                df_plot2_Pieces_captures)

# df_plot1 ##############################################################################

df_tymekkk_blitz_black_plot1 <- df_tymekkk_blitz_black_games %>% 
  mutate(hero=ifelse(color=="white",white.elo,black.elo)) %>% 
  select(gracz,rodzaj,hero,date) 
df_tymekkk_blitz_white_plot1 <- df_tymekkk_blitz_white_games%>% 
  mutate(hero=ifelse(color=="white",white.elo,black.elo)) %>% 
  select(gracz,rodzaj,hero,date) 
df_Mlodziak77_rapid_black_plot1 <- df_Mlodziak77_rapid_black_games%>% 
  mutate(hero=ifelse(color=="white",white.elo,black.elo)) %>% 
  select(gracz,rodzaj,hero,date) 
df_Mlodziak77_rapid_white_plot1 <- df_Mlodziak77_rapid_white_games%>% 
  mutate(hero=ifelse(color=="white",white.elo,black.elo)) %>% 
  select(gracz,rodzaj,hero,date) 
df_Mlodziak77_blitz_black_plot1 <-df_Mlodziak77_blitz_black_games %>% 
  mutate(hero=ifelse(color=="white",white.elo,black.elo)) %>% 
  select(gracz,rodzaj,hero,date) 
df_Mlodziak77_blitz_white_plot1 <- df_Mlodziak77_blitz_white_games%>% 
  mutate(hero=ifelse(color=="white",white.elo,black.elo)) %>% 
  select(gracz,rodzaj,hero,date) 
df_Mlodziak77_bullet_black_plot1 <-df_Mlodziak77_bullet_black_games %>% 
  mutate(hero=ifelse(color=="white",white.elo,black.elo)) %>% 
  select(gracz,rodzaj,hero,date) 
df_Mlodziak77_bullet_white_plot1 <-df_Mlodziak77_bullet_white_games %>% 
  mutate(hero=ifelse(color=="white",white.elo,black.elo)) %>% 
  select(gracz,rodzaj,hero,date) 
df_grooney_rapid_black_plot1 <-df_grooney_rapid_black_games %>% 
  mutate(hero=ifelse(color=="white",white.elo,black.elo)) %>% 
  select(gracz,rodzaj,hero,date) 
df_grooney_rapid_white_plot1 <- df_grooney_rapid_white_games%>% 
  mutate(hero=ifelse(color=="white",white.elo,black.elo)) %>% 
  select(gracz,rodzaj,hero,date) 
df_grooney_blitz_black_plot1 <- df_grooney_blitz_black_games%>% 
  mutate(hero=ifelse(color=="white",white.elo,black.elo)) %>% 
  select(gracz,rodzaj,hero,date) 
df_grooney_blitz_white_plot1 <- df_grooney_blitz_white_games%>% 
  mutate(hero=ifelse(color=="white",white.elo,black.elo)) %>% 
  select(gracz,rodzaj,hero,date) 
df_grooney_bullet_black_plot1 <- df_grooney_bullet_black_games%>% 
  mutate(hero=ifelse(color=="white",white.elo,black.elo)) %>% 
  select(gracz,rodzaj,hero,date) 
df_grooney_bullet_white_plot1 <-df_grooney_bullet_white_games %>% 
  mutate(hero=ifelse(color=="white",white.elo,black.elo)) %>% 
  select(gracz,rodzaj,hero,date) 
df_tymekkk_blitz_plot1 <- rbind(df_tymekkk_blitz_white_plot1, df_tymekkk_blitz_black_plot1)%>%
  group_by(date) %>%
  summarise(rating = max(hero)) %>% 
  mutate(date=as_datetime(date)) %>% 
  pad() %>% 
  na.locf(fromLast = TRUE) %>% 
  mutate(date= as.Date(date))
df_Mlodziak77_bullet_plot1 <- rbind(df_Mlodziak77_bullet_white_plot1, df_Mlodziak77_bullet_black_plot1)%>%
  group_by(date) %>%
  summarise(rating = max(hero)) %>% 
  mutate(date=as_datetime(date)) %>% 
  pad() %>% 
  na.locf(fromLast = TRUE) %>% 
  mutate(date= as.Date(date))
df_Mlodziak77_blitz_plot1 <- rbind(df_Mlodziak77_blitz_white_plot1, df_Mlodziak77_blitz_black_plot1)%>%
  group_by(date) %>%
  summarise(rating = max(hero)) %>% 
  mutate(date=as_datetime(date)) %>% 
  pad() %>% 
  na.locf(fromLast = TRUE) %>% 
  mutate(date= as.Date(date))
df_Mlodziak77_rapid_plot1 <- rbind(df_Mlodziak77_rapid_white_plot1, df_Mlodziak77_rapid_black_plot1) %>%
  group_by(date) %>%
  summarise(rating = max(hero)) %>% 
  mutate(date=as_datetime(date)) %>% 
  pad() %>% 
  na.locf(fromLast = TRUE) %>% 
  mutate(date= as.Date(date))
df_grooney_rapid_plot1 <- rbind(df_grooney_rapid_white_plot1, df_grooney_rapid_black_plot1)%>%
  group_by(date) %>%
  summarise(rating = max(hero)) %>% 
  mutate(date=as_datetime(date)) %>% 
  pad() %>% 
  na.locf(fromLast = TRUE) %>% 
  mutate(date= as.Date(date))
df_grooney_blitz_plot1 <- rbind(df_grooney_blitz_white_plot1, df_grooney_blitz_black_plot1)%>%
  group_by(date) %>%
  summarise(rating = max(hero)) %>% 
  mutate(date=as_datetime(date)) %>% 
  pad() %>% 
  na.locf(fromLast = TRUE) %>% 
  mutate(date= as.Date(date))
df_grooney_bullet_plot1 <- rbind(df_grooney_bullet_white_plot1, df_grooney_bullet_black_plot1) %>%
  group_by(date) %>%
  summarise(rating = max(hero)) %>% 
  mutate(date=as_datetime(date)) %>% 
  pad() %>% 
  na.locf(fromLast = TRUE) %>% 
  mutate(date= as.Date(date))
df_blitz_plot1 <- merge(merge(df_grooney_blitz_plot1, df_Mlodziak77_blitz_plot1, by = "date", all = TRUE),
                        df_tymekkk_blitz_plot1, by = "date", all = TRUE) %>% 
  rename(grooney=rating.x,Mlodziak77=rating.y,tymekkk=rating) %>% 
  mutate(rodzaj="blitz")

df_bullet_plot1 <- merge(df_grooney_bullet_plot1, df_Mlodziak77_bullet_plot1, by = "date", all = TRUE) %>% 
  rename(grooney=rating.x,Mlodziak77=rating.y)%>% 
  mutate(tymekkk=NA,rodzaj="bullet")

df_rapid_plot1 <- merge(df_grooney_rapid_plot1, df_Mlodziak77_rapid_plot1, by = "date", all = TRUE) %>% 
  rename(grooney=rating.x,Mlodziak77=rating.y)%>% 
  mutate(tymekkk=NA,rodzaj="rapid")
df_plot1<-rbind(df_blitz_plot1,
                df_bullet_plot1,
                df_rapid_plot1)%>% 
  mutate(date= as.Date(date),tymekkk=tymekkk+300)

# df_plot1<-rbind(df_tymekkk_blitz_black_games,
#                   df_tymekkk_blitz_white_games,
#                   df_Mlodziak77_rapid_black_games,
#                   df_Mlodziak77_rapid_white_games,
#                   df_Mlodziak77_blitz_black_games,
#                   df_Mlodziak77_blitz_white_games,
#                   df_Mlodziak77_bullet_black_games,
#                   df_Mlodziak77_bullet_white_games,
#                   df_grooney_rapid_black_games,
#                   df_grooney_rapid_white_games,
#                   df_grooney_blitz_black_games,
#                   df_grooney_blitz_white_games,
#                   df_grooney_bullet_black_games,
#                   df_grooney_bullet_white_games) %>% 
#   mutate(hero=ifelse(color=="white",white.elo,black.elo)) %>% 
#   select(gracz,rodzaj,hero,date) %>% 
#   group_by(gracz,rodzaj,date) %>%
#   mutate(rating = max(hero),date=as_datetime(date)) %>% 
#   ungroup() %>% 
#   select(-hero) %>% 
#   pad(group = c("gracz","rodzaj")) %>% 
#   na.locf(fromLast = TRUE) %>% 
#   ungroup() %>% 
#   distinct() %>% 
#   pivot_wider(values_from=rating,names_from= gracz) %>%
#   group_by(rodzaj) %>% 
#   na.locf(., na.rm = FALSE) %>% 
#   mutate(tymekkk=tymekkk+300)

# df_heatmap #######################
df_games<-rbind(df_tymekkk_blitz_black_games,
                df_tymekkk_blitz_white_games,
                df_Mlodziak77_rapid_black_games,
                df_Mlodziak77_rapid_white_games,
                df_Mlodziak77_blitz_black_games,
                df_Mlodziak77_blitz_white_games,
                df_Mlodziak77_bullet_black_games,
                df_Mlodziak77_bullet_white_games,
                df_grooney_rapid_black_games,
                df_grooney_rapid_white_games,
                df_grooney_blitz_black_games,
                df_grooney_blitz_white_games,
                df_grooney_bullet_black_games,
                df_grooney_bullet_white_games) %>% 
  select(date,time,color,gracz,rodzaj,hero.points) %>% 
  mutate(weekday = weekdays(as.Date(date, "%Y.%m.%d"))) %>% 
  mutate(time_hour = as.numeric(str_extract(time, "\\d+")))

df_games<-rbind(df_games,mutate(df_games,color="both")) %>% 
  group_by(weekday, time_hour, color,gracz,rodzaj) %>%
  summarise(sum = sum(hero.points, na.rm = TRUE), n = n()) %>% 
  mutate(mean=sum/n) %>% 
  mutate(day=case_when(weekday=="poniedzia³ek" ~ 1,
                       weekday=="wtorek"~ 2,
                       weekday=="œroda"~ 3,
                       weekday=="czwartek"~ 4,
                       weekday=="pi¹tek"~ 5,
                       weekday=="sobota"~ 6,
                       weekday=="niedziela"~ 7),
         hour=time_hour) %>% 
  ungroup() %>% 
  select(-c(weekday,time_hour,sum))

gracz<-rep(c("grooney","Mlodziak77","tymekkk"),times=1,each=1512)
rodzaj<-rep(c("blitz","bullet","rapid"),times=3,each=504)
color<- rep(c("white", "black","both"),times=9,each=168)
day <- rep(c(1:7),times=27,each=24)
hour <- rep(c(0:23),times=189,each=1)
n <- rep(0,4536)
mean <- rep(0,4536)
df_games_0<-data.frame(gracz,rodzaj,color,day,hour,n,mean)
df_heatmap<-rbind(df_games,df_games_0) %>% 
  distinct(gracz,rodzaj,color,day,hour,.keep_all = TRUE)

# df_scatter ###########################

df_scatter<-rbind(df_tymekkk_blitz_black_games,
                  df_tymekkk_blitz_white_games,
                  df_Mlodziak77_rapid_black_games,
                  df_Mlodziak77_rapid_white_games,
                  df_Mlodziak77_blitz_black_games,
                  df_Mlodziak77_blitz_white_games,
                  df_Mlodziak77_bullet_black_games,
                  df_Mlodziak77_bullet_white_games,
                  df_grooney_rapid_black_games,
                  df_grooney_rapid_white_games,
                  df_grooney_blitz_black_games,
                  df_grooney_blitz_white_games,
                  df_grooney_bullet_black_games,
                  df_grooney_bullet_white_games)%>% 
  mutate(ranking_gracz=ifelse(color=="white",white.elo,black.elo),ranking_przeciwnik=ifelse(color=="black",white.elo,black.elo),wynik=hero.points, link=link,data=date) %>% 
  select(ranking_gracz,ranking_przeciwnik,wynik,link,data,gracz,rodzaj,color)

# df_0, df_ruchygracza, openingi###########################

df_games<-rbind(df_tymekkk_blitz_black_games,
                df_tymekkk_blitz_white_games,
                df_Mlodziak77_rapid_black_games,
                df_Mlodziak77_rapid_white_games,
                df_Mlodziak77_blitz_black_games,
                df_Mlodziak77_blitz_white_games,
                df_Mlodziak77_bullet_black_games,
                df_Mlodziak77_bullet_white_games,
                df_grooney_rapid_black_games,
                df_grooney_rapid_white_games,
                df_grooney_blitz_black_games,
                df_grooney_blitz_white_games,
                df_grooney_bullet_black_games,
                df_grooney_bullet_white_games) %>% 
  select(gracz,rodzaj,color,opening_name=opening.name,id,points=hero.points) %>% 
  mutate(opening_name = ifelse(grepl(":", opening_name), substr(opening_name, 1, unlist(gregexpr(":", opening_name)) -1), opening_name)) %>%   
  mutate(opening_name=ifelse(opening_name=="GrÄ‚Ä1nfeld Defense","Grunfeld Defense",opening_name))%>%
  group_by(gracz,rodzaj,color,opening_name) %>% 
  mutate(n=n(),mean=mean(points))

df_moves<-rbind(df_tymekkk_blitz_black_moves,
                df_tymekkk_blitz_white_moves,
                df_Mlodziak77_rapid_black_moves,
                df_Mlodziak77_rapid_white_moves,
                df_Mlodziak77_blitz_black_moves,
                df_Mlodziak77_blitz_white_moves,
                df_Mlodziak77_bullet_black_moves,
                df_Mlodziak77_bullet_white_moves,
                df_grooney_rapid_black_moves,
                df_grooney_rapid_white_moves,
                df_grooney_blitz_black_moves,
                df_grooney_blitz_white_moves,
                df_grooney_bullet_black_moves,
                df_grooney_bullet_white_moves) %>%
  mutate(move=ifelse(color=="black",black,white),id=game.id) %>% 
  select(gracz,rodzaj,color,id,move) %>% 
  mutate(move=case_when(is.na(as.numeric(substr(move,nchar(move),nchar(move)))) == FALSE ~ substr(move,nchar(move)-1,nchar(move)),
                        (substr(move,1,5) == "O-O-O" & color=="black") ~ "c8d8",
                        (substr(move,1,5) == "O-O-O" & color=="white") ~ "c1d1",
                        (substr(move,1,3) == "O-O" & color=="black") ~ "g8f8",
                        (substr(move,1,3) == "O-O" & color=="white") ~ "g1f1",
                        substr(move,nchar(move)-1,nchar(move)-1) == "=" ~ substr(move,nchar(move)-3,nchar(move)-2),
                        substr(move,nchar(move)-2,nchar(move)-2) == "=" ~ substr(move,nchar(move)-4,nchar(move)-3),
                        substr(move,nchar(move),nchar(move)) == "+" ~ substr(move,nchar(move)-2,nchar(move)-1),
                        substr(move,nchar(move),nchar(move)) == "#" ~ substr(move,nchar(move)-2,nchar(move)-1),
                        TRUE ~ "nie")) %>% 
  filter(move!="nie")
df_add1<- filter(df_moves,(move=="c8d8" | move=="c1d1" | move=="g8f8" | move=="g1f1")) %>% 
  mutate(move=substr(move,1,2))
df_add2<- filter(df_moves,(move=="c8d8" | move=="c1d1" | move=="g8f8" | move=="g1f1")) %>% 
  mutate(move=substr(move,3,4))

df_moves<-rbind(filter(df_moves,(move!="c8d8" & move!="c1d1" &  move!="g8f8" & move!="g1f1")),df_add1,df_add2)
df_moves<-inner_join(df_moves,select(df_games,gracz,rodzaj,color,opening_name,id),by=c("gracz","rodzaj","color","id"))
df_moves<-df_moves %>%  
  group_by(gracz,rodzaj,color,opening_name,move) %>% 
  mutate(count=n(),id=NULL) %>% 
  distinct()
df_games<-df_games %>% 
  select(-c(id,points)) %>% 
  distinct()
openingi<-df_moves%>% 
  ungroup() %>% 
  distinct(opening_name) %>% 
  arrange(opening_name) %>% 
  pull(opening_name)

d<-length(openingi)

gracz<-rep(c("grooney","Mlodziak77","tymekkk"),times=1,each=384*d)
rodzaj<-rep(c("blitz","bullet","rapid")       ,times=3,each=128*d)
color<- rep(c("white", "black")               ,times=9,each=64*d)
opening_name<-rep(openingi                    ,times=18,each=64)
X<-rep(letters[1:8]                           ,times=18*d,each=8)
Y<-rep(as.character(1:8)                      ,times=144*d,each=1)
count<-rep(0                                ,times=1152*d,each=1)
df_moves_0<-data.frame(gracz,rodzaj,color,opening_name,X,Y,count)


df_moves<-df_moves %>% 
  ungroup() %>% 
  mutate(X=substr(move,1,1),Y=substr(move,2,2)) %>% 
  select(gracz,rodzaj,color,opening_name,X,Y,count) %>% 
  rbind(df_moves_0) %>% 
  distinct(gracz,rodzaj,color,opening_name,X,Y,.keep_all = TRUE) %>% 
  group_by(gracz,rodzaj,color,opening_name) %>%
  mutate(suma=sum(count)) %>%
  filter(suma!=0) %>% 
  select(-c(suma))
df_0<-df_games
df_ruchygracza<-df_moves

#write.csv########################################################################################
write.csv(df_plot1,"dane_obrobione/df_plot1.csv",row.names = FALSE)
write.csv(df_plot2,"dane_obrobione/df_plot2.csv",row.names = FALSE)
write.csv(df_heatmap,"dane_obrobione/df_heatmap.csv",row.names = FALSE)
write.csv(df_scatter,"dane_obrobione/df_scatter.csv",row.names = FALSE)
write.csv(df_0,"dane_obrobione/df_0.csv",row.names = FALSE)
write.csv(df_ruchygracza,"dane_obrobione/df_ruchygracza.csv",row.names = FALSE)
write.csv(openingi,"dane_obrobione/openingi.csv",row.names = FALSE)
