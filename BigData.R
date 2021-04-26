lengthOfFile<- function(filepath,systemLinuxUnix=FALSE){
  #if(.Platform$OS.type == "unix" )
  if ( systemLinuxUnix){
    l <- try(system(paste("wc -l",filepath),intern=TRUE))
    l<-strsplit(l,split=" ")
    l<-as.numeric(l[[1]])
    l
  }
  else{
    l<-length(count.fields(filepath))
    l
  }
}
lengthOfFile("konta.csv", FALSE)


#trzeba doinstalować na Windows obslgue wc aby TRUE zadzialalo
start_time <- Sys.time()
lengthOfFile("konta.csv",TRUE)
end_time <- Sys.time()
wyn1<-end_time - start_time

#wersja na Windows
start_time <- Sys.time()
lengthOfFile("konta.csv",FALSE)
end_time <- Sys.time()
wyn2<-end_time - start_time
print(wyn1)
print(wyn2)

#ladowanie calego pliku CSV do pamieci
konta<- read.csv("konta.csv")
View(konta)

#ladowanie porcjami (data chunk)
srednia<- function(filePath, columnName, header=TRUE, size, sep=","){
  #pamiętamy gdzie skonczylismy czytac dane
  fileConnection<- file(description = filePath, open="r")
  suma<-0
  counter<-0
  #param header pozwala odtworzyc naglowki dla kolejnych odczytow z pliku
  data<-read.table(fileConnection, nrows=size, header= header, fill=TRUE, sep=sep)
  columnsNames<-names(data)
  #repeat dziala dopoki nie ma break
  repeat{
    if(nrow(data) == 0){
      break
    }
    #omijamy puste rekordy
    data<-na.omit(data)
    suma<- suma + sum(data[[columnName]])
    #nrow zwraca ilosc wierszy w dataset 
    counter <- counter + nrow(data)
    data<-read.table(fileConnection, nrows=size, col.names = columnsNames, fill=TRUE, sep=sep)
  }
  close(fileConnection)
  #zwracamy srednia
  if (counter !=0)
    return (suma/counter)
  else
    return (0)
}

#size podaje ile rekordow wczytac naraz
srednia("konta.csv","saldo", header=TRUE, size = 100)


#uzycie R funkcji mean na calym dataset
mean(konta[["saldo"]], na.rm = TRUE)


#Czytanie do bazy
library(DBI)
library(RSQLite)

#wczytujemy na razie z NA
readToBase<- function(filePath, dbPath, tableName, header=TRUE, size,sep=",", deleteTable=TRUE){
  ap= !deleteTable
  ov= deleteTable
  
  fileConnection<- file(description = filePath, open="r")
  #polaczenie z baza
  dbConn<- dbConnect(SQLite(),dbPath)
  data<-read.table(fileConnection, nrows=size, header= header, fill=TRUE, sep=sep)
  columnsNames<-names(data)
  #czy w pierwszym kroku chcemy utworzyc nowa tabelke czy dopisywac wiersze
  dbWriteTable(conn=dbConn, name=tableName, data, append=ap, overwrite=ov)
  repeat{
    if (nrow(data)==0){
      break
    }
    data<-read.table(fileConnection, nrows=size, col.names = columnsNames, fill=TRUE, sep=sep)
    dbWriteTable(conn=dbConn, name=tableName,data,append=TRUE,overwrite=FALSE)
  }
  close(fileConnection)
  dbDisconnect(dbConn)
}

readToBase("konta.csv","konta.sql","konta",size=100)

#weryfikacja czy zgadza sie ilosc rekordow wczytanych do bazy
dbp="konta.sql"
con<- dbConnect(SQLite(),dbp)
tablename="konta"
#uwaga - to zwroci o 1 linie mniej niz lengthOfFile ze wzgledu na headers
dbGetQuery(con,paste0("SELECT COUNT(*) FROM ",tablename,";"))
lengthOfFile("konta.csv",FALSE)

#install.packages("RPostgres")
#install.packages("rstudioapi")
library(RPostgres)
library(rstudioapi)

connectMe<-function(typBazy=Postgres(), dbname="jvvcwymj", host="dumbo.db.elephantsql.com", user="jvvcwymj"){
  con<-dbConnect(typBazy, dbname=dbname, host=host, user=user, 
                 password=askForPassword("database password"))
  
}

#test polaczenia
con<-connectMe()


readToPostgres<- function(filePath, dbConn, tableName, header=TRUE, size,sep=",", deleteTable=TRUE){
  ap= !deleteTable
  ov= deleteTable
  
  fileConnection<- file(description = filePath, open="r")
  data<-read.table(fileConnection, nrows=size, header= header, fill=TRUE, sep=sep)
  columnsNames<-names(data)
  dbWriteTable(conn=dbConn, name=tableName, data, append=ap, overwrite=ov)
  repeat{
    if (nrow(data)==0){
      break
    }
    data<-read.table(fileConnection, nrows=size, col.names = columnsNames, fill=TRUE, sep=sep)
    dbWriteTable(conn=dbConn, name=tableName,data,append=TRUE,overwrite=FALSE)
  }
  close(fileConnection)
  dbDisconnect(dbConn)
}

dbConn<- connectMe()
readToPostgres("pjatk_su.csv",dbConn, "suisides",size=1000)
password="ZQgTZlzDRborabdd_zQtOQBh3JXCkD2y"

#sprawdzenie czy rekordy sa zaladowane do bazy Postgres
con<- connectMe()
tablename="suisides"
dbGetQuery(con,paste0("SELECT COUNT(*) FROM ",tablename,";"))


#instalacja gplayer (manipulacja) praz ggplot (wizualizacja)
#install.packages("tidyverse")
library(tidyverse)


suicidesFromFile<-read.csv("pjatk_su.csv")
nrow(suicidesFromFile)
object.size(suicidesFromFile)

#dostep do Postgres przez connection
con<- connectMe()
dbGetInfo(con)
dbListTables(con)
dbListFields(con,"suisides")
suicideTable<-tbl(con,"suisides")
#to jest polaczenie z tabela w bazie, nie sama tabela!!!
object.size(suicideTable)

#wybor rekordow z dplayer - LAZY QUERY
suicideTable%>%select(country, year, age, generation)
#ladujemy cala tabele do R - collect()
tabelaR<- suicideTable%>%select(everything())%>%collect()
View(tabelaR)

#rysuj wykres - data= cechy dotyczace wykresu, geom_bar = funkcja geometryczna rysujaca wykres
#aes = estetyka, country - zlicza ile bylo dla kazdego kraju wystapien rekordow
#coord_flip - zamiana osi
ggplot(data=suicideTable)+geom_bar(aes(x=country))+coord_flip()

ggplot(data=suicidesFromFile)+geom_bar(aes(x=country))+coord_flip()

#sprawdzamy co dziala szybciej 
start_time <- Sys.time()
ggplot(data=suicideTable)+geom_bar(aes(x=country))+coord_flip()
end_time <- Sys.time()
wyn1<-end_time - start_time

start_time <- Sys.time()
ggplot(data=suicidesFromFile)+geom_bar(aes(x=country))+coord_flip()
end_time <- Sys.time()
wyn2<-end_time - start_time

#oczywiscie wolniej dziala z uzyciem bazy danych
print(wyn1)
print(wyn2)


#filtrowanie na tabeli w R
tabelaPoland<-filter(tabelaR, country=="Poland")
View(tabelaPoland)

#filtrowanie warunkow dla tabeli z bazy
dataPoland <- suicideTable %>%
  filter(country == 'Poland') %>%
  collect()
view(dataPoland)

#histogram dla Polski
ggplot(data=dataPoland)+geom_bar(aes(x=country))
ggplot(data=dataPoland)+geom_bar(aes(x=year))

ggplot(data=dataPoland)+geom_point(aes(x=year,y=suicides_no))
