# 1. Utworz funkcje: rankAccount <- function(dataFrame,colName,groupName,valueSort,num)
#  ktora bedzie zwracala dla danej tabeli(dataFrame) n wierszy 
#  posiadajace najwieksze wartosci(sortowanie po kolumnie valueSort) dla wybranej grupy
#  (konkretna wartosc komorki , np. "NAUCZYCIEL) z kolumny(colName) np. occupation-zawod.

library(dplyr)
#manually
#wynik <- na.omit(konta) %>% filter(occupation=='NAUCZYCIEL') %>% .[order(-.$age),] %>% slice(1:10)
#wynik

rankAccount<- function(colName, groupName, valueSort, num){
  df<- read.csv("konta.csv")
  wynik <- na.omit(df) %>% filter(.[[colName]]==groupName) %>% .[order(-.[[valueSort]]),] %>% slice(1:num)
}

result <- rankAccount("occupation", "NAUCZYCIEL","age", 15)
result
result <- rankAccount("occupation", "NAUCZYCIEL","saldo", 15)
result


#2. Tak jak w 1 tylko z uzyciem datachunku.
#  przyklad naglowka:
#  rankAccountBigDatatoChunk(filename = "usersAccounts.csv", 1000,"occupation", "NAUCZYCIEL", "saldo",10)

rankAccountBigDatatoChunk<- function(filePath = "konta.csv", size, colName, groupName, valueSort, num){
  fileConnection<- file(description = filePath, open="r")
  topDF <-NULL
  data<-read.table(fileConnection, nrows=size, header= TRUE, fill=TRUE, sep=",")
  columnsNames<-names(data)
  repeat{
    if(nrow(data) == 0){
      break
    }
    data<-na.omit(data)
    tempDF <- NULL
    if (is.null(topDF)){
      tempDF <- data
    }else{
      tempDF <- rbind(topDF,data)
    }
    topDF <- tempDF %>% filter(.[[colName]]==groupName) %>% .[order(-.[[valueSort]]),] %>% slice(1:num)
    data<-read.table(fileConnection, nrows=size, col.names = columnsNames, fill=TRUE, sep=",")
  }
  close(fileConnection)
  topDF
}

rankAccountBigDatatoChunk("konta.csv", 1000, "occupation", "NAUCZYCIEL","saldo",10)


# 3. SPRAWIDZIC CZY DA SIE ZROBIC TO SAMO W zapytaniu SQL dla takich wartosci jak: 
#    tabelaZbazyDanych,occupation, nauczyciel, saldo

# 3a. query results from SQLite
library(DBI)
library(RSQLite)

#manually
#dbp="konta.sql"
#con<- dbConnect(SQLite(),dbp)
#tableName="konta"
#colName ="occupation"
#groupName = "NAUCZYCIEL"
#valueSort = "saldo"
#num=10
#dbGetQuery(con,paste0("SELECT * FROM ",tablename," WHERE ",colName,"='",groupName,
#                      "' ORDER BY ",valueSort," DESC LIMIT ",num,";"))

rankAccountSQLite<-function(dbp, tableName,colName, groupName, valueSort, num){
  conSQLite<- dbConnect(SQLite(),dbp)
  dbGetQuery(conSQLite,paste0("SELECT * FROM ",tableName," WHERE ",colName,"='",groupName,
                        "' ORDER BY ",valueSort," DESC LIMIT ",num,";"))
}

rankAccountSQLite("konta.sql","konta","occupation", "NAUCZYCIEL","saldo",10)


# 3b. query results from Postgres
library(RPostgres)
library(rstudioapi)

connectMe<-function(typBazy=Postgres(), dbname="jvvcwymj", host="dumbo.db.elephantsql.com", user="jvvcwymj"){
  con <- dbConnect(typBazy, dbname=dbname, host=host, user=user, 
                 password=askForPassword("database password"))
}

#test polaczenia
conElephantSQL<-connectMe()
#tablename="suisides"
#dbGetQuery(con,paste0("SELECT COUNT(*) FROM ",tablename,";"))

rankSuicidesPostgreSQL<-function(conPostgres, tableName,colName, groupName, valueSort, num){
  dbGetQuery(conPostgres,paste0("SELECT * FROM ",tableName," WHERE ",colName,"='",groupName,
                                 "' ORDER BY ",valueSort," DESC LIMIT ",num,";"))
}

rankSuicidesPostgreSQL(conElephantSQL,"suisides","country","Albania","suicides_no",10)
