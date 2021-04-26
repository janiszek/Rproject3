#Go to otomoto
#Find your dreamcar
#Go to second page of results
#Copy link

library(rvest)
library(stringr)
library(progress)
library(ggplot2)
library(dplyr)

link = 'https://www.otomoto.pl/osobowe/audi/a4/'

#NIE DZIALA
download.file(link, destfile = "scrapedpage.html", quiet=TRUE)
page <- read_html('scrapedpage.html')
#page <- read_html(link)

#generyczny Xpath
result <- page %>% html_nodes(xpath = "//span[@class='offer-price__number ds-price-number']/span[1]") %>% html_text()
result <- as.integer(result, " ", "")
summary(result)


#Exercise 1: downloading prices from first 5 pages
N = 5
results <- c()
link = 'https://www.otomoto.pl/osobowe/audi/a4/?search[order]=created_at%3Adesc&page='
pb <- progress_bar$new(total=N)
for(i in 1:N){
  #paste0 - domyslnie nie uzywa spacji = paste('a','b',sep='')
  download.file(paste0(link, i), destfile = "scrapedpage.html", quiet=TRUE)
  read_html('scrapedpage.html') %>%
    html_nodes(xpath = "//span[@class='offer-price__number ds-price-number']/span[1]") %>%
    html_text() %>%
    #w pipe . oznacza wynik z poprzedniego pipe'u
    str_replace(., " ", "") %>%
    as.numeric() -> results_i
  results <- c(results, results_i)
  pb$tick()
}
summary(results)
plot(density(results), lwd=2, col='blueviolet')

#Exercise 2: Downloading prices and year from all pages
results <- data.frame('price'=numeric(), 'year'=numeric())
pb <- progress_bar$new(total=N)
for(i in 1:N){
  download.file(paste0(link, i), destfile = "scrapedpage.html", quiet=TRUE)
  page_nodes <- read_html('scrapedpage.html') %>% html_nodes(xpath = '//*[@class="offer-item__content ds-details-container"]')
  
  page_nodes %>%
    html_nodes(xpath = "//span[@class='offer-price__number ds-price-number']/span[1]") %>%
    html_text() %>%
    str_replace(., " ", "") %>%
    as.numeric() -> price
  
  page_nodes %>%
    html_nodes(xpath = '//*[@data-code="year"]') %>%
    html_text() %>%
    str_extract(., regex('\\d{4}')) %>%
    #trzeba castowac na numeric bo inaczej traktuje to jako factor
    as.numeric() -> year
  
  results <- rbind(results, data.frame('price' = price, 'year' = year))
  pb$tick()
}

#wykres zaleznosci ceny od roku produkcji
plot(x=results$year, y=results$price, pch = 19)

#violin plot (skrzypcowy) - dla kazdego roku narysowany jest wykres gestosci 
results$yearfactor <-factor(results$year)
p <- ggplot(results, aes(x=yearfactor, y=price))+
  geom_violin(fill="red3")
p

results %>% group_by(year) %>% summarize(mean(price))

#Exercise 3: Get also mileage, engine capacity, fuel type
#str_replace - zamienia tylko pierwszy przypadek
#str_replace_all - zamienia wszystkie wystapienia

#UWAGA: moga pojawić się NA - trzeba je usunac - trzeba zejsc poziom nizej - zamienic zamiast page_nodes na node
N = 50
results <- data.frame('price'=numeric(), 'year'=numeric(), 'mileage'=numeric(), 
                      'fuel'=c(), 'region'=c())
pb <- progress_bar$new(total=N)
for(i in 1:N){
  download.file(paste0(link, i), destfile = "scrapedpage.html", quiet=TRUE)
  page_nodes <- read_html('scrapedpage.html') %>% html_nodes(xpath = '//*[@class="offer-item__content ds-details-container"]')
  
  for(node in page_nodes){
    #wchodze w konkretny wezel oferty
    node %>%
      #uwaga - tutaj nie wyszukuje w ramach wezla podanego w pipe tylko w ramach calego kodu, 
      #wiec trzeba dodac . aby szukal w ramach dzieci tego wezla
      html_nodes(xpath = ".//span[@class='offer-price__number ds-price-number']/span[1]") %>%
      html_text() %>%
      str_replace_all(., " ", "") %>%
      as.numeric() -> price
    
    node %>%
      html_nodes(xpath = './/*[@data-code="year"]') %>%
      html_text() %>%
      str_extract(., regex('\\d{4}')) %>%
      as.numeric() -> year
    
    node %>%
      html_nodes(xpath = './/*[@data-code="mileage"]') %>%
      html_text() %>%
      str_replace_all(., " ", "") %>%
      str_extract(regex('[0-9]+')) %>%
      as.numeric() -> mileage
    
    node %>%
      html_nodes(xpath = './/*[@data-code="fuel_type"]/span') %>%
      html_text() -> fuel
      
    node %>%
      html_nodes(xpath = './/*[@data-code="ds-location-region"]') %>%
      html_text() -> region
    
    if(length(price) == 0) price <- NA
    if(length(year) == 0) year <- NA
    if(length(mileage) == 0) mileage <- NA
    if(length(fuel) == 0) fuel <- NA
    if(length(region) == 0) region <- NA
    
    results <- rbind(results, data.frame('price' = price, 'year' = year, 'mileage' = mileage, 
                                         'fuel'=fuel, 'region'=region))
  }
  pb$tick()
}

table(results$region)

#wersja Macieja Maczki - podejscie z cwiczen - SPRAWDZIC
zrobWierszRvest<-function(w,wektorLinkow){
  newUrl<-wektorLinkow[w]
  page<-read_html(newUrl)
  cena<-html_node(page,".offer-price")%>%html_attr('data-price')
  lst<- page%>% xml_find_all(xpath='//*[@class="offer-params__item"]')
  v<-lapply(lst, function(node) {
    # find the first span
    first_span_node = xml_find_first(node, "./span[@class='offer-params__label']")
    label = xml_text(first_span_node, trim = TRUE)
    
    # find the div/a
    value_node = xml_find_first(first_span_node, "./following-sibling::div/a")
    # check if the div/a exists
    if(length(value_node) != 0) {
      value <- xml_text(xml_find_first(value_node, "./@title"))
    } else {
      value <- paste0(xml_text(xml_find_all(node, "./div/text()")), collapse = " ")
    }
    c(label=label, value=value)
  })
  v<-unlist(v)
  indexy<-seq(1,length(v),1)
  nazwyKolumn<- v[indexy%%2==1]
  wartosci<- v[indexy%%2==0]
  df1<- data.frame (matrix(wartosci,nrow = 1,ncol=length(wartosci)) )
  names(df1) <- nazwyKolumn
  df1<-cbind(cena,df1)
}

#Exercise 4: Analyze what impacts the price
#Aby zrobić dobrą analizę należałoby wyczyscic NA oraz wartosci odstajace (np. przebieg 2013 000)
#szybki rzut oka na dane:
data <- results
data <- data[data$mileage<1000000,]
#rok produkcji zamieniami na wiek auta
data$year <- 2021 - data$year

#pokazuje ladne wykresy zaleznosci wymiarow miedzy soba - sprawdzanie sensownosci danych
plot(data)

#sprawdzanie korelacji - test Pearsona (tutaj mozemy użyć) - pokazuje cor 0.69
cor.test(data$year,data$mileage)
#szybki model regresji liniowej - wychodzi, ze sredniorocznie kierowcy przejezdzaja 10tys km
#tylko intercept (wyraz wolny) jest wysoki 80k
lm(mileage ~ year, data=data)
#lepszy jest bez intercept - srednio rocznie kierowcy przejezdzaja 16tys km
lm(mileage ~ year - 1, data=data)

plot(data$price,data$fuel)

#wykres skrzypcowy cena do rodzaju paliwa
p<-ggplot(data,aes(x=fuel,y=price))+
  geom_violin(fill="red3")
p

p<-ggplot(data,aes(x=region,y=price))+
  geom_violin(fill="red3")
p

#Modelowanie ceny auta za pomocą wieku auta (włączając efekty kwadratowe)
# Warto zwrócić uwagę, na ograniczoną skuteczność modelu wynikającą z jego struktury (powyżej pewnego wieku auta model traci sens)
fit <- lm(price ~ year + I(year^2), data=data)
summary(fit)
plot(x=data$year, y=data$price, pch=19)
lines(x = 0:30, y = predict(object = fit, data.frame(year = 0:30)), col = 'darkgoldenrod1', lwd=2)
#Modelowanie ceny auta za pomocą wieku auta (włączając wiek^2 i wiek^3)
# Warto zwrócić uwagę, że powyżej ~x=20 model traci sens
fit <- lm(price ~ year + I(year^2) + I(year^3), data=data)
summary(fit)
plot(x=data$year, y=data$price, pch=19)
lines(x = 0:30, y = predict(object = fit, data.frame(year = 0:30)), col = 'darkgoldenrod1', lwd=2)
#Liczymy pochodną dyskretną średniej ceny po wieku
data %>% group_by(year) %>% summarize(mean(price), n()) %>% as.data.frame(.) -> prices
plot(diff(prices[2:27,2]), type = "l")

#Exercise 5: (Metaphysical): when to buy a car?
