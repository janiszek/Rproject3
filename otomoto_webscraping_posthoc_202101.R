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

download.file(link, destfile = "scrapedpage.html", quiet=TRUE)
page <- read_html('scrapedpage.html')

result <- page %>% html_nodes(xpath = "//span[@class='offer-price__number ds-price-number']/span[1]") %>% html_text()
result <- as.integer(str_replace(result, " ", ""))




#Exercise 1: Downloading prices from first 5 pages
N = 5
results <- c()
link = 'https://www.otomoto.pl/osobowe/audi/a4/?search[order]=created_at%3Adesc&page='
pb <- progress_bar$new(total=N)
for(i in 1:N){
  download.file(paste0(link, i), destfile = "scrapedpage.html", quiet=TRUE)
  read_html('scrapedpage.html') %>%
    html_nodes(xpath = "//span[@class='offer-price__number ds-price-number']/span[1]") %>%
    html_text() %>%
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
    as.numeric() -> year
  
  results <- rbind(results, data.frame('price' = price, 'year' = year))
  pb$tick()
}

plot(x=results$year, y=results$price, pch = 19)

# Basic violin plot
results$yearfactor <- factor(results$year)
p <- ggplot(results, aes(x=yearfactor, y=price)) + 
  geom_violin(fill="red3")
p

results %>% group_by(year) %>% summarize(mean(price))


#Exercise 3: Get also mileage, engine capacity, fuel type
N = 5
results <- data.frame('price'=numeric(), 'year'=numeric(), 'mileage'=numeric(), 'fuel'=c(), 'region'=c())
pb <- progress_bar$new(total=N)
for(i in 1:N){
  download.file(paste0(link, i), destfile = "scrapedpage.html", quiet=TRUE)
  page_nodes <- read_html('scrapedpage.html') %>% html_nodes(xpath = '//*[@class="offer-item__content ds-details-container"]')
  
  for(node in page_nodes){
    node %>%
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
      html_nodes(xpath = './/*[@class="ds-location-region"]') %>%
      html_text() -> region
      
    
    if(length(price) == 0) price <- NA
    if(length(year) == 0) year <- NA
    if(length(mileage) == 0) mileage <- NA
    if(length(fuel) == 0) fuel <- NA
    if(length(region) == 0) region <- NA
    
    results <- rbind(results, data.frame('price' = price, 'year' = year, 'mileage' = mileage, 'fuel' = fuel,
                                         'region' = region))
  }
  pb$tick()
}
table(results$region)

#Exercise 4: Analyze what impacts the price
data <- results
data <- data[data$mileage < 2000000,]
data$year <- 2021 - data$year
#Pierwsza organoleptyczna weryfikacja sensowności danych
plot(data)

#Weryfikujemy hipotezę związku wieku auta z przebiegiem
cor.test(data$year, data$mileage)
#Sprawdzamy ile rocznie średnio auto pokonywało kilometrów
lm(mileage ~ year - 1, data=data)

#Wykresy skrzypcowe pokazujące rozkład cen Audi A4 w zależności od typu paliwa i województwa
p <- ggplot(data, aes(x=fuel, y=price)) + 
  geom_violin(fill="red3")
p

p <- ggplot(data, aes(x=region, y=price)) + 
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

#Exercise 5: (Metaphysical): When to buy a car?

