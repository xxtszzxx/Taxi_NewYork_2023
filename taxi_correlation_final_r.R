
---
title: "Wpływ warunków atmosferycznych na kursy taksówek w Nowym Jorku.Porównanie studium przypadku 22-29 stycznia 2023 i 10-17 maja 2023 roku."
authors: "Katarzyna Walczak, Julia Wiktorowicz"
date: "25 January 2024"
output: html_document
---

## Wybór bazy danych

Wybrane przez nas dane pochodzą z dwóch niezależnych od siebie źródeł. Dane dotyczące przejazdu taksówek pochodzą ze strony https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page?fbclid=IwAR36L_RzkjJy3pj7pcfXgZg8JbIAC_Hi4ewNbfqE-EjHBrbTAwy4Ku0EnJ0 natomiast dane pogodowe ze strony https://www.visualcrossing.com/weather/weather-data-service .

- Plik traktujący o przejazdach taksówek w Nowym Jorku pokazuje ilość przejazdów oraz dodatkowych rzeczy z nimi związanych (np. ilość osób, kierunek, płatności) każdego dnia w okresie 22-29/01/2023.
- Natomiast drugi plik  przedstawia warunki pogodowe (np. temperaturę - w stopniach C, prędkość wiatru) panujące w Nowym Jorku w okresie 22-29/01/2023.

W drugiej części projektu wczytane zostaną dodatkowo także dane dotyczące okresu 10-17/05/2023. Analogicznie więc:
- Plik traktujący o przejazdach taksówek w Nowym Jorku w okresie 10-17/05/2023.
- Oraz plik przedstawiający warunki pogodowe w okresie 10-17/05/2023.

## PROBLEM BADACZWY
Czy istnieje, a jeżeli tak to jaka, korelacja pomiędzy warunkami atmosferycznymi a kursowaniem taksówek w Nowym Jorku.

## PYTANIA BADAWCZE
1. Czy istnieje związek między liczbą pasażerów a liczbą przejazdów?
2. Czy istnieje związek między liczbą pasażerów a sumami napiwków?
3. Czy istnieje związek między sumami napiwków a liczbą przejazdów?
4. Czy istnieje związek pomiędzy liczbą przejazdów a temperaturą?
5. Czy istnieje związek pomiędzy liczbą przejazdów a opadami?
6. Czy istnieje związek pomiędzy liczbą przejazdów a prędkością wiatru?
7. Czy istnieje związek pomiędzy liczbą pasażerów a temperaturą?
8. Czy istnieje związek pomiędzy liczbą pasażerów a opadami?
9. Czy istnieje związek pomiędzy liczbą pasażerów a prędkością wiatru?
10. Czy istnieje związek pomiędzy napiwkami a temperaturą?
11. Czy istnieje związek pomiędzy napiwkami a opadami?
12. Czy istnieje związek pomiędzy napiwkami a prędkością wiatru?

## Krótki opis

Poniższy kod dokonuje analizy oraz wizualizacji danych z 2023 roku przy użyciu kilkunastu bibliotek. Finalne wizualizacje służą nam jako podstawy do dokonania słownego opisu danych i dojścia do konkluzji.Każdy segment kodu zostały opracowane w taki sam analogiczny sposób.

Projekt rozpoczynamy od analizy ilości przejazów, sum napiwków oraz ilości osób korzystających usług taksówek w Nowym Jorku w wybranmy przez nas zakresie czasowym. Na podstawie owych danych tworzymy wykresy i dokonujemy pierwszej analizy.
Tę część podsumowujemy tworząc korelację przejazdów,osób i napiwków, aby w realny sposób pokazać pokrycie się danych. 

Następna część dotyczy danych pogodowych z tego samego okresu czasowego. W pierwszej kolejności została stworzona nowa tabelka z danymi, które bierzemy pod uwagę w naszej analizie. Następnie stworzony został wykres pokazujący zakres temperatur, prędkości wiatru i opadów.
Następne wykresy to korelacja warunków pogodowych z liczbami pasażerów, ilościa przejazdów i sumami uzyskanych napiwków. 

Projekt został podzielony na trzy części: pierwsza zakłada powyżej opisaną analizę dla wybranego tygodnia w styczniu (22-29/01/2023); 
druga część to analogicznie przeprowadzona analiza dla maja (10-17/05/2023), a ostatnia do porównanie i podsumowanie wyników.


## Początkowy etap to wczytanie koniecznych pakietów, które pozwolą na odpowiednie przetowrzenie oraz obrobienie danych.

library(readxl)
library(janitor)
library(tidyr)
library(tidyverse)
library(RColorBrewer)
library(palmerpenguins)
library(reshape2)
library(knitr)


######### I CZĘŚĆ ##########
#### ANALIZA WPŁYWU WARUNKÓW POGODOWYCH NA KURSY TAKSÓWEK W STYCZNIU ####

## Wczytanie danych pogodowych oraz przejazdów taksówek dla stycznia.
library(readr)
weather_data_january <- read_csv("/Users/kasiawalczak/Desktop/proj_zal/New York City,USA 2023-01-22 to 2023-01-29.csv")

head(weather_data_january)

taxi_january <- read_csv("/Users/kasiawalczak/Desktop/proj_zal/taxi_january.csv", 
                         col_types = cols(...1 = col_number(), 
                                          passenger_count = col_number(), trip_distance = col_number(), 
                                          RatecodeID = col_number(), PULocationID = col_number(), 
                                          DOLocationID = col_number(), payment_type = col_number(), 
                                          fare_amount = col_number(), extra = col_number(), 
                                          mta_tax = col_number(), tip_amount = col_number(), 
                                          tolls_amount = col_number(), improvement_surcharge = col_number(), 
                                          total_amount = col_number(), congestion_surcharge = col_number(), 
                                          airport_fee = col_number(), year = col_double(), 
                                          hour_of_day = col_number(), trip_duration = col_number()))

head(taxi_january)



## DZIENNA ILOŚĆ KURSÓW TAKSÓWEK W OKRESIE 22-29/01/2023

#Kod zlicza ilość przejazdów w okresie 22-29/01/2023 wykorzystując kolumnę "day" oraz zapisuje i wyświetla dane w tabelce. 
library(dplyr)

as.numeric(taxi_january$day)
taxi_january_day <- taxi_january %>% 
  group_by(Day = taxi_january$day) %>% 
  summarize(total_rides = n())

View(taxi_january_day)

#Pokazanie największej i najmniejszej wartość ilości przejazdów aby użyć jej w podsumowaniach wykresów 
max(taxi_january_day$total_rides)
min(taxi_january_day$total_rides)

# Obliczenie średniej liczby przejazdów w celu użycia jej w podsumowaniach
mean_rides_january <- mean(taxi_january_day$total_rides)

# Wykres prezentujący ilość przejazdów każdego dnia w okresie 22-29/01/2023
library(ggplot2)

ggplot(taxi_january_day, aes(x = Day , y= total_rides)) + 
  geom_col (color='orange', alpha=.8, position="dodge", fill="brown") + xlab("Day") +
  ylab("Total Rides") + coord_flip() + labs(title="Total Rides per Day in 22-29/01/2023")
scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#PODSUMOWANIE WYKRESU
Wykres został stworzony na podstawie danych z pliku "taxi_january_day". Przedstawia nam on dane na temat ilości przejazdów w okresie 22-29/01/2023. 
Jak łatwo można zauważyć tendencja wzrostowa przypada na 24-28 stycznia z czego dniem, w którym widzimy największe oblężenie jest 26 stycznia, jest to ponad 110 tysięcy przejazdów, a dokłdniej 110263.
Podobny wynik choć niższy przypada na 27 oraz 28 stycznia. Najmniejsza ilość przypada na dni 22, 23 oraz 29 stycznia. Przejazdy w tych dniach osiągają podobne wyniki w okolicach ponad 80 tysięcy przejazdów.
Średnia liczba przejazdów wyniosła dla badanego tygodnia w styczniu 98 176.



## DZIENNA ILOŚĆ PASAŻERÓW TAKSÓWEK W OKRESIE 22-29/01/2023

#Kod zlicza liczbę pasażerów w okresie 22-29/01/2023 wykorzystując kolumnę "passenger_count" oraz zapisuje i wyświetla dane w tabelce. 
taxi_january_passengers <- taxi_january %>% 
  group_by(Day = day) %>% 
  summarize(total_passengers = sum(passenger_count))

View(taxi_january_passengers)

#Pokazanie największej i najmniejszej wartość ilości pasażerów aby użyć jej w podsumowaniach wykresów 
max(taxi_january_passengers$total_passengers) 
min(taxi_january_passengers$total_passengers) 

# Obliczenie średniej liczby pasażerów w celu użycia jej w podsumowaniach
mean_passengers_january <- mean(taxi_january_passengers$total_passengers)

# Wykres prezentujący ilość pasażerów każdego dnia w okresie 22-29/01/2023
ggplot(taxi_january_passengers, aes(x = Day , y= total_passengers)) + 
  geom_col (color='brown', alpha=.5, position="dodge", fill="pink") + xlab("Day") +
  ylab("Passengers Count") + coord_flip() + labs(title="Passengers Count in 22-29/01/2023")
scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#PODSUMOWANIE WYKRESU
Wykres przedstawia liczbę pasażerów taksówek w okresie 22-29/01/2023. Jak łatwo można zauważyć najwyższa liczba pasażerów przypada na dzień 28 stycznia jest to dokadnie 156 941 osób. 
Równie wysoko plasuje się 26 oraz 27 stycznia liczba w obu dniach dochodzi do około 150 000. 
Natomiast najniższa liczba pasażerów pojawiła się 23 stycznia ijest to dokładnie 113 566.Różnica pomiędzy 28 stycznia a 23 to 43 357.
Średnia liczba pasażerów wyniosła dla badanego tygodnia w styczniu 134 378.



## DZIENNA SUMA PRZYZNANYCH NAPIWKÓW W OKRESIE 22-29/01/2023

#Kod zlicza sumę napiwków w okresie 22-29/01/2023 wykorzystując kolumnę "tip_amount" oraz zapisuje i wyświetla dane w tabelce. 
taxi_january_tips <- taxi_january %>% 
  group_by(Day = day) %>% 
  summarize(total_tip = sum(tip_amount))

View(taxi_january_tips)

#Pokazanie największej i najmniejszej sumy napiwków, w celu użycia ich w podsumowaniach wykresów.
max(taxi_january_tips$total_tip) 
min(taxi_january_tips$total_tip) 

# Obliczenie średniej sumy napiwków w celu użycia jej w podsumowaniach.
mean_tips_january <- mean(taxi_january_tips$total_tip)

# Wykres prezentujący sumę napiwków każdego dnia w okresie 10-17/05/2023
ggplot(taxi_january_tips, aes(x = Day , y= total_tip)) + 
  geom_col (color='brown', alpha=.5, position="dodge", fill="deeppink4") + xlab("Day") +
  ylab("Total Tips") + coord_flip() + labs(title="Total Tips in 22-29/01/2023")
scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#PODSUMOWANIE WYKRESU
Wykres przedstawia sumy napiwków przyznanych w okresie 22-29/01/2023. Jak można zauważyć najwyższa suma napiwków została przyznana 28 stycznia i jest to dokadnie 381 044.4 dolarów.
Trochę mniej, bo niecałe 360 tysięcy dolarów uzyskano 27 stycznia. 
Natomiast najniższa suma napiwków została przyznana 23 stycznia i wyniosła dokładnie 292 555 dolarów. Różnica pomiędzy napiwkami z 28 stycznia a 23 stycznia to aż 88 489.4, czyli niecałe 89 tysięcy dolarów.
Średnia suma napiwków wyniosła dla badanego tygodnia w styczniu 328 926.1 dolara.


# STWORZENIE TABELI Z WSZYSTKIMI PRZEJAZDAMI, PASAŻERAMI ORAZ NAPIWKAMI (NA POTRZEBY KORELACJI)
correlation_rides_passengers_tips_january <- data.frame(
  Passengers = taxi_january_passengers$total_passengers,
  Rides = taxi_january_day$total_rides,
  Tips = taxi_january_tips$total_tip)

View(correlation_rides_passengers_tips_january)

# Korelacja pomiędzy ilością przejazdów a ilością pasażerów w okresie 22-29/01/2023
rides_passengers_january <- cor(correlation_rides_passengers_tips_january$Passengers, correlation_rides_passengers_tips_january$Rides)

cat("Correlation:", rides_passengers_january, "\n")

# Wykres przedstawiający korelację 
library("ggpubr")
ggscatter(correlation_rides_passengers_tips_january, x = "Passengers", y = "Rides", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Passengers", ylab = "Rides")

# Podsumowanie korelacji pomiędzy ilością przejazdów a ilością pasażerów w okresie 22-29/01/2023
Korelacja wyniosła 0.9065327 co oznacza,że korelacja jest silna.


# Korelacja pomiędzy ilością przejazdów a sumą napiwków w okresie 22-29/01/2023
rides_tips_january <- cor(correlation_rides_passengers_tips_january$Tips, correlation_rides_passengers_tips_january$Rides)

cat("Correlation:", rides_tips_january, "\n")

# Wykres przedstawiający korelację 

ggscatter(correlation_rides_passengers_tips_january, x = "Tips", y = "Rides", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Tips", ylab = "Rides")

# Podsumowanie korelacji pomiędzy ilością przejazdów a sumą napiwków w okresie 22-29/01/2023
Korelacja wyniosła 0.9483723 co oznacza,że korelacja jest silna. Jest silniejsza niż korelacja pomiędzy ilością przejazdów a liczbą pasażerów.


# Korelacja pomiędzy liczbą pasażerów a sumą napiwków w okresie 22-29/01/2023
passengers_tips_january <- cor(correlation_rides_passengers_tips_january$Tips, correlation_rides_passengers_tips_january$Passengers)

cat("Correlation:", passengers_tips_january, "\n")

# Wykres przedstawiający korelację 
ggscatter(correlation_rides_passengers_tips_january, x = "Tips", y = "Passengers", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Tips", ylab = "Passengers")

# Podsumowanie korelacji pomiędzy ilością przejazdów, a ilością pasażerów w okresie 22-29/01/2023
Korelacja wyniosła 0.7934185 co oznacza,że korelacja jest umiarkowana. Nie jest ona jednak tak silna, 
jak w przypadku korelacji między sumą napiwków a ilością przejazdów lub między ilością przejazdów a liczbą pasażerów.



##STOWRZENIE NOWEJ TABELKI Z WARTOŚCIAMI POGODOWYMI, KTÓRE INTERESUJĄ NAS W STOSUNKU DO NASZEGO MODELU ANALIZY

#Nowa tabelka składa się z następujących danych: nazwy miasta, daty, temperatury, prędkości wiatru oraz ogólnie opisanych warunków.
weather_data_january_new <- data.frame(
  Name = weather_data_january$name,
  DateTime = weather_data_january$datetime,
  Temp = weather_data_january$temp,
  Rain = weather_data_january$precip,
  WindSpeed = weather_data_january$windspeed,
  Conditions = weather_data_january$description)

View(weather_data_january_new)

# Dodoatkowo sprawdzam najwyższą, najniższą i średnią wartość temperatury aby użyć jej w podsumowaniach 
max(weather_data_january_new$Temp) 
min(weather_data_january_new$Temp) 
mean(weather_data_january_new$Temp)

# Wykres prezentujący temperatury w okresie 22-29/01/2023
ggplot(weather_data_january_new, aes(x = DateTime , y= Temp)) + 
  geom_col (color='orange', alpha=.5, position="dodge", fill="brown") + xlab("Date Time") +
  ylab("Temperature") + coord_flip() + labs(title="Temperature in 22-29/01/2023")
scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#PODSUMOWANIE WYKRESU
Wykres przedstawia temperatury panujące w Nowym Jorku w okresie 22-29/01/2023. Jak łatwo można zauważyć najniższa temperatura przypada na dzień 23 stycznia jest lekko ponad 3 stopnie.
Druga najniższa temperatura przypada na 22 stycznia i jest prawie 4 stopnie. 
Natomiast najwyższa temperatura pojawiła się 29 stycznia i jest to ponad 8 stopni. Względnie wysoka temperatura pojawia się również 26 stycznia, prawie 8 stopni. 
Średnia temepratur w badanym tygodniu stycznia wyniosła 5.35 stopnia C.


# Dodatkowo sprawdzam najwyższą,najniższą i średnią prędkość wiatru, aby użyć jej w podsumowaniach.
max(weather_data_january_new$WindSpeed) 
min(weather_data_january_new$WindSpeed) 
mean(weather_data_january_new$WindSpeed)

# Wykres prezentujący prędkość wiatru w okresie 22-29/01/2023
ggplot(weather_data_january_new, aes(x = DateTime , y= WindSpeed)) + 
  geom_col (color='orange', alpha=.5, position="dodge", fill="lightpink") + xlab("Date Time") +
  ylab("WindSpeed") + coord_flip() + labs(title="WindSpeed in 22-29/01/2023")
scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#PODSUMOWANIE WYKRESU 
Wykres przedstawia prędkość wiatru w Nowym Jorku w okresie 22-29/01/2023. Jak łatwo można zauważyć najsłabszy wiatr przypada na 29 stycznia, kiedy to wyniósł 16.2 km/h.
Najsilniejszy wiatr można było zoabserwować 23 stycznia, kiedy to wyniósł 37.1 km/h. 
Średnia prędkość wiatru wyniosła 24.1625 km/h. 


# Dodatkowo sprawdzam najwyższą,najniższą i średnią ilość opadów, aby użyć jej w podsumowaniach.
max(weather_data_january_new$Rain) 
min(weather_data_january_new$Rain) 
mean(weather_data_january_new$Rain)

# Wykres prezentujący średnią ilość opadów w okresie 22-29/01/2023
ggplot(weather_data_january_new, aes(x = DateTime , y= Rain)) + 
  geom_col (color='orange', alpha=.5, position="dodge", fill="deepskyblue3") + xlab("Date Time") +
  ylab("Rain") + coord_flip() + labs(title="Rain in 22-29/01/2023")
scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#PODSUMOWANIE WYKRESU 
Wykres przedstawia sumy opadów w Nowym Jorku w okresie 22-29/01/2023. Jak można zauważyć 24, 27 i 29 stycznia nie zaobserwowano żadnych opadów. 
Najsłabsze opady miały miejsce 28 stycznia, kiedy to wysniosły 0.699 mm.
Największe opady miały miejsce 25 stycznia, kiedy to wyniosły aż 20.68 mm.
Średnia suma opadów wyniosła 7.068125 mm.



# STWORZENIE TABELI NA POTRZEBY KOREACJI CZYNNIKÓW POGODOWYCH Z KURSAMI TAKSÓWEK DLA STYCZNIA
correlation_df_january <- data.frame(
  Month = "January",
  Day = taxi_january_day$Day,
  Passengers = taxi_january_passengers$total_passengers,
  Rides = taxi_january_day$total_rides,
  Tips = taxi_january_tips$total_tip,
  Temp = weather_data_january_new$Temp,
  Rain = weather_data_january_new$Rain,
  WindSpeed = weather_data_january_new$WindSpeed)

View(correlation_df_january)

### KORELACJA TEMPERATURY Z KURSOWANIEM TAKSÓWEK DLA STYCZNIA ###

# Korelacja ilości przejazdów oraz temperatury w okresie 22-29/01/2023
temp_rides_january <- cor(correlation_df_january$Temp, correlation_df_january$Rides)

cat("Correlation:", temp_rides_january, "\n")

# Wykres przedstawiający korelację 
ggscatter(correlation_df_january, x = "Temp", y = "Rides", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Temperature", ylab = "Rides")

# INTERPRETACJA KORELACJI PRZEJAZDÓW ORAZ TEMPERATURY W OKRESIE 22-29/01/2023
Korelacja wyniosła 0.203485, co oznacza, że jest to słaba korelacja.


# Korelacja ilości pasażerów oraz temperatury w okresie 22-29/01/2023
temp_passengers_january <- cor(correlation_df_january$Temp, correlation_df_january$Passengers)

cat("Correlation:", temp_passengers_january, "\n")

# Wykres przedstawiający korelację 
ggscatter(correlation_df_january, x = "Temp", y = "Passengers", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Temperature", ylab = "Passengers")

# INTERPRETACJA KORELACJI ILOŚCI PASAŻERÓW ORAZ TEMPERATURY W OKRESIE 22-29/01/2023
Korelacja wyniosła 0.3184567, co oznacza, że jest dośc słaba. jest jednak silniejsza niż korelacja między ilością przejazdów a temperaturą.


# Korelacja napiwków oraz temperatury w okresie 22-29/01/2023
temp_tips_january <- cor(correlation_df_january$Tips, correlation_df_january$Temp)

cat("Correlation:", temp_tips_january, "\n")

# Wykres przedstawiający korelację 
ggscatter(correlation_df_january, x = "Temp", y = "Tips", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Temperature", ylab = "Tips")


# INTERPRETACJA KORELACJI SUMY NAPIWKÓW ORAZ TEMPERATURY W OKRESIE 22-29/01/2023
Korelacja wyniosła 0.2360965, co oznacza, że jest słaba. Jest jednak nieznacznie silniejsza niż korelacja między ilością przejazdów a temperaturą.



### KORELACJA PRĘDKOŚCI WIATRU Z KURSOWANIEM TAKSÓWEK DLA STYCZNIA ###

# Korelacja ilości przejazdów oraz prędkości wiatru w okresie 22-29/01/2023
windspeed_rides_january <- cor(correlation_df_january$WindSpeed, correlation_df_january$Rides)

cat("Correlation:", windspeed_rides_january, "\n")

# Wykres przedstawiający korelację 
ggscatter(correlation_df_january, x = "WindSpeed", y = "Rides", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Wind Speed", ylab = "Rides")

# INTERPRETACJA KORELACJI PRZEJAZDÓW ORAZ PRĘDKOŚCI WIATRU W OKRESIE 22-29/01/2023
Korelacja wyniosła 0.1855634, co oznacza, że jest słaba.


# Korelacja ilości pasażerów oraz prędkości wiatru w okresie 22-29/01/2023
windspeed_passengers_january <- cor(correlation_df_january$WindSpeed, correlation_df_january$Passengers)

cat("Correlation:", windspeed_passengers_january, "\n")

# Wykres przedstawiający korelację 
ggscatter(correlation_df_january, x = "WindSpeed", y = "Passengers", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Wind Speed", ylab = "Passengers")

# INTERPRETACJA KORELACJI ILOŚCI PASAŻERÓW ORAZ PRĘDKOŚĆI WIATRU W OKRESIE 22-29/01/2023
Korelaja wyniosła -0.1120329 jest ujemna i bardzo słaba. ujemna korelacja oznacza, że wraz ze wzrostem wartości jednej cechy obserwowany jest spadek wartości drugiej cechy.


# Korelacja napiwków oraz prędkości wiatru w okresie 22-29/01/2023
windspeed_tips_january <- cor(correlation_df_january$Tips, correlation_df_january$WindSpeed)

cat("Correlation:", windspeed_tips_january, "\n")

# Wykres przedstawiający korelację 
ggscatter(correlation_df_january, x = "WindSpeed", y = "Tips", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Wind Speed", ylab = "Tips")

# INTERPRETACJA KORELACJI SUMY NAPIWKÓW ORAZ PRĘDKOŚCI WIATRU W OKRESIE 22-29/01/2023
Korelacja wyniosła 0.2648601, co oznacza, że jest słaba. Jest jednak widocznie silniejsza niż korelacja pomiędzy liczbą przejazdów a prędkością wiatru.


### KORELACJA OPADÓW Z KURSOWANIEM TAKSÓWEK DLA STYCZNIA ###

# Korelacja ilości przejazdów oraz ilości opadów w okresie 22-29/01/2023
rain_rides_january <- cor(correlation_df_january$Rain, correlation_df_january$Rides)

cat("Correlation:", rain_rides_january, "\n")

# Wykres przedstawiający korelację 
ggscatter(correlation_df_january, x = "Rain", y = "Rides", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Rain", ylab = "Rides")

# INTERPRETACJA KORELACJI PRZEJAZDÓW ORAZ OPADÓW W OKRESIE 22-29/01/2023
Korelacja wyniosła -0.01618484, co oznacza, że jest bardzo słaba.


# Korelacja ilości pasażerów oraz opadów w okresie 22-29/01/2023
rain_passengers_january <- cor(correlation_df_january$Rain, correlation_df_january$Passengers)

cat("Correlation:", rain_passengers_january, "\n")

# Wykres przedstawiający korelację 
ggscatter(correlation_df_january, x = "Rain", y = "Passengers", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Rain", ylab = "Passengers")

# INTERPRETACJA KORELACJI ILOŚCI PASAŻERÓW ORAZ OPADÓW W OKRESIE 22-29/01/2023
Korelaja wyniosła -0.2632932, co oznacza, że jest dość słaba, ale widocznie silniejsza niż korelacja pomiędzy liczba przejazdów a opadami.


# Korelacja napiwków oraz opadów w okresie 22-29/01/2023
rain_tips_january <- cor(correlation_df_january$Tips, correlation_df_january$Rain)

cat("Correlation:", rain_tips_january, "\n")

# Wykres przedstawiający korelację 
ggscatter(correlation_df_january, x = "Rain", y = "Tips", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Rain", ylab = "Tips")

# INTERPRETACJA KORELACJI SUMY NAPIWKÓW ORAZ OPADÓW W OKRESIE 22-29/01/2023
Korelacja wyniosła 0.0452029, co oznacza, że jest bardzo słaba. po raz pierwszy jednak w przypadku korelacji opadów z wybranymi aspektami kursowania taksówek zauważalną jest niewielka korelacja dodatnia.
Oznacza to, że możliwe, że istnieje pewna zależność, że wraz ze wzrostem opadów rosną (lecz prawie niezauważalnie) napiwki.


######### II CZĘŚĆ ##########
#### ANALIZA WPŁYWU WARUNKÓW POGODOWYCH NA KURSY TAKSÓWEK W MAJU ####

# Analogicznie została przetworzona część danych dotyczących maja, a dokładnie okresu 10-17 maja 2023 roku.

## Wczytanie danych pogodowych oraz przejazdów taksówek dotyczących maja 2023 roku
weather_data_may <- read_csv("/Users/kasiawalczak/Desktop/proj_zal//New York City,USA 2023-05-10 to 2023-05-17.csv")

head(weather_data_january)

taxi_may <- read_csv("/Users/kasiawalczak/Desktop/proj_zal/taxi_may.csv", 
                     col_types = cols(...1 = col_number(), 
                                      passenger_count = col_number(), trip_distance = col_number(), 
                                      RatecodeID = col_number(), PULocationID = col_number(), 
                                      DOLocationID = col_number(), payment_type = col_number(), 
                                      fare_amount = col_number(), extra = col_number(), 
                                      mta_tax = col_number(), tip_amount = col_number(), 
                                      tolls_amount = col_number(), improvement_surcharge = col_number(), 
                                      total_amount = col_number(), congestion_surcharge = col_number(), 
                                      airport_fee = col_number(), year = col_double(), 
                                      hour_of_day = col_number(), trip_duration = col_number()))

head(taxi_may)

## DZIENNA ILOŚĆ KURSÓW TAKSÓWEK W OKRESIE 10-17/05/2023

#Kod zlicza ilość przejazdów w okresie 10-17/05/2023 wykorzystując kolumnę "day" oraz zapisuje i wyświetla dane w tabelce. 
as.numeric(taxi_may$day)

taxi_may_day <- taxi_may %>% 
  group_by(Day = taxi_may$day) %>% 
  summarize(total_rides = n())

View(taxi_january_day)

#Pokazanie największej i najmniejszej wartość ilości przejazdów aby użyć jej w podsumowaniach wykresów 
max(taxi_may_day$total_rides)
min(taxi_may_day$total_rides)

# Obliczenie średniej liczby przejazdów w celu użycia jej w podsumowaniach
mean_rides_may <- mean(taxi_may_day$total_rides)

# Wykres prezentujący ilość przejazdów każdego dnia w okresie 10-17/05/2023
ggplot(taxi_may_day, aes(x = Day , y= total_rides)) + 
  geom_col (color='orange', alpha=.8, position="dodge", fill="lightblue1") + xlab("Day") +
  ylab("Total Rides") + coord_flip() + labs(title="Total Rides per Day in 10-17/05/2023")
scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#PODSUMOWANIE WYKRESU 
Wykres został stworzony na podstawie danych z pliku "taxi_may_day". Przedstawia nam on dane na temat ilości przejazdów w okresie 10-17/05/2023. 
Jak można zauważyć tendencja wzrostowa przypada na 15-17 maja z czego dniem, w którym widzimy największe oblężenie jest 17 maja, jest to ponad 126 tysięcy przejazdów, a dokłdniej 126313.
Podobny wynik choć niższy przypada na 11 maja. Widoczna jest także tendencja spadkowa, która przypada na 11-14 maja.
Najmniejsza ilość przypada na dni 14 i 15 maja, czyli niedzielę i poniedziałek. Przejazdy w tych dniach osiągają podobne wyniki w okolicach ponad 100 tysięcy przejazdów.
Średnia liczba przejazdów wyniosła dla badanego tygodnia w maju 116 300.


## DZIENNA ILOŚĆ PASAŻERÓW TAKSÓWEK W OKRESIE 10-17/05/2023

#Kod zlicza liczbę pasażerów w okresie 10-17/05/2023 wykorzystując kolumnę "passenger_count" oraz zapisuje i wyświetla dane w tabelce. 
taxi_may_passengers <- taxi_may %>% 
  group_by(Day = day) %>% 
  summarize(total_passengers = sum(passenger_count))

View(taxi_may_passengers)

#Pokazanie największej i najmniejszej wartość ilości pasażerów aby użyć jej w podsumowaniach wykresów 
max(taxi_may_passengers$total_passengers) 
min(taxi_may_passengers$total_passengers) 

# Obliczenie średniej liczby pasażerów w celu użycia jej w podsumowaniach
mean_passengers_may <- mean(taxi_may_passengers$total_passengers)

# Wykres prezentujący ilość pasażerów każdego dnia w okresie 22-29/01/2023
ggplot(taxi_may_passengers, aes(x = Day , y= total_passengers)) + 
  geom_col (color='brown', alpha=.5, position="dodge", fill="palegreen1") + xlab("Day") +
  ylab("Passengers Count") + coord_flip() + labs(title="Passengers Count in 10-17/05/2023")
scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#PODSUMOWANIE WYKRESU 
Wykres przedstawia liczbę pasażerów taksówek w okresie 10-17/05/2023. Jak można zauważyć najwyższa liczba pasażerów przypada na dzień 13 maja i jest to dokadnie 171 149 osób. 
Równie wysoko plasuje się 17 maja. Liczba pasażerów tego dnia dochodzi prawie do 170 tysięcy.
Natomiast najniższa liczba pasażerów miała miejsce 15 maja  i jest to dokładnie 139 702. Różnica pomiędzy 13 maja a 15 maja to 31 447.
Średnia liczba pasażerów wyniosła dla badanego tygodnia w maju 159860.4.


## DZIENNA SUMA PRZYZNANYCH NAPIWKÓW W OKRESIE 10-17/05/2023

#Kod zlicza sumę napiwków w okresie 10-17/05/2023 wykorzystując kolumnę "tip_amount" oraz zapisuje i wyświetla dane w tabelce. 
taxi_may_tips <- taxi_may %>% 
  group_by(Day = day) %>% 
  summarize(total_tip = sum(tip_amount))

View(taxi_may_tips)

#Pokazanie największej i najmniejszej sumy napiwków, w celu użycia ich w podsumowaniach wykresów.
max(taxi_may_tips$total_tip) 
min(taxi_may_tips$total_tip) 

# Obliczenie średniej sumy napiwków w celu użycia jej w podsumowaniach.
mean_tips_may <- mean(taxi_may_tips$total_tip)

# Wykres prezentujący sumę napiwków każdego dnia w okresie 10-17/05/2023
ggplot(taxi_may_tips, aes(x = Day , y= total_tip)) + 
  geom_col (color='brown', alpha=.5, position="dodge", fill="yellow4") + xlab("Day") +
  ylab("Total Tips") + coord_flip() + labs(title="Total Tips in 10-17/05/2023")
scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#PODSUMOWANIE WYKRESU 
Wykres przedstawia sumy napiwków przyznanych w okresie 10-17/05/2023. Jak można zauważyć najwyższa suma napiwków została przyznana 17 maja i jest to dokadnie 476 923.7 dolarów.
Równie wysoko plasuje się 11 maja, kiedy to w sumie przyznano prawie 462 tysiące dolarów w napiwkach.
Natomiast najniższa suma napiwków została przyznana 14 maja i wyniosła dokładnie 372 521.2 dolarów. Różnica pomiędzy napiwkami z 17 maja a 14 maja to aż 104 402.5, czyli ok. 105 tysięcy dolarów.
Średnia suma napiwków wyniosła dla badanego tygodnia w maju 425 259.8 dolarów.


# STWORZENIE TABELI Z WSZYSTKIMI PRZEJAZDAMI, PASAŻERAMI ORAZ NAPIWKAMI (NA POTRZEBY KORELACJI)
correlation_rides_passengers_tips_may <- data.frame(
  Passengers = taxi_may_passengers$total_passengers,
  Rides = taxi_may_day$total_rides,
  Tips = taxi_may_tips$total_tip)

View(correlation_rides_passengers_tips_may)

# Korelacja pomiędzy ilością przejazdów a ilością pasażerów w okresie 10-17/05/2023
rides_passengers_may <- cor(correlation_rides_passengers_tips_may$Passengers, correlation_rides_passengers_tips_may$Rides)

cat("Correlation:", rides_passengers_may, "\n")

# Wykres przedstawiający korelację 
ggscatter(correlation_rides_passengers_tips_may, x = "Passengers", y = "Rides", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Passengers", ylab = "Rides")

# Podsumowanie korelacji pomiędzy ilością przejazdów a ilością pasażerów w okresie 10-17/05/2023
Korelacja wyniosła 0.7762446 co oznacza,że korelacja jest silna, ale nie tak silna jak w styczniu.


# Korelacja pomiędzy ilością przejazdów a sumą napiwków w okresie 10-17/05/2023
rides_tips_may <- cor(correlation_rides_passengers_tips_may$Tips, correlation_rides_passengers_tips_may$Rides)

cat("Correlation:", rides_tips_may, "\n")

# Wykres przedstawiający korelację 
ggscatter(correlation_rides_passengers_tips_may, x = "Tips", y = "Rides", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Tips", ylab = "Rides")

# Podsumowanie korelacji pomiędzy ilością przejazdów a sumą napiwków w okresie 10-17/05/2023
Korelacja wyniosła 0.8774225 co oznacza,że korelacja jest silna. Jest też silniejsza niż korelacją miedzy ilością przejazdów a liczbą pasażerów.


# Korelacja pomiędzy liczbą pasażerów a sumą napiwków w okresie 10-17/05/2023
passengers_tips_may <- cor(correlation_rides_passengers_tips_may$Tips, correlation_rides_passengers_tips_may$Passengers)

cat("Correlation:", passengers_tips_may, "\n")

# Wykres przedstawiający korelację 
ggscatter(correlation_rides_passengers_tips_may, x = "Tips", y = "Passengers", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Tips", ylab = "Passengers")

# Podsumowanie korelacji pomiędzy liczbą pasażerów a sumą napiwków w okresie 10-17/05/2023
Korelacja wyniosła 0.4069147 co oznacza,że korelacja jest umiarkowana. Nie jest ona jednak tak silna, 
jak w przypadku korelacji między sumą napiwków a ilością przejazdów lub między ilością przejazdów a liczbą pasażerów.



##STOWRZENIE NOWEJ TABELKI Z Z WARTOŚCIAMI POGODOWYMI, KTÓRE INTERESUJĄ NAS W STOSUNKU DO NASZEGO MODELU ANALIZY

#Nowa tabelka składa się z następujących danych: nazwy miasta, daty, temperatury, prędkości wiatru oraz ogólnie opisanych warunków.
weather_data_may_new <- data.frame(
  Name = weather_data_may$name,
  DateTime = weather_data_may$datetime,
  Temp = weather_data_may$temp,
  Rain = weather_data_may$precip,
  WindSpeed = weather_data_may$windspeed,
  Conditions = weather_data_may$description)

View(weather_data_may_new)

# Dodatkowo sprawdzam najwyższą,najniższą i średnią wartość temperatury aby użyć jej w podsumowaniach 
max(weather_data_may_new$Temp) 
min(weather_data_may_new$Temp) 
mean(weather_data_may_new$Temp)

# Wykres prezentujący temperatury w okresie 10-17/05/2023
ggplot(weather_data_may_new, aes(x = DateTime , y= Temp)) + 
  geom_col (color='orange', alpha=.5, position="dodge", fill="royalblue1") + xlab("Date Time") +
  ylab("Temperature") + coord_flip() + labs(title="Temperature in 10-17/05/2023")
scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#PODSUMOWANIE WYKRESU 
Wykres przedstawia temperatury panujące w Nowym Jorku w okresie 10-17/05/2023. Jak łatwo można zauważyć najniższa temperatura przypada na dzień 10 maja, czyli 16.2 stopni C. 
Druga najniższa temperatura przypada na 17 maja i jest prawie 17 stopni. 
Natomiast najwyższa temperatura miała miejsce 12 i 13 maja i jest to ponad 23 stopnie. 
Średnia temperatura wyniosła 19.6375 stopnia C.


# Dodatkowo sprawdzam najwyższą,najniższą i średnią prędkość wiatru, aby użyć jej w podsumowaniach.
max(weather_data_may_new$WindSpeed) 
min(weather_data_may_new$WindSpeed) 
mean(weather_data_may_new$WindSpeed)

# Wykres prezentujący prędkość wiatru w okresie 10-17/05/2023
ggplot(weather_data_may_new, aes(x = DateTime , y= WindSpeed)) + 
  geom_col (color='orange', alpha=.5, position="dodge", fill="lightsalmon") + xlab("Date Time") +
  ylab("WindSpeed") + coord_flip() + labs(title="WindSpeed in 10-17/05/2023")
scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#PODSUMOWANIE WYKRESU 
Wykres przedstawia prędkość wiatru w Nowym Jorku w okresie 10-17/05/2023. Jak łatwo można zauważyć najsłabszy wiatr przypada na 13 maja, kiedy to wyniósł 11.3 km/h.
Najsilniejszy wiatr można było zoabserwować 16 maja, kiedy to wyniósł 27.2 km/h. 
Średnia prędkość wiatru wyniosła 19.7125 km/h. 


# Dodatkowo sprawdzam najwyższą ilość opadów.
max(weather_data_may_new$Rain) 

# W Wybranym tygodniu maja nie odnotowano żadnych opadów. 



# STWORZENIE TABELI NA POTRZEBY KOREACJI CZYNNIKÓW POGODOWYCH Z KURSAMI TAKSÓWEK DLA MAJA
correlation_df_may <- data.frame(
  Month = "May",
  Day = taxi_may_day$Day,
  Passengers = taxi_may_passengers$total_passengers,
  Rides = taxi_may_day$total_rides,
  Tips = taxi_may_tips$total_tip,
  Temp = weather_data_may_new$Temp,
  Rain = weather_data_may_new$Rain,
  WindSpeed = weather_data_may_new$WindSpeed)

View(correlation_df_may)


### KORELACJA TEMPERATURY Z KURSOWANIEM TAKSÓWEK DLA MAJA ###

# Korelacja ilości przejazdów oraz temperatury w okresie 10-17/05/2023
temp_rides_may <- cor(correlation_df_may$Temp, correlation_df_may$Rides)

cat("Correlation:", temp_rides_may, "\n")

# Wykres przedstawiający korelację 
ggscatter(correlation_df_may, x = "Temp", y = "Rides", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Temperature", ylab = "Rides")

# INTERPRETACJA KORELACJI PRZEJAZDÓW ORAZ TEMPERATURY W OKRESIE 10-17/05/2023
Korelacja wyniosła 0.0493114 co oznacza, że jest bardzo słaba. Na podstawie tego wyniku możemy śmiało stwierdzić, że nie ma dużego związku między
ilością przejazdów a temperaturą w maju. 


# Korelacja ilości pasażerów oraz temperatury w okresie 10-17/05/2023
temp_passengers_may <- cor(correlation_df_may$Temp, correlation_df_may$Passengers)

cat("Correlation:", temp_passengers_may, "\n")

# Wykres przedstawiający korelację 
ggscatter(correlation_df_may, x = "Temp", y = "Passengers", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Temperature", ylab = "Passengers")

# INTERPRETACJA KORELACJI ILOŚCI PASAŻERÓW ORAZ TEMPERATURY W OKRESIE 10-17/05/2023
Wyniosła 0.4813287, więc korelacja jest umiarkowana. Jest zdecydowanie większa niż w przypadku korelacji temperatury z przejazdami. 
Na podstawie tego wyniku możemy stwierdzić, że temperatura może mieć pewien wpływ na liczbę pasażerów taksówek. 


# Korelacja napiwków oraz temperatury w okresie 10-17/05/2023
temp_tips_may <- cor(correlation_df_may$Tips, correlation_df_may$Temp)

cat("Correlation:", temp_tips_may, "\n")

# Wykres przedstawiający korelację 
ggscatter(correlation_df_may, x = "Temp", y = "Tips", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Temperature", ylab = "Tips")

# INTERPRETACJA KORELACJI SUMY NAPIWKÓW ORAZ TEMPERATURY W OKRESIE 10-17/05/2023
Wyniosła -0.3395846, więc korelacja jest słaba. Dodatkowo korelacja jest ujemna, co może znaczyć, że w wybranym tygodnia maja wraz ze wzrostem temperatury maleją napiwki.



### KORELACJA PRĘDKOŚCI WIATRU Z KURSOWANIEM TAKSÓWEK DLA MAJA ###

# Korelacja ilości przejazdów oraz prędkości wiatru w okresie 10-17/05/2023
windspeed_rides_may <- cor(correlation_df_may$WindSpeed, correlation_df_may$Rides)

cat("Correlation:", windspeed_rides_may, "\n")

# Wykres przedstawiający korelację 
ggscatter(correlation_df_may, x = "WindSpeed", y = "Rides", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Wind Speed", ylab = "Rides")

# INTERPRETACJA KORELACJI PRZEJAZDÓW ORAZ PRĘDKOŚCI WIATRU W OKRESIE 10-17/05/2023
Korelacja wyniosła -0.2587434 co oznacza, że jest bardzo słaba. Na podstawie tego wyniku możemy śmiało stwierdzić, że nie ma dużego związku między
ilością przejazdów a prędkością wiatru. 


# Korelacja ilości pasażerów oraz prędkości wiatru w okresie 10-17/05/2023
windspeed_passengers_may <- cor(correlation_df_may$WindSpeed, correlation_df_may$Passengers)

cat("Correlation:", windspeed_passengers_may, "\n")

# Wykres przedstawiający korelację 
ggscatter(correlation_df_may, x = "WindSpeed", y = "Passengers", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Wind Speed", ylab = "Passengers")

# INTERPRETACJA KORELACJI ILOŚCI PASAŻERÓW ORAZ PRĘDKOŚCI WIATRU W OKRESIE 10-17/05/2023
Wyniosła -0.4544341, więc korelacja jest umiarkowana. Dodatkowo korelacja jes ujemna, co może sugerować, że wraz ze wzrostem prędkości wiatru maleje liczba pasażerów.


# Korelacja napiwków oraz prędkości wiatru w okresie 10-17/05/2023
windspeed_tips_may <- cor(correlation_df_may$Tips, correlation_df_may$WindSpeed)

cat("Correlation:", windspeed_tips_may, "\n")

# Wykres przedstawiający korelację 
ggscatter(correlation_df_may, x = "WindSpeed", y = "Tips", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Wind Speed", ylab = "Tips")

# INTERPRETACJA KORELACJI SUMY NAPIWKÓW ORAZ PRĘDKOŚCI WIATRU W OKRESIE 10-17/05/2023
Wyniosła 0.03925768, więc korelacja jest bardzo słaba.Można stwierdzić, 
że nie ma związku między sumami napiwków a prędkością wiatru w badanym majowym okresie.


### KORELACJA OPADÓW Z KURSOWANIEM TAKSÓWEK DLA MAJA ###
Jako, że w wybranym tygodniu maja nie odnotowano żadnych opadów koralacja dla wszystkich wspólczynników nie jest możliwa do obliczenia.



######### III CZĘŚĆ ##########
#### PORÓWNANIE WYNIKÓW I PODSUMOWANIE ####

# Pierwszym krokiem jest połaczenie tabel korelacji stycznia i maja w jedną, co umożliwi prezentację wyników.
correlation <- merge(correlation_df_january, correlation_df_may,
                     by = c("Month", "Day", "Passengers", "Rides", "Tips", "Temp", "Rain", "WindSpeed"), all=TRUE)

### KURSY TAKSÓWEK - PORÓWNANIE ###
# Na potrzeby badania należy porównać ze sobą kursowanie taksówek w wybranych przez nas okresach, tj. 22-29 stycznia i 10-17 maja 2023 roku.

# Porównanie liczby przejazdów
ggplot(correlation, aes(x = Day , y = Rides, color = Month)) + 
  geom_bar(stat = "identity", position="dodge", fill = "lightgrey") + 
  xlab("Day") + ylab("Total Rides") + labs(title="Total Rides per Day") +
  facet_wrap(~ Month)

# Porównanie średniej liczby przejazdów
mean_rides_may - mean_rides_january

# PODSUMOWANIE
Wyraźnie widać, że w wybranym tygodniu maja było znacznie więcej kursów niż w styczniu. Różnica między średnimi z obu tygodni to aż 18 124. 

# Porównanie liczby pasażerów
ggplot(correlation, aes(x = Day , y = Passengers, color = Month)) + 
  geom_bar(stat = "identity", position="dodge", fill = "lightgrey") + 
  xlab("Day") + ylab("Total Passengers") + labs(title="Total Passengers per Day") +
  facet_wrap(~ Month)

# Porównanie średniej liczby przejazdów
mean_passengers_may - mean_passengers_january

# PODSUMOWANIE
Wyraźnie widać, że w wybranym tygodniu maja było znacznie więcej pasażerów. Różnica między średnimi z obu tygodni to aż ponad 25 tysięcy osób.

# Porównanie sumy zebranych napiwków
ggplot(correlation, aes(x = Day , y = Tips, color = Month)) + 
  geom_bar(stat = "identity", position="dodge", fill = "lightgrey") + 
  xlab("Day") + ylab("Total Tips") + labs(title="Total Tips per Day") +
  facet_wrap(~ Month)

# Porównanie średniej liczby przejazdów
mean_tips_may - mean_tips_january

# PODSUMOWANIE
Wyraźnie widać, że w wybranym tygodniu maja taksówkarze dostawli znacznie więcej napiwków. Różnica między średnimi z obu tygodni to ponad 96 tysiące dolarów.


### KURSY TAKSÓWEK - PORÓWNANIE KORELACJI###
# Na potrzeby badania należy porównać ze sobą korelacje róznych zależnych w kursowaniu taksówek w wybranych przez nas okresach, tj. 22-29 stycznia i 10-17 maja 2023 roku.

# Korelacja pomiędzy liczbą przejazdów a liczbą pasażerów
# Przypomnienie wyników korelacji dla stycznia i maja
rides_passengers_january
rides_passengers_may

# Obliczenie korelacji ilości przejazdów a liczby pasażerów wspólnej dla obu tygodni
rp <- cor(correlation$Rides, correlation$Passengers)

cat("Correlation:", rp, "\n")

# Ukazanie korelacji na wykresie
ggscatter(correlation, x = "Rides", y = "Passengers", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Rides", ylab = "Passengers") + ggtitle("Rides and Passengers Correlation") 

# PODSUMOWANIE
Korelacje policzono osobno dla każdego tygodnia już wskazywały na silną zależność. 
W szczególności dotyczyło to tygodnia styczniowego. policzenie korelacji wspólnie dla obu tygodni potwierdza i wzamcnia tę tezę.
Korelacja wynosi dokładnie 0.9317622, co oznacza, że istnieje związek pomiędzy liczba przejazdów a liczba pasażerów. 
Dodatkowo wynik wskazuje, że wraz ze wzrostem liczby przejazdów rośnie liczba pasażerów.


# Korelacja pomiędzy liczbą przejazdów a sumą napiwków
# Przypomnienie wyników korelacji dla stycznia i maja
rides_tips_january
rides_tips_may

# Obliczenie korelacji ilości przejazdów a liczby pasażerów wspólnej dla obu tygodni
rt <- cor(correlation$Rides, correlation$Tips)

cat("Correlation:", rt, "\n")

# Ukazanie korelacji na wykresie
ggscatter(correlation, x = "Rides", y = "Tips", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Rides", ylab = "Tips") + ggtitle("Rides and Tips Correlation") 

# PODSUMOWANIE
Korelacje policzono osobno dla każdego tygodnia już wskazywały na silną zależność. 
W szczególności dotyczyło to tygodnia styczniowego. Policzenie korelacji wspólnie dla obu tygodni potwierdza i wzamcnia tę tezę.
Korelacja wynosi dokładnie 0.9423512, co oznacza, że istnieje związek pomiędzy liczba przejazdów a sumą uzyskanych napiwków. 
Dodatkowo wynik wskazuje, że wraz ze wzrostem liczby przejazdów rośnie suma napiwków.


# Korelacja pomiędzy liczbą pasażerów a sumą napiwków
# Przypomnienie wyników korelacji dla stycznia i maja
passengers_tips_january
passengers_tips_may

# Obliczenie korelacji ilości przejazdów a liczby pasażerów wspólnej dla obu tygodni
pt <- cor(correlation$Passengers, correlation$Tips)

cat("Correlation:", pt, "\n")

# Ukazanie korelacji na wykresie
ggscatter(correlation, x = "Passengers", y = "Tips", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Passengers", ylab = "Tips") + ggtitle("Passengers and Tips Correlation") 

# PODSUMOWANIE
Korelacje policzono osobno dla każdego tygodnia wskazywały na istnienei pewnej zależności.
W szczególności dotyczyło to ponownie tygodnia styczniowego. Korelacja policzona tylko dla maja nie wskazywała w tym przypadku na isnienie silnej, a jedynie umiarkowanej korelacji.
Policzenie korelacji wspólnie dla obu tygodni potwierdziło jednak zdecydowanie tezę o istnieniu zalezności.
Korelacja wynosi dokładnie  0.8335365, co oznacza, że istnieje związek pomiędzy liczba pasażerów a sumą uzyskanych napiwków. Wynik korelacji nie jest jednak tak wysoki jak w wcześniejszych przypadkach.
Dodatkowo wynik wskazuje, że wraz ze wzrostem liczby pasażerów rośnie suma napiwków.


### KURSY TAKSÓWEK A POGODA - PORÓWNANIE KORELACJI###
Na potrzeby badania należy porównać ze sobą korelacje róznych zależnych w kursowaniu taksówek w wybranych przez nas okresach, tj. 22-29 stycznia i 10-17 maja 2023 roku, a warunkami pogody, które wzięłyśmy pod uwagę.

### Korelacja pomiędzy liczbą pasażerów a pogodą
#Przypomnienie wyników korelacji dla stycznia i maja
rides_passengers_january
rides_passengers_may

#Obliczenie korelacji liczby pasażerów a temperaturą

pt <- cor(correlation$Passengers, correlation$Temp)

cat("Correlation:", pt, "\n")

#Ukazanie korelacji na wykresie

ggscatter(correlation, x = "Passengers", y = "Temp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Passengers", ylab = "Temp") + ggtitle("Passengers and Temp Correlation") 

# PODSUMOWANIE
Korelacje między przejazdami a pasażerami policzono osobno dla każdego tygodnia wskazywały na silne zależności.W ramach przypomnienia dla stycznia korelacja wynosiła: 0.9065327 natomiast dla maja: 0.7762446.
W szczególności dotyczyło to ponownie tygodnia styczniowego. Korelacja policzona dla maja  wskazywała w tym przypadku na isnienie korelacji jednak delikatnie mniejszej niż w styczniu. 
Policzenie korelacji pasażerów,a warunków pogodowych wspólnie dla obu tygodni potwierdziło jednak zdecydowanie tezę o istnieniu zalezności. Korelacja wynosi dokładnie  0.7693067, co oznacza, że istnieje związek pomiędzy kursami taxówek,a warunkami pogodowymi co badałyśmy w projekcie. 

