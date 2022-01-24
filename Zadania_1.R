#1. Napisz funkcję sprawdzająca czy 1 liczba jest podzielna przez druga użyj - %%
czy_podzielna <-function(a, b){
  if (a%% b ==0){
    print (paste0("liczba ",a," przez " , b," jest podzielna"))
  } else {
    print (paste0("liczba ",a," przez " , b," nie jest podzielna"))
  }
}

czy_podzielna(33,5)


#2. Pociąg z Lublina do Warszawy przejechał połowę drogi ze średnią prędkością 120 km/h.
# Drugą połowę przejechał ze średnią prędkością 90 km/h.
# Jaka była średnia prędkość pociągu.

# Vsr = 2/(1/v1 + 1/v2)



Vsr<- 2/( 1/120 + 1/90)

paste("Średnia prędkość pociągu",Vsr)



#3. Utwórz funkcję obliczającą współczynnik korelacji r Pearsona dla 2 wektorów o tej samej długości.
# Wczytaj dane plik dane.csv i oblicz współczynnik dla wagi i wzrostu. W komentarzu napisz co oznacza wynik.

setwd("/home/marcin/Dokumenty/PJAP/R")

dfDane <- read.csv("./dane.csv", TRUE,";")
View(dfDane)

plot(dfDane$wzrost, dfDane$waga, xlab = "wzrost",ylab="waga")
cor(dfDane$wzrost, dfDane$waga, method = "pearson")

# 0.9793459 - bardzo silna korelacja między zmiennymi. Korelacja dodatnia. 
# czyli gdy rośnie wzrost - rośnie również waga



#4. Napisz funkcję zwracającą ramke danych z danych podanych przez użytkownika 
# stworzDataFrame <- function(ile=1)
# W pierwszym wierszu użytkownik podaje nazwy kolumn. w kolejnych wierszach zawartość wierszy ramki danych ( tyle wierszy ile podaliśmy w argumencie ile.
# ile=1 oznacza, że gdy użytkownik nie poda żadnej wartości jako parametr, domyślna wartością będzie 1)

stworzDataFrame <-function(ile=1){
  prompt <- "Podaj nazwy kolumn rozdzielając je spacją.Po ostatniej naciśnij enter: "
  nameOfColumns <- strsplit(readline(prompt), " " )[[1]]
  
  df_start <-  data.frame(nameOfColumns )
  df <- t(df_start)
  df <- df[-1,]
  
  colnames(df) <- nameOfColumns
  rowNames<- character() 
  
  for (k in 1:ile)
  {
    
    prompt <- paste("Podaj wartości dla wiersza nr ",k  , " rozdzielając je spacją.Po ostatniej naciśnij enter:  ", sep=" ")
    wiersz <- strsplit(readline(prompt), " " )[[1]]
    df <- rbind(df, wiersz)
    rowNames <- c(rowNames, paste("wiersz", k, sep="_"))
    
  }
  df <- data.frame(df, row.names = rowNames)
  
  df 
  
}

stworzDataFrame(2)



#5 Napisz funkcję , która pobiera sciezkeKatalogu, nazweKolumny, jakaFunkcje, DlaIluPlikow i liczy: 
# mean, median,min,max w zależności od podanej nazwy funkcji w argumencie, z katologu który podaliśmy i z tylu plików ilu podaliśmy dla wybranej nazwy kolumny. 
# UWAGA: w podanych plikach R pobierając komórki nazwane liczbami R wstawi przed nazwy X. Funkcję przetestuj dla katalogu smogKrakow.zip.
# Wykonując obliczenia pomiń brakujące wartości.




liczZplikow <- function(sciezka,nazwaKolumny,jakaFunkcja="mean",DlaIluPlikow=1){ 
  
  listaPlikow <-  list.files(sciezka)
  
  liczbaPlikow <- length(listaPlikow)
  
  # brak plików do pobrania
  if (liczbaPlikow ==0){
    info =  paste("Nie znalazłem plików w podanej ścieżce", sciezka  ,sep=" ")
    print(info)
    return (NULL)
  }
  
  # niewlaściwa funkcja
  else if ( ! trimws(jakaFunkcja)%in%c("mean", "median","min","max")) {
    info =  paste("Nie obsługuję funkcji ", jakaFunkcja, ". Dopuszczalne: 'mean', 'median','min','max'"  ,sep=" ")
    print(info)
    return (NULL)
  }
  
  # niewłasciwa 'liczba' w wywołaniu funkcji
  else   if( ! is.numeric(DlaIluPlikow)){
    info =  paste(DlaIluPlikow, "nie jest liczbą całowitą. Musisz podać liczbę całkowitą",  sep=" ")
    print (info)
    return (NULL)
  }
  
  # za duża/mała  liczba w wywołaniu funkcji
  else   if(DlaIluPlikow > liczbaPlikow || DlaIluPlikow<1 ){
    info =  paste("Musisz podać liczbę plików między 1 a", liczbaPlikow  ,sep=" ")
    print (info)
    return (NULL)
  }
  
  
  nazwaKolumny <- paste('X',trimws(nazwaKolumny),sep = "")
  
  
  
  df <- data.frame()
  
  #na potrzeby określenia ile plików zostało wczytanych bo mialo badaną kolumnę
  dodanychPlikow = 0
  
  # wczytanie plikow
  for (i in 1: DlaIluPlikow) {
    plik <- paste(sciezka,listaPlikow[i], sep = "/")
    
    wczytanyPlik <- read.csv(plik)
    
    # w ktorej kolumnie występuje szukana kolumna
    nrKolumny <-  grep( nazwaKolumny, colnames(wczytanyPlik))
    
    
    # jeżeli przekazana kolumna znajduje sie we wczytanym pliku to go dodaje do df
    
    if (length(nrKolumny)) {
      
      df <-   rbind(df, wczytanyPlik[nrKolumny])
      
      dodanychPlikow = dodanychPlikow+1
      
      info =  paste("Plik", plik , "wczytany" ,sep=" ")
      print (info)
      
      
    }
    else {
      info =  paste("Plik", plik , "nie został wczytany" ,sep=" ")
      print (info)
    }
  }
  
  # print(cat("", sep="\n\n"))
  info =  paste("Wczytanych plików", dodanychPlikow , sep=" ")
  print (info)
  #print(cat("", sep="\n\n"))
  
  
  
  
  if (dodanychPlikow >0) {
    
    # podmiana nazwy kolumny
    colnames(df) <- "X"
    
    # i szukam właściwej fukncji
    if (trimws(jakaFunkcja) == "mean") {
      result<- mean( na.omit(df$X  , na.strings=c("","NA")))
    }
    else if (trimws(jakaFunkcja) == "median") {
      result<- median( na.omit(df$X, na.strings=c("","NA")))
    }
    else if (trimws(jakaFunkcja) == "min"){
      result<- min( na.omit(df$X , na.strings=c("","NA")))
    }
    else if (trimws(jakaFunkcja) == "max") {
      result<- max( na.omit(df$X, na.strings=c("","NA")))
    }
    
    
    info =  paste("Funkcja", jakaFunkcja, "dla kolumny",substr(nazwaKolumny,2,100),"zwraca" , result , sep=" ")
    print (info)
    
  }
  # jak nie to NULL
  else {
    info =  paste("Kolumny", trimws(nazwaKolumny), " nie znajduje sie w zaczytanych plikach"  ,sep=" ")
    print(info)
    return (NULL)
  }
}


# sprawdzenie

Sciezka = '/home/marcin/Dokumenty/PJAP/R/smogKrakow'

liczZplikow(Sciezka,'214_pm10',"max", 5)



#Lista plików w katalogu: 

#  list.files(sciezka)

# Omijanie na : na.omit(myDataFrame[[nazwaKolumny]])
#  Do złączania stringów: 
#  paste("string1","string2",sep="TU WSTAW SEPARATOR")
#  Gdy mamy, rózne oznaczenia NA w plikach możemy wykorzystać ( w tym wypadku pusty znak i NA:
#                                                               na.strings=c("","NA")
