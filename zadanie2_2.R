library(rvest)
library(dplyr)
library(plyr)
library(DBI)
library(RSQLite)


 

# zamiana pobranych parametrów jako lista na obiekt DataFrame
convert_params_to_df <-function(params){
  
  list_1 = c()
  list_2 = c()
  df = data.frame()
  
  for (i in 1:length(params) ) {
    if (i%%2 == 1) {
      list_1 <- append(list_1,gsub("[^[:alnum:]]", "_", params[[i]]  )  )
    }
    else
      list_2 <- append(list_2, params[[i]] )
  }
  
  #w pierszej liście jest zawsze o finanse więcej
  if(length(list_1) >length(list_2))
    list_1 <- list_1[-length(list_1)]
  
  
  df = data.frame(list_2)
  df = t(df)
  
  names(df) <- list_1
  colnames(df)<- list_1
  rownames(df) <- NULL
  
  return (df)
  
}

# pobranie parametrów samochodu z ogłoszenia
get_params <- function(car_link ){
  
  # car_link <- "https://www.otomoto.pl/oferta/kia-sorento-ID6Eiq3y.html"
  car_page <-  read_html(car_link)
  
  
  cena <- car_page %>% html_nodes(".offer-price__number") %>% .[[1]] %>%html_text2()
  id <- car_page %>% html_nodes("#ad_id")%>% .[[1]] %>%html_text2()
  data_dodania  <- car_page %>% html_nodes(".offer-meta__value:nth-child(1)") %>% .[[1]] %>%html_text2()
  
  # pierwsza lista parametrów
  params_1 <- car_page %>% html_nodes(".offer-params__list")%>% .[[1]] %>% html_text2()
  params_1_list <-  as.list(strsplit(params_1, split = "\n")[[1]])
  params_1_df <-  convert_params_to_df(params_1_list)
  
  
  # druga lista parametrów
  params_2 = car_page %>% html_nodes(".offer-params__list")%>% .[[2]]  %>% html_text2()
  params_2_list <-  as.list(strsplit(params_2, split = "\n")[[1]])
  params_2_df <-  convert_params_to_df(params_2_list)
  
  # łącze obie listy parametrów
  params_final_df <- data.frame()
  params_final_df<-data.frame( cbind(id, car_link, data_dodania, cena, params_1_df, params_2_df))
  
  # dodane opoznienie - za duzo zapytan i serwer zwracal blad
  Sys.sleep(0.1) 
  
  return (params_final_df)
}

# zapis do bazy danych
writeToDb <- function (carsDf, tableName) {
  
  columnNames<-names(carsDf)
  
  dbConn<-dbConnect(SQLite(),"cars.sqlite")
  
  
  #### sprawdzenie czy istnieje tabela i jak tak to pobranie z niej listy kolumn
  # a jak nie ma to przypisuje z DataFrame - nie ma jeszcze tabeli, alter nie ma sensu
  # kolumny zostaną stworzone insertem
  
  if ( dbExistsTable(dbConn, tableName) ) {
    dbColumns <- dbListFields(dbConn, tableName )
  } else {
    dbColumns <- columnNames
  }
  
  
  # dodanie brakujących kolumn gdy są jakieś nowe w DataFrame:
  
  for (columnName in  setdiff (columnNames, dbColumns ) )  {
    
    sql <-  paste("ALTER TABLE suv ADD COLUMN", columnName , "text;")
    #print(sql)
    dbExecute(dbConn, sql)
  }
  
  dbWriteTable(dbConn,name=tableName,value=carsDf,append=TRUE,overwrite=FALSE)
  dbDisconnect(dbConn)
}

# link outomoto z ktorego będą pobierane ogłoszenia
otomoto_link <- "https://www.otomoto.pl/osobowe/seg-suv"


cars <-  data.frame()
page <-  read_html(otomoto_link)
 

# liczba stron ogłoszeń
subpages <- page %>% html_nodes(".ooa-1t3tmog+ .ooa-wak9h6 span" )  %>% html_text2()

#co ile stron wrzuca do bazy danych:
subpagesToDB <- 3
#licznik stron wczytanych do DataFrame
cntPagesToDF <- 0
# nazwa tabeli do ktorej wrzucam ogłoszenia:
tableName <- 'SUV'

###  na potrzeby testow:
subpages <- 5
cars <-  data.frame()


start_exec <- Sys.time()

for(page_no in 1: subpages ) {
  
  print( paste("Strona:", page_no,"start",print(Sys.time())))
  
  # wczytanie strony ze szczegółami ogłoszenia
  cnt <- 0
  repeat {
    
    link = paste0(otomoto_link,"?page=",page_no)
    page = read_html(link)
    car_links <-  page %>% html_nodes(".ooa-1mgjl0z-Text a") %>% html_attr("href")
    
    print(paste( link," => " ,length(car_links)))
    cnt <- cnt +1
    if (length(car_links) >0){
      break
    }
    
    if(  cnt>=3)  {
      break
    }
  }
  
  # pobranie szczegółów ogłoszenia do DataFrame
  if (length(car_links)>0 ) {
    for (i in 1: length(car_links)) {
      
      if (  startsWith( car_links[i], 'https://www.otomoto.pl/oferta' )    ) {
        car_df <- data.frame()
        
        car_df <- get_params(car_links[i])
        cars <- rbind.fill(cars, car_df)
      }}
    
    
    print( paste("Strona:", page_no,"wczytana", print(Sys.time())))
    
    
    print(paste("Czas wykonywania do tej pory:", Sys.time()- start_exec   ))
    cntPagesToDF <- cntPagesToDF + 1
  }
  
  if ( cntPagesToDF %% subpagesToDB ==0 ||  page_no==subpages) {
 
    print(paste("zapis do bazy",    nrow(cars) , "rekordów" ))
    writeToDb(cars,tableName)
    
    #zerowanie
    cntPagesToDF <- 0
    cars <-  data.frame()
  }
  
} 
 



###############################################
#   usunięcie bazy                            #
#                                             #
#   dbConn<-dbConnect(SQLite(),"cars.sqlite") #
#   dbRemoveTable(dbConn, 'SUV')
#   dbDisconnect(dbConn)                      #
###############################################

 
 






 
