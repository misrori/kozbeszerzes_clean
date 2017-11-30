library(data.table)

# get_adat
#####
adat <- data.table(fread("eredmyn_to_clean.csv", stringsAsFactors = F))

adat<- adat[,-c(1, 15, 16)]
names(adat)

#####

#########################nyertesek
#####
nyertes_tisztitas <- function(szoveg){
  szoveg<- gsub('\\"', "", szoveg)
  szoveg<- gsub("\\'", "", szoveg)
  szoveg<- gsub("\\„", "", szoveg)
  szoveg<- gsub("\\”", "", szoveg)
  darabok <- strsplit(szoveg, ";")[[1]]
  darabok <- darabok[darabok!=""]
  darabok <- unique(darabok)
  if(length(darabok)>1){
  return(list(darabok ))
  }else{
    return(szoveg)
  }
  
}


nyertes_hosz <- function(szoveg){
  szoveg<- gsub('\\"', "", szoveg)
  szoveg<- gsub("\\'", "", szoveg)
  szoveg<- gsub("\\„", "", szoveg)
  szoveg<- gsub("\\”", "", szoveg)
  darabok <- strsplit(szoveg, ";")[[1]]
  darabok <- darabok[darabok!=""]
  darabok <- unique(darabok)
  return(length(darabok ))
}

nyertes_list_to_chr <- function(szoveg){

  return(unlist(szoveg))
}


adat$nyertes_lista <- sapply(adat$nyertes_ajanlattevo,nyertes_tisztitas)
adat$nyertesek_szama <- sapply(adat$nyertes_ajanlattevo,nyertes_hosz)


list_to_rows <- function(my_df, oszlop){
  my_df <- data.table(my_df)
  lista_hosz <-my_df$nyertesek_szama
  
  if( lista_hosz>1){
    t<- data.frame(my_df, stringsAsFactors = F)
    th <-data.table(t[,-which(names(my_df)==oszlop)])
    repeted <- th[rep(1,lista_hosz),]
    repeted$nyertes_unique <- unname(unlist(my_df[,eval(quote(oszlop)), with=F]))
    
    return(repeted)
  } else if(lista_hosz==1){
    t<- data.frame(my_df, stringsAsFactors = F)
    th <-data.table(t[,-which(names(my_df)==oszlop)])
    repeted <- th[rep(1,lista_hosz),]
    repeted$nyertes_unique <- unname(unlist(my_df[,eval(quote(oszlop)), with=F]))
    return(repeted)
  }else if(lista_hosz==0){
    t<- data.frame(my_df, stringsAsFactors = F)
    th <-data.table(t[,-which(names(my_df)==oszlop)])
    th$nyertes_unique <- ""
    return(th)
  }
  
}

adat_tabla_to_extended_nyertes <- function(my_adat, oszlop){


vege_list <- list()
for(i in 1:nrow(adat)){
  print(i)
  vege_list[[i]]<- list_to_rows(adat[i,], 'nyertes_lista')

}

minden_nyertes <- rbindlist(vege_list)

return(minden_nyertes)

}


nyertesek_extended_tabla<- adat_tabla_to_extended_nyertes(adat, 'nyertes_lista')

######




#########################ajanlatkero
#####
ajanlatkero_tisztitas <- function(szoveg){
  szoveg<- gsub('\\"', "", szoveg)
  szoveg<- gsub("\\'", "", szoveg)
  szoveg<- gsub("\\„", "", szoveg)
  szoveg<- gsub("\\”", "", szoveg)
  darabok <- strsplit(szoveg, ";")[[1]]
  darabok <- darabok[darabok!=""]
  darabok <- unique(darabok)
  if(length(darabok)>1){
    return(list(darabok ))
  }else{
    return(szoveg)
  }
  
}


ajanlatkero_hosz <- function(szoveg){
  szoveg<- gsub('\\"', "", szoveg)
  szoveg<- gsub("\\'", "", szoveg)
  szoveg<- gsub("\\„", "", szoveg)
  szoveg<- gsub("\\”", "", szoveg)
  darabok <- strsplit(szoveg, ";")[[1]]
  darabok <- darabok[darabok!=""]
  darabok <- unique(darabok)
  return(length(darabok ))
}







ajanlatkero_list_to_chr <- function(szoveg){
  
  return(unlist(szoveg))
}


adat$ajanlatkero_lista <- sapply(adat$ajanlatkero,ajanlatkero_tisztitas)
adat$ajanlatkero_szama <- sapply(adat$ajanlatkero,ajanlatkero_hosz)
adat <- adat[,-c('nyertes_lista', 'nyertesek_szama')]


list_to_rows_ajanlat <- function(my_df, oszlop){
  my_df <- data.table(my_df)
  lista_hosz <-my_df$ajanlatkero_szama
  
  if( lista_hosz>1){
    t<- data.frame(my_df, stringsAsFactors = F)
    th <-data.table(t[,-which(names(my_df)==oszlop)])
    repeted <- th[rep(1,lista_hosz),]
    repeted$ajanlatkero_unique <- unname(unlist(my_df[,eval(quote(oszlop)), with=F]))
    
    return(repeted)
  } else if(lista_hosz==1){
    t<- data.frame(my_df, stringsAsFactors = F)
    th <-data.table(t[,-which(names(my_df)==oszlop)])
    repeted <- th[rep(1,lista_hosz),]
    repeted$ajanlatkero_unique <- unname(unlist(my_df[,eval(quote(oszlop)), with=F]))
    return(repeted)
  }else if(lista_hosz==0){
    t<- data.frame(my_df, stringsAsFactors = F)
    th <-data.table(t[,-which(names(my_df)==oszlop)])
    th$ajanlatkero_unique <- ""
    return(th)
  }
  
}

adat_tabla_to_extended_nyertes <- function(my_adat, oszlop){
  
  
  vege_list <- list()
  for(i in 1:nrow(adat)){
    print(i)
    vege_list[[i]]<- list_to_rows_ajanlat(adat[i,], 'ajanlatkero_lista')
    
  }
  
  minden_nyertes <- rbindlist(vege_list)
  
  return(minden_nyertes)
  
}

ajanlatkerok_extended_tabla<- adat_tabla_to_extended_nyertes(adat, 'ajanlatkero')

######




