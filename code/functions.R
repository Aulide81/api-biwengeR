library(httr)

get_token <- function(email, password){
  
  response <- POST( url="https://biwenger.as.com/api/v2/auth/login" , 
                    body = list("email"= email, "password"= password))
  
  token<-content(response)
  
  return(token$token)
  
}

get_leagues<-function(token){
  
  response <- GET(url="https://biwenger.as.com/api/v2/account", 
                  add_headers(authorization = paste('Bearer', token)))
  
  lista <- content(response)
  ligas <- sapply(lista$data$leagues, function(l){
    c("League Name"=l$name,"Id League"=l$id,"User Name"=l$user$name,"Id User"=l$user$id)} )
  ligas<-t(ligas)|>data.frame()
  return(ligas)
}

set_ids<-function(list_ligues, nleague){
  lista<-list('id_league'=as.numeric(list_ligues[nleague,2]),
              'id_user'=as.numeric(list_ligues[nleague,4]))
}

set_ids2<-function(list_ligues, id.league){
  
  id.league<-as.numeric(id.league)
  id.user <- as.numeric(list_ligues$Id.User[list_ligues$Id.League==id.league])
  
  return(list('id_league'= id.league,'id_user'= id.user))
}

get_members<-function(token, list_ids){
  
  response<-GET(url="https://biwenger.as.com/api/v2/league?include=all,-lastAccess&fields=*,standings,tournaments,group,settings(description)",
                add_headers(authorization = paste('Bearer', token), `x-league` = list_ids$id_league, `x-user` = list_ids$id_user))
  
  lista <- content(response)
  
  icon_league<-paste0("https://cdn.biwenger.com/",lista$data$icon)
  
  miembros<-sapply(lista$data$standings, function(p){
    c("Id"=p$id,
      "Name"=p$name,
      "Points"=p$points,
      "Size"=p$teamSize,
      "Value"=p$teamValue,
      "icon"=paste0("https://cdn.biwenger.com/",p$icon))
  })|>t()|>data.frame()
  
  miembros<-structure(miembros,"create_date"=lista$data$created)
  
  return(miembros)
  
}
get_movements<-function(token, list_ids, members, amount_init=50000000, step=500){
  
  # Inicializamos el balance
  amount_init<-amount_init
  balance<-lapply(members$Name, function(p){
    m <- c(amount_init)
    names(m) <- attr(members,"create_date")
    return(m)
  })
  names(balance)<-members$Name
  
  url<-"https://biwenger.as.com/api/v2/league/1648876/board?offset=__OFFSET__&limit=__step__"
  url<-gsub("__step__",step,url)
  
  OFFSET<-0
  continue<-TRUE
  while (continue){
    
    url<-gsub("__OFFSET__",OFFSET,url)
    response = GET(url, add_headers(authorization = paste('Bearer', token), `x-league` = list_ids$id_league, `x-user` = list_ids$id_user))
    lista<-content(response)$data
    
    for (div in lista){
      
      if (div$type=="leagueReset") {
        continue<-FALSE
        break
      }
      
      if (div$type %in% c("transfer","market")){
        position<-1
        for(mov in div$content){
          
          #Existe comrpador
          if ('to' %in% names(mov)){
            if (div$date %in% names(balance[[mov$to$name]])){
              amount<-c(-mov$amount)
              names(amount)<- (div$date + position)
              balance[[mov$to$name]]<-c(balance[[mov$to$name]],amount)
            }else{
              amount<-c(-mov$amount)
              names(amount)<- (div$date)
              balance[[mov$to$name]]<-c(balance[[mov$to$name]],amount)
            }
          }
          #Existe vendedor
          if ('from' %in% names(mov)){
            if (div$date %in% names(balance[[mov$from$name]])){
              amount<-c(mov$amount)
              names(amount) <- (div$date + position)
              balance[[mov$from$name]]<-c(balance[[mov$from$name]],amount)
            }else{
              amount <- c(mov$amount)
              names(amount) <- (div$date)
              balance[[mov$from$name]]<-c(balance[[mov$from$name]],amount)
            }
          }
        }
      }
    }
    OFFSET<-OFFSET+step
  }
  
  return(balance)
}
