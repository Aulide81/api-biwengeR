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

  rownames(miembros) <- miembros$Id
  miembros<-structure(miembros,"create_date"=lista$data$created, "icon_league"=icon_league)
  
  return(miembros)  
}

get_movements<-function(token, list_ids, members, amount_init=50000000, step=500){
  
  
  # funcion que raspa el aumento de clausula.
  increment_scrap <- function(){
    
    position<-1
    date_div <- div$date
    
    for(mov in div$content){
      
      id_user <- as.character(mov$user$id)
      
      if (id_user %in% members_Id){
        
        if ( date_div %in% names(balance[[id_user]]) ){
          
          amount<-c(-mov$amount)
          names(amount) <- (date_div + position)
          balance[[id_user]]<<-c(balance[[id_user]],amount)
          
        }else{
          
          amount<-c(-mov$amount)
          names(amount)<- (date_div)
          balance[[id_user]]<<-c(balance[[id_user]],amount)
          
        }
      }
      position<-position+1
    }
  }
  # funcion que raspa el intercambio.
  exchange_scrap <- function(){
    
    position<-1
    date_div <- div$date
    mov <- div$content
    
    
    if ('from' %in% names(mov)){
      id_user <- as.character(mov$from$id)
      
      if(id_user %in% members_Id){
        
        if (date_div %in% names(balance[[id_user]])){
          amount<-c(-mov$amount)
          names(amount)<- (date_div + position)
          balance[[id_user]]<<-c(balance[[id_user]],amount)
        }else{
          amount<-c(-mov$amount)
          names(amount)<- (date_div)
          balance[[id_user]]<<-c(balance[[id_user]],amount)
        }
      }
    }
    
    if ('to' %in% names(mov)){
      id_user <- as.character(mov$to$id)
      
      if(id_user %in% members_Id){
        
        if (date_div %in% names(balance[[id_user]])){
          amount<-c(mov$amount)
          names(amount)<- (date_div + position)
          balance[[id_user]]<<-c(balance[[id_user]],amount)
        }else{
          amount<-c(mov$amount)
          names(amount)<- (date_div)
          balance[[id_user]]<<-c(balance[[id_user]],amount)
        }
      }
    }
  }
  # funcion que raspa compra/venta.
  transfer_scrap <- function(){
    
    position<-1
    date_div <- div$date
    
    for(mov in div$content){
      
      # Existe comprador
      if ('to' %in% names(mov)){
        
        id_user <- as.character(mov$to$id)
        
        if(id_user %in% members_Id){
          
          if (date_div %in% names(balance[[id_user]])){
            amount<-c(-mov$amount)
            names(amount)<- (date_div + position)
            balance[[id_user]]<<-c(balance[[id_user]],amount)
          }else{
            amount<-c(-mov$amount)
            names(amount)<- (date_div)
            balance[[id_user]]<<-c(balance[[id_user]],amount)
          }
        }
      }
      
      # Existe vendedor
      if ('from' %in% names(mov)){
        
        id_user <- as.character(mov$from$id)
        
        if(id_user %in% members_Id){
          
          if (date_div %in% names(balance[[id_user]])){
            amount<-c(mov$amount)
            names(amount)<- (date_div + position)
            balance[[id_user]]<<-c(balance[[id_user]],amount)
          }else{
            amount<-c(mov$amount)
            names(amount)<- (date_div)
            balance[[id_user]]<<-c(balance[[id_user]],amount)
          }
        }
      }
      position<-position + 1
    }
  }
  # funcion que raspa los bonus de la jornada
  bonus_jornada_scrap <- function(){
    
    mov <- div$content
    jornada <- mov$round$name
    pos_expr<-regexpr("Round \\d{1,2}", jornada)
    jornada<-strsplit(jornada,"")[[1]][1:attr(pos_expr,"match.length")] |> paste0(collapse = "")
    
    for (resultado in mov$results){
      
      id_user <- as.character(resultado$user$id)
      
      if (id_user %in% members_Id) {
        
        if (!(jornada %in% names( balance[[id_user]]))){
          
          bonus <- ifelse(is.null(resultado$bonus), 0, resultado$bonus)
          names(bonus) <- jornada
          balance[[id_user]] <<- c(balance[[id_user]], bonus)
        } 
      }
    }
  }
  
# Otros bonus
bonus_scrap <- function(){  
  date_div <- div$date
  for (mov in div$content){
    id_user <- as.character(mov$user$id)
    if (id_user %in% members_Id) {
      bonus <- ifelse( is.null(mov$amount), 0, mov$amount)
      names(bonus) <- date_div
      balance[[id_user]] <<- c(balance[[id_user]], bonus)
    }
  }
}
  
  # Inicializamos el balance
  if (list_ids$id_user!=9997844) stop("Disconected Server")
  amount_init<-amount_init
  create_date <- attr(members,"create_date")
  members_Id<-members$Id

 balance<-lapply(members$Name, function(p){
    m <- c(amount_init)
    names(m) <- create_date
    return(m)
  })
  names(balance)<-members$Id
  
  # Preparamos la url
  url<-"https://biwenger.as.com/api/v2/league/__league__/board?offset=__OFFSET__&limit=__step__"
  url<-gsub("__league__", list_ids$id_league, url)
  url<-gsub("__step__", step, url)
  
  
  OFFSET <- 0 # inicio de la ventana a cargar
  continue <- TRUE # Bandera del bucle
  
  # tipos de movimientos a raspar
  #tipos <- c("transfer","market", "exchange","clauseIncrement","roundFinished")
  
  # Comienza el bucle
  while (continue){
    
    # Peticion get
    response = GET( gsub("__OFFSET__",OFFSET,url), 
                    add_headers(authorization = paste('Bearer', token), 
                                `x-league` = list_ids$id_league, 
                                `x-user` = list_ids$id_user))
    lista<-content(response)$data
    
    for (div in lista){
      
      if ( div$type %in% c("leagueReset", "leagueWelcome") ) {
        continue<-FALSE
        break
      }
      
      switch(div$type,
             clauseIncrement=increment_scrap(),
             exchange=exchange_scrap(),
             transfer=transfer_scrap(),
             market=transfer_scrap(),
             roundFinished=bonus_jornada_scrap(),
             bonus=bonus_scrap())
    }
    
    OFFSET<-OFFSET+step
    
    #if (length(lista) < step) {
    #  continue <- FALSE
    #}
    
    Sys.sleep(5)
  }
  return(balance)
}
