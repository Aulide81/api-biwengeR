# api-biwengeR

Funciones en R que permite el raspado de una sesión As Biwenger

## obtener token.

token <- get_token( email='xxx@gmail.com', password='****')

## obtener ligas.

ligas <- get_leagues(token)

## seleccionar liga/ obtener ids.

ids<-set_ids(ligas,1)

## Obtener miembros

#miembros<-get_members(token,ids)

## Obtener compras/ventas.

movimientos<-get_movements(token, ids, miembros)
summary<-sapply(movimientos, sum)
summary<-data.frame("Balance"=summary)
summary
