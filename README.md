# api-biwengeR

Funciones en R que permite el raspado de una sesión As Biwenger

### Obtener token.

token <- get_token( email='xxx@gmail.com', password='****')

### Obtener ligas.

ligas <- get_leagues(token)

### Seleccionar liga/ obtener ids.

ids<-set_ids(ligas,1)

### Obtener miembros

miembros<-get_members(token,ids)

### Obtener compras/ventas.

movimientos<-get_movements(token, ids, miembros)
summary<-sapply(movimientos, sum)
summary<-data.frame("Balance"=summary)
summary
