# api-biwengeR

Funciones en R que permite el raspado de una sesión As Biwenger. La única dependencia es la libreria httr

### Descargar/Cargar Funciones.

Si deseamos descargar el fichero con las funciones en nuestro terminal:

download.file("https://raw.githubusercontent.com/Aulide81/api-biwengeR/main/code/functions.R", destfile = "biwengeR.R")

file.show("biwengeR.R")

Si deseamos cargar las funciones en nuestra sesión de R:

 source("https://raw.githubusercontent.com/Aulide81/api-biwengeR/main/code/functions.R")
 
 list()

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
