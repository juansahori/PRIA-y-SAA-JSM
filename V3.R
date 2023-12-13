#Función para obtener el movimiento óptimo de la máquina basado en la secuencia inicial del usuario
movimiento_optimo <- function(historial) {
  if (nrow(historial) < 3) {
    opciones <- c("r", "p", "s")
    return(sample(opciones, 1))
  } else {
    # Obtener la opción del usuario más común de todo el historial
    opcion_usuario_mas_comun <- names(sort(table(historial$opcion_usuario), decreasing = TRUE)[1])
    
    # Elige el movimiento que vencería a la opción del usuario más común
    if (opcion_usuario_mas_comun == "r") {
      return(sample(c("p", "r", "s"), 1, prob = c(0.4, 0.3, 0.3)))
    } else if (opcion_usuario_mas_comun == "p") {
      return(sample(c("p", "r", "s"), 1, prob = c(0.3, 0.3, 0.4)))
    } else {
      return(sample(c("p", "r", "s"), 1, prob = c(0.3, 0.4, 0.3)))
    }
  }
}



#Función para jugar al piedra, papel o tijera
jugar_partida <- function(usuario,maquina) {
  if(usuario == maquina) {
    cat("Empate\n")
    cat(paste("Elegiste",usuario,"la maquina elegio",maquina,"\n"))
    return("Empate")
  } else if ((usuario == "r" && maquina == "s") || 
             (usuario == "p" && maquina == "r") ||
             (usuario == "s" && maquina == "p")) {
    cat("Ganaste\n")
    cat(paste("Elegiste",usuario,"la maquina elegio",maquina,"\n"))
    return("Ganaste")
  } else{
    cat("Perdiste\n")
    cat(paste("Elegiste",usuario,"la maquina elegio",maquina,"\n"))
    return("Perdiste")
  }
}

#Función para mostrar estadísticas de un jugador
estadisticas_jugador <- function(partidas) {
  cat("Estadísticas del jugador:\n")
  cat("Partidas jugadas: ", nrow(partidas), "\n")
  cat("Ganadas: ", sum(partidas$resultado == "Ganaste"), "\n")
  cat("Perdidas: ", sum(partidas$resultado == "Perdiste"), "\n")
  cat("Empates: ", sum(partidas$resultado == "Empate"), "\n")
}

#Función para mostrar créditos
creditos <- function(jugadores) {
  cat("Historial total de puntuaciones:\n")
  for (nombre in names(jugadores)) {
    cat("Jugador: ", nombre, "\n")
    estadisticas_jugador(jugadores[[nombre]])
  }
}

# Cargar los perfiles de jugadores desde el archivo si existe
if (file.exists("perfiles_jugadores.rds")) {
  jugadores <- readRDS("perfiles_jugadores.rds")
} else {
  jugadores <- list()
}

#Bucle pricipal
while (TRUE) {
  cat("Introduzca su nombre de jugador: \n")
  cat("O salir del juego(Q,q)")
  nombre_jugador <- tolower(readline())
  
  if (nombre_jugador == "q") {
    cat("Saliendo del juego. Hasta la próxima.\n")
    break
  } else if (nombre_jugador %in% names(jugadores)) {
    cat("Bienvenido de nuevo, ", nombre_jugador, "!\n")
  } else {
    cat("Bienvenido, ", nombre_jugador, "!\n")
    jugadores[[nombre_jugador]] <- data.frame(
      resultado = character(),
      opcion_usuario = character(),
      opcion_maquina = character(),
      stringsAsFactors = FALSE
    )  # Inicializa el perfil del jugador como un data frame
  }
  
  # Bucle para el juego
  while (TRUE) {
    cat("Introduzca piedra (R,r), papel (P,p) o tijera (S,s): \n")
    cat("Otras opciones: historico del jugador(H,h),  créditos(C,c) y volver al menu principal(Q,q)")
    opcion_usuario <- tolower(readline())
    
    if (opcion_usuario == "q") {
      cat("Saliendo del juego. Hasta la próxima.\n")
      saveRDS(jugadores, "perfiles_jugadores.rds")  # Guarda los perfiles de jugadores en un archivo
      break
    } else if (opcion_usuario == "h") {
      estadisticas_jugador(jugadores[[nombre_jugador]])
    } else if (opcion_usuario == "c") {
      creditos(jugadores)
    } else if (opcion_usuario %in% c("r", "p", "s")) {
      opcion_maquina <- movimiento_optimo(jugadores[[nombre_jugador]])
      resultado <- jugar_partida(opcion_usuario, opcion_maquina)
      
      # Agrega una nueva fila al data frame del jugador
      jugadores[[nombre_jugador]] <- rbind(jugadores[[nombre_jugador]], data.frame(
        resultado = resultado,
        opcion_usuario = opcion_usuario,
        opcion_maquina = opcion_maquina,
        stringsAsFactors = FALSE
      ))
    } else {
      cat("Error, tecla no válida\n")
    }
  }
}