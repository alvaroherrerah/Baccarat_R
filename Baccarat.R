
rm(list =ls())

jugar_baccarat <- function(baraja) {
  
  numeros <- c("As", 2:9, "J","Q","K") 
  valor <- c(1, 2:9, rep(0, 3))
  palos  <- c("picas", "corazones", "diamantes", "tréboles")
  baraja <-  cbind(expand.grid(numero=numeros, 
                               palo=palos, stringsAsFactors=FALSE), valor)
  
  repartir_carta <- function(baraja) {
    carta <- baraja[sample(nrow(baraja), 1, replace = F), ]
    return(carta)
  }
  cat("\n\n", "Nueva Partida")
  cartas_jugador <- rbind(repartir_carta(baraja), repartir_carta(baraja))
  cartas_banca <- rbind(repartir_carta(baraja), repartir_carta(baraja))
  
  total_jugador <- sum(as.numeric(cartas_jugador$valor)) %% 10
  total_banca <- sum(as.numeric(cartas_banca$valor)) %% 10
  
  cat("\n\n", "Tus cartas son: ", 
      cartas_jugador$numero[1], "de", cartas_jugador$palo[1],
      " y", cartas_jugador$numero[2], "de", cartas_jugador$palo[2],
      " y su valor total es: ", 
      total_jugador,"\n")
  
  cat("Las cartas de la banca son: ", 
      cartas_banca$numero[1], "de", cartas_banca$palo[1],
      " y", cartas_banca$numero[2], "de", cartas_banca$palo[2],
      " y su valor total es: ", 
      total_banca, "\n\n")
  
  
  if (total_jugador %in% c(8, 9) | total_banca %in% c(8, 9)){
    if (total_jugador > total_banca) {
      cat("Jugador gana.","\n\n")
      return(invisible("Jugador gana"))
    } else if (total_jugador < total_banca) {
      cat("Banca ha ganado.","\n\n")
      return(invisible("Banca gana"))
    } else {
      cat("Es empate.","\n\n")
      return(invisible("Empate"))
    }
  } else {
    resp <- readline("¿Quieres carta? s/n ")
      if(resp == 's'){
        n_carta_jugador <- repartir_carta(baraja)
        n_cartas_jugador <- rbind(cartas_jugador, n_carta_jugador)
        n_total_jugador <- sum(as.numeric(n_cartas_jugador$valor)) %% 10
        
        cat("\n\n", "Ahora tus cartas son: ", 
            n_cartas_jugador$numero[1], "de", n_cartas_jugador$palo[1],
            " ,", n_cartas_jugador$numero[2], "de", n_cartas_jugador$palo[2],
            " y", n_cartas_jugador$numero[3], "de", n_cartas_jugador$palo[3],
            " y su valor total es: ", 
            n_total_jugador, "\n")
        
        if(total_banca <= 5){
          n_carta_banca <- repartir_carta(baraja)
          n_cartas_banca <- rbind(cartas_banca, n_carta_banca)
          n_total_banca <- sum(as.numeric(n_cartas_banca$valor)) %% 10
          
          cat("Las cartas de la banca ahora son: ", 
              n_cartas_banca$numero[1], "de", n_cartas_banca$palo[1],
              " ,", n_cartas_banca$numero[2], "de", n_cartas_banca$palo[2],
              " y", n_cartas_banca$numero[3], "de", n_cartas_banca$palo[3],
              " y su valor total es: ", 
              n_total_banca, "\n\n")
          
          if (n_total_jugador > n_total_banca) {
            cat("Jugador gana.","\n\n")
            return(invisible("Jugador gana"))
          } else if (n_total_jugador < n_total_banca) {
            cat("Banca ha ganado.","\n\n")
            return(invisible("Banca gana"))
          } else {
            cat("Es empate.","\n\n")
            return(invisible("Empate"))
          }
        } else {
          n_total_banca <- total_banca
          
          cat("Las cartas de la banca son: ", 
              cartas_banca$numero[1], "de", cartas_banca$palo[1],
              " y", cartas_banca$numero[2], "de", cartas_banca$palo[2],
              " y su valor total es: ", 
              n_total_banca, "\n\n")
        
          if (n_total_jugador > n_total_banca) {
            cat("Jugador gana.","\n")
            return(invisible("Jugador gana"))
          } else if (n_total_jugador < n_total_banca) {
            cat("Banca ha ganado.","\n")
            return(invisible("Banca gana"))
          } else {
            cat("Es empate.","\n")
            return(invisible("Empate"))
          }
        }
      }
      if(resp == 'n'){
        n_total_jugador <- total_jugador
        
        cat("\n\n", "Tus cartas son: ", 
            cartas_jugador$numero[1], "de", cartas_jugador$palo[1],
            " y", cartas_jugador$numero[2], "de", cartas_jugador$palo[2],
            " y su valor total es: ", 
            n_total_jugador, "\n")
        
        if(total_banca <= 5){
          n_carta_banca <- repartir_carta(baraja)
          n_cartas_banca <- rbind(cartas_banca, n_carta_banca)
          n_total_banca <- sum(as.numeric(n_cartas_banca$valor)) %% 10
          
          cat("Las cartas de la banca ahora son: ", 
              n_cartas_banca$numero[1], "de", n_cartas_banca$palo[1],
              " ,", n_cartas_banca$numero[2], "de", n_cartas_banca$palo[2],
              " y", n_cartas_banca$numero[3], "de", n_cartas_banca$palo[3],
              " y su valor total es: ", 
              n_total_banca, "\n\n")
          
          if (n_total_jugador > n_total_banca) {
            cat("Jugador gana.","\n\n")
            return(invisible("Jugador gana"))
          } else if (n_total_jugador < n_total_banca) {
            cat("Banca ha ganado.","\n\n")
            return(invisible("Banca gana"))
          } else {
            cat("Es empate.","\n\n")
            return(invisible("Empate"))
          }
        } else {
          n_total_banca <- total_banca
          
          cat("Las cartas de la banca son: ", 
              cartas_banca$numero[1], "de", cartas_banca$palo[1],
              " y", cartas_banca$numero[2], "de", cartas_banca$palo[2],
              " y su valor total es: ", 
              n_total_banca, "\n\n")
          
          if (n_total_jugador > n_total_banca) {
            cat("Jugador gana.","\n\n")
            return(invisible("Jugador gana"))
          } else if (n_total_jugador < n_total_banca) {
            cat("Banca ha ganado.","\n\n")
            return(invisible("Banca gana"))
          } else {
            cat("Es empate.","\n\n")
            return(invisible("Empate"))
          }
        }
      }
  }
}



simulador_partidas <- function(n, bolsa, apuesta){
  resultados <- c()
  bolsa_ap <- bolsa
  apuesta_part <- apuesta

  for (i in 1:n){
    
      resultados[i] <- jugar_baccarat(baraja)
      
      if(resultados[i] == "Jugador gana") {
        cat("\n\n", "La bolsa tenía: ", bolsa_ap, "\n") 
        bolsa_ap <- bolsa_ap + apuesta_part
        cat("Tras ganar tiene: ", bolsa_ap, "\n\n")
      }
      if(resultados[i] == "Banca gana") {
        cat("\n\n", "La bolsa tenía: ", bolsa_ap, "\n")
        bolsa_ap <- bolsa_ap - apuesta_part 
        cat("Tras perder tiene: ", bolsa_ap, "\n\n")
      }
      if(resultados[i] == "Empate") {
        cat("\n\n", "La bolsa tenía: ", bolsa_ap, "\n")
        bolsa_ap <- bolsa_ap
        cat("Tras empatar tiene: ", bolsa_ap, "\n\n")
      }
  }
  victorias_jugador <- sum(resultados == "Jugador gana")
  victorias_banca <- sum(resultados == "Banca gana")
  empates <- sum(resultados == "Empate")
  
  print(paste("Victorias del jugador: ", victorias_jugador))
  print(paste("Victorias de la banca: ", victorias_banca))
  print(paste("Empates: ", empates))
  print(paste("Bolsa tras", n, "partidas: ", bolsa_ap))
}


simulador_partidas(15, 500, 100)






