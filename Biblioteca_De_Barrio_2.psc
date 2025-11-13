Algoritmo Biblioteca_De_Barrio_2
	
	Definir D, i, j, opcion, limite_prestamos Como Entero
	Definir prestamos_total_netos Como Entero
	Definir prestamos, devoluciones, netos, totales Como Entero
	Definir top_valor_neto, dia_pico Como Entero
	Definir promedios, porcentaje, valor_a_redondear, valor_truncado, porcentaje_final Como Real
	Definir categorias Como Caracter
	Definir error_dia, captura_exitosa, alerta_encontrada Como Logico
	
	Dimensionar prestamos[14, 4]
	Dimensionar devoluciones[14, 4]
	Dimensionar netos[14, 4]
	Dimensionar totales[4]
	Dimensionar top_valor_neto[4]
	Dimensionar dia_pico[4]
	Dimensionar promedios[4]
	Dimensionar porcentaje[4]
	Dimensionar categorias[4]
	
	D <- 0
	limite_prestamos <- 44
	
	Para i <- 1 Hasta 14 Con Paso 1 Hacer
		Para j <- 1 Hasta 4 Con Paso 1 Hacer
			prestamos[i, j] <- 0
			devoluciones[i, j] <- 0
			netos[i, j] <- 0
		FinPara
	FinPara
	
	Para i <- 1 Hasta 4 Con Paso 1 Hacer
		totales[i] <- 0
		promedios[i] <- 0.0
		porcentaje[i] <- 0.0
		top_valor_neto[i] <- -99999
		dia_pico[i] <- 0
	FinPara
	
	categorias[1] <- "Infantil"
	categorias[2] <- "Novela"
	categorias[3] <- "Tecnica"
	categorias[4] <- "Otros"
	
	Repetir
		Escribir "Biblioteca de Barrio: Prestamos, Devoluciones y Estadisticas (ODS 11)"
		Escribir "------------------------------------------------------------------"
		Escribir "|1|.- Capturar Datos"
		Escribir "|2|.- Reportes (Netos, Promedio y Porcentaje)"
		Escribir "|3|.- Top y Picos"
		Escribir "|4|.- Alertas por Demanda"
		Escribir "|5|.- Reporte Detallado Préstamos/Devoluciones"
		Escribir "[6]. Salir"
		Escribir "Ingrese una opcion: "
		Leer opcion
		
		Escribir "------------------------------------------------------------------"
		
		Si opcion = 1 Entonces
			Escribir "	=== CAPTURAR DATOS === "
			Escribir "Ingrese el numero de dias a registrar (maximo 14): "
			Leer D
			
			Si D >= 1 Y D <= 14 Entonces
				Para i <- 1 Hasta 4 Con Paso 1 Hacer
					totales[i] <- 0
					top_valor_neto[i] <- -99999
					dia_pico[i] <- 0
				FinPara
				
				Para i <- 1 Hasta D Con Paso 1 Hacer
					Escribir "Dia ", i, ":"
					captura_exitosa <- Falso
					
					Repetir
						error_dia <- Falso
						
						Para j <- 1 Hasta 4 Con Paso 1 Hacer
							Escribir "  - Prestamos de ", categorias[j], ": "
							Leer prestamos[i, j]
							Escribir "  - Devoluciones de ", categorias[j], ": "
							Leer devoluciones[i, j]
							
							Si prestamos[i, j] < 0 O devoluciones[i, j] < 0 Entonces
								Escribir "=== ERROR === Préstamos/Devoluciones no pueden ser negativos. Reintente el día."
								error_dia <- Verdadero
								j <- 5
							SiNo
								netos[i, j] <- prestamos[i, j] - devoluciones[i, j]
								
								Si j = 4 Y error_dia = Falso Entonces
									captura_exitosa <- Verdadero
								FinSi
							FinSi
						FinPara
						
					Hasta Que captura_exitosa = Verdadero
					
					Para j <- 1 Hasta 4 Con Paso 1 Hacer
						totales[j] <- totales[j] + netos[i, j]
						
						Si netos[i, j] > top_valor_neto[j] Entonces
							top_valor_neto[j] <- netos[i, j]
							dia_pico[j] <- i
						FinSi
						
						Si netos[i, j] < 0 Entonces
							Escribir "AVISO en Día ", i, " - ", categorias[j], ": Neto negativo (", netos[i, j], "). Revisar stock."
						FinSi
					FinPara
				FinPara
				Escribir "Datos de ", D, " dias capturados con éxito."
			SiNo
				Escribir "=== ERROR === El numero de dias debe estar entre 1 y 14."
			FinSi
		FinSi
		
		Si opcion = 2 Entonces
			Si D > 0 Entonces
				Escribir "	=== REPORTES DIARIOS Y PROMEDIOS === "
				
				Escribir "--- Tabla de Préstamos Netos Diarios ---"
				Escribir "Día | Infantil | Novela | Técnica | Otros"
				Escribir "---------------------------------------------"
				
				Para i <- 1 Hasta D Con Paso 1 Hacer
					Escribir i, "   | ", netos[i, 1], "      | ", netos[i, 2], "      | ", netos[i, 3], "      | ", netos[i, 4]
				FinPara
				
				prestamos_total_netos <- 0
				Para i <- 1 Hasta 4 Con Paso 1 Hacer
					prestamos_total_netos <- prestamos_total_netos + totales[i]
				FinPara
				
				Escribir "--- Promedios y Porcentajes Acumulados ---"
				Para i <- 1 Hasta 4 Con Paso 1 Hacer
					promedios[i] <- totales[i] / D
					
					Si prestamos_total_netos > 0 Entonces
						porcentaje[i] <- (totales[i] / prestamos_total_netos) * 100
					SiNo
						porcentaje[i] <- 0.0
					FinSi
					
					valor_a_redondear <- porcentaje[i] * 100
					valor_truncado <- Trunc(valor_a_redondear + 0.5)
					porcentaje_final <- valor_truncado / 100
					
					Escribir "Categoria: ", categorias[i], ""
					Escribir "  - Promedio Neto/Día: ", promedios[i], " libros."
					Escribir "  - Porcentaje de Participación: ", porcentaje_final, "%"
					Escribir ""
				FinPara
			SiNo
				Escribir "=== ERROR === Primero debe capturar los datos (Opcion 1)."
			FinSi
		FinSi
		
		Si opcion = 3 Entonces
			Si D > 0 Entonces
				Escribir "	=== TOP Y PICOS DE DEMANDA === "
				
				max_total_periodo <- -99999
				indice_top_general <- 0
				Para i <- 1 Hasta 4 Con Paso 1 Hacer
					Si totales[i] > max_total_periodo Entonces
						max_total_periodo <- totales[i]
						indice_top_general <- i
					FinSi
				FinPara
				
				Escribir "--- Top Categoría (Periodo) ---"
				Escribir "La categoría con mayor demanda neta fue ", categorias[indice_top_general], " con ", max_total_periodo, " movimientos."
				Escribir "---------------------------------"
				
				Escribir "--- Día Pico por Categoría ---"
				Para i <- 1 Hasta 4 Con Paso 1 Hacer
					Escribir "Categoria ", categorias[i], ": Pico de ", top_valor_neto[i], " netos en el Día ", dia_pico[i], "."
				FinPara
			SiNo
				Escribir "=== ERROR === Primero debe capturar los datos (Opcion 1)."
			FinSi
		FinSi
		
		Si opcion = 4 Entonces
			Si D > 0 Entonces
				alerta_encontrada <- Falso
				Escribir "	=== ALERTAS!! (Límite de ", limite_prestamos, " Préstamos/Día) ==="
				
				Para i <- 1 Hasta D Con Paso 1 Hacer
					Para j <- 1 Hasta 4 Con Paso 1 Hacer
						Si prestamos[i, j] >= limite_prestamos Entonces
							Escribir " ALERTA!! - ", categorias[j], " superó el límite el Día ", i, " con ", prestamos[i, j], " préstamos."
							alerta_encontrada <- Verdadero
						FinSi
					FinPara
				FinPara
				
				Si alerta_encontrada = Falso Entonces
					Escribir " NO HAY ALERTAS de alta demanda."
				SiNo
					Escribir "--- Sugerencia: Priorizar la adquisición de títulos de las categorías alertadas. ---"
				FinSi
			SiNo
				Escribir "=== ERROR === Primero debe capturar los datos (Opcion 1)."
			FinSi
		FinSi
		
		Si opcion = 5 Entonces
			Si D > 0 Entonces
				Escribir "	=== REPORTE DETALLADO (PRÉSTAMOS Y DEVOLUCIONES) ==="
				Para i <- 1 Hasta D Con Paso 1 Hacer
					Escribir "--- Día ", i, " ---"
					Para j <- 1 Hasta 4 Con Paso 1 Hacer
						Escribir "  -   ", categorias[j], ' ', "|   Préstamos: ", prestamos[i, j], "   | Devoluciones: ", devoluciones[i, j]
					FinPara
				FinPara
			SiNo
				Escribir "=== ERROR === Primero debe capturar los datos (Opcion 1)."
			FinSi
		FinSi
		
		Si opcion = 6 Entonces
			Escribir "	=== SALIENDO DEL SISTEMA ==="
			Escribir "Programa finalizado. Que tenga un buen día."
		SiNo
			Si opcion < 1 O opcion > 6 Entonces
				Escribir "=== OPCION INVALIDA ===. Por favor, ingrese una opcion valida."
			FinSi
		FinSi
		
		Escribir "------------------------------------------------------------------"
		
	Hasta Que opcion = 6
	
FinAlgoritmo