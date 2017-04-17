module Main where
import NavesEspaciales
import Test.HUnit
import Data.List



--Naves para pruebas:
contenedorSolo = Base Contenedor
nave1 = Base Motor
nave2 = Módulo Cañón (Base Escudo) (Base Motor)
nave3 = Módulo Motor (Base Escudo) (Base Cañón)
nave4 = Módulo Contenedor nave2 nave3
nave5 = Módulo Contenedor nave3 nave2
nave6 = Módulo Contenedor nave4 nave1
nave7 = Módulo Contenedor nave1 nave5
nave8 = Módulo Contenedor nave1 nave6
nave9 = Módulo Escudo
		(Módulo Escudo (Módulo Escudo (Base Escudo) (Base Cañón)) (Módulo Motor (Base Contenedor) (Base Motor)))
		(Módulo Escudo (Módulo Contenedor (Base Motor) (Base Contenedor)) (Módulo Escudo (Base Cañón) (Base Escudo)))
nave10 = Módulo Escudo
        (Módulo Escudo (Módulo Escudo (Base Escudo) (Base Motor)) (Módulo Motor (Base Contenedor) (Base Motor)))
        (Módulo Escudo (Módulo Contenedor (Base Motor) (Base Contenedor)) (Módulo Escudo (Base Cañón) (Base Escudo)))
nave11 = Módulo Escudo
        (Base Contenedor)
        (Módulo Escudo (Módulo Contenedor (Base Motor) (Base Contenedor)) (Módulo Escudo (Base Cañón) (Base Escudo)))
nave12 = Módulo Escudo
        (Módulo Escudo (Base Contenedor) (Módulo Motor (Base Contenedor) (Base Motor)))
        (Módulo Escudo (Módulo Contenedor (Base Motor) (Base Contenedor)) (Módulo Escudo (Base Cañón) (Base Escudo)))
nave13=  Módulo Contenedor (Módulo Motor (Base Cañón) (Base Cañón)) (Módulo Cañón(Base Cañón) (Base Motor))
nave14 = Módulo Contenedor (Módulo Contenedor(Módulo Cañón(Base Cañón)(Base Motor)) (Módulo Motor(Base Cañón)(Base Cañón)))(Base Motor)
soloUnMotor = Base Motor
puroContenedor = Módulo Contenedor (Base Contenedor) (Base Contenedor)
tresCañones = Módulo Cañón (Base Cañón) (Base Cañón)

contenedorYCañon = Módulo Contenedor (Base Cañón) (Base Contenedor)
otroCañon = Módulo Contenedor (Base Contenedor) (Base Cañón)

escudoSinCañon = Módulo Escudo (Base Contenedor) (Base Contenedor)

protegido = Módulo Escudo (Base Contenedor) (Base Cañón)
protegidoNivel1Estribor = Módulo Contenedor soloUnMotor protegido

superProtegido = Módulo Motor protegido protegido

desbalanceado = Módulo Escudo (Base Contenedor) protegido


--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8" ~: testsEj8
  ]

testsEj2 = test [
  0 ~=? capacidad soloUnMotor,
  3 ~=? capacidad puroContenedor,
	0 ~=? poderDeAtaque nave1,
  2 ~=? poderDeAtaque nave9,
	True ~=? puedeVolar soloUnMotor,
  False ~=? puedeVolar protegido,
	True ~=? mismoPotencial nave2 nave3,
  False ~=? mismoPotencial puroContenedor protegido
  ]

testsEj3 = test [
  nave1 ~=? mayorCapacidad [nave1],
	nave12 ~=? mayorCapacidad [nave1,nave9,nave2,nave3,protegido,puroContenedor,nave12,nave11,otroCañon,desbalanceado],
	nave9 ~=? mayorCapacidad [nave1,nave9,nave2,nave3,protegido,puroContenedor,nave11,otroCañon,desbalanceado]
  ]




testsEj4 = test [
  puroContenedor ~=? transformar (\c-> Contenedor) nave2,
  puroContenedor ~=? transformar (\c-> Contenedor) nave3,
  nave13         ~=? transformar (\c->if c == Escudo then Cañón else c) nave5,
  nave5          ~=? transformar (\c->c) nave5,
  nave14         ~=? transformar (\c->if c == Escudo then Cañón else c) nave6,
  soloUnMotor    ~=? transformar(\c->Motor) contenedorSolo
  ]

testsEj5 = test [
    nave9 ~=? impactar (Babor, 0, Grande) nave9,
    contenedorSolo ~=? impactar (Babor, 0, Torpedo) nave9,
    nave11 ~=? impactar (Babor, 1, Grande) nave10,
    nave12 ~=? impactar (Babor, 2, Grande) nave10,
    (Base Contenedor) ~=? impactar (Babor, 0, Pequeño) nave1
  ]

testsEj6 = test [
  nave1 ~=? maniobrar nave1  [(Babor,1,Grande),(Babor,2,Torpedo),(Estribor, 1, Pequeño)],
	puroContenedor ~=? maniobrar nave7 [(Babor,1,Grande),(Babor,2,Torpedo),(Estribor, 1, Pequeño)]
  ]

testsEj7 = test [
  3 ~=? length (pruebaDeFuego [(Babor,1,Grande),(Babor,2,Torpedo),(Estribor, 1, Pequeño)] [nave1,nave2,nave3,nave4,nave5,nave6,nave7,nave8,nave9])
  ]

testsEj8 = test [
  (4,6) ~=? (dimensiones $ maniobrar nave9 [(Babor,1,Grande),(Babor,2,Torpedo)]),
  (1,1) ~=? dimensiones nave1,
  (4,8) ~=? dimensiones nave9,
  (3,4) ~=? dimensiones nave4,
	(3,2) ~=? dimensiones desbalanceado
  --(4,6) ~=? (dimensiones $ maniobrar nave9 [(Babor,1,Grande),(Babor,2,Torpedo)])
  ]


--Ejemplos de referencia para maniobrar:
--maniobrar nave9 [(Babor, 0, Grande),(Babor,2,Torpedo),(Estribor,0,Pequeño)] destruye solo el subárbol izquierdo del subárbol izquierdo.
--maniobrar nave9 [(Estribor,0,Pequeño),(Babor,2,Torpedo),(Babor, 1, Grande)] destruye todo el subárbol izquierdo.
