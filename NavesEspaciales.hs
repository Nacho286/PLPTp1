module NavesEspaciales (Componente(Contenedor, Motor, Escudo, Cañón), NaveEspacial(Módulo, Base), Dirección(Babor, Estribor), TipoPeligro(Pequeño, Grande, Torpedo), Peligro,foldNave, capacidad, poderDeAtaque, puedeVolar, mismoPotencial, mayorCapacidad, transformar, impactar, maniobrar, pruebaDeFuego, componentesPorNivel, dimensiones) where

data Componente = Contenedor | Motor | Escudo | Cañón deriving (Eq, Show)

data NaveEspacial = Módulo Componente NaveEspacial NaveEspacial | Base Componente deriving Eq

data Dirección = Babor | Estribor deriving Eq

data TipoPeligro = Pequeño | Grande | Torpedo deriving Eq

type Peligro = (Dirección, Int, TipoPeligro)

instance Show NaveEspacial where
  show = ("\n" ++) . (padNave 0 0 False)

padNave nivel acum doPad (Base c) = (if doPad then pad (4*nivel + acum) else "") ++ show c
padNave nivel acum doPad (Módulo x i d) = (if doPad then pad (4*nivel + acum) else "") ++ show x ++
            pad 4 ++ padNave (nivel+1) (acum+l) False i ++ "\n" ++
            padNave (nivel+1) (acum+l) True d where l = length $ show x

pad :: Int -> String
pad i = replicate i ' '

--Ejercicio 1
foldNave :: (Componente->a)->(Componente->a->a->a)->NaveEspacial->a
foldNave f g (Base c) = f c
foldNave f g (Módulo c n1 n2) = g  c (foldNave f g n1) (foldNave f g n2)


--Auxiliar?---
toInt:: Bool-> Int
toInt True = 1
toInt False = 0

esComponente:: Componente-> Componente->Bool
esComponente c1 c2 = c1 == c2

poderDeVuelo:: NaveEspacial -> Int
poderDeVuelo = foldNave(toInt.esComponente Motor) (\c x y -> x + y + toInt(esComponente Motor c))

poderDeDefensa:: NaveEspacial -> Int
poderDeDefensa = foldNave(toInt.esComponente Escudo) (\c x y -> x + y + toInt(esComponente Escudo c))

--Ejercicio 2
capacidad :: NaveEspacial -> Int
capacidad = foldNave (toInt.esComponente Contenedor) (\c x y -> x + y + toInt(esComponente Contenedor c))


poderDeAtaque :: NaveEspacial -> Int
poderDeAtaque = foldNave(toInt.esComponente Cañón) (\c x y -> x + y + toInt(esComponente Cañón c))


puedeVolar :: NaveEspacial -> Bool
puedeVolar = foldNave (esComponente Motor) (\c x y -> x || y || (esComponente Motor c))

mismoPotencial :: NaveEspacial -> NaveEspacial -> Bool
mismoPotencial =(\x y ->(capacidad x)==(capacidad y) &&(poderDeAtaque x)==(poderDeAtaque y) && (poderDeDefensa x) == (poderDeDefensa y) && (poderDeVuelo x) == (poderDeVuelo y))

----Ejercicio 3

mayorCapacidad :: [NaveEspacial] -> NaveEspacial
mayorCapacidad = foldr (\x y -> if((capacidad x)<(capacidad y)) then y else x) (Base Motor)

----Ejercicio 4

transformar :: (Componente -> Componente) -> NaveEspacial -> NaveEspacial
transformar = undefined

---- Ejercicio 5
impactar :: Peligro -> NaveEspacial -> NaveEspacial
impactar = undefined

---- Ejercicio 6
maniobrar :: NaveEspacial -> [Peligro] -> NaveEspacial
maniobrar = foldl (flip impactar)

---- Ejercicio 7
pruebaDeFuego :: [Peligro] -> [NaveEspacial] -> [NaveEspacial]
pruebaDeFuego = (\x y -> filter puedeVolar  [(flip maniobrar) x i| i <-y ])
--pruebaDeFuego = (filter puedeVolar) (foldl (flip maniobrar))

---- Ejercicio 8
componentesPorNivel :: NaveEspacial -> Int -> Int
componentesPorNivel = undefined

dimensiones :: NaveEspacial -> (Int, Int)
dimensiones = undefined
