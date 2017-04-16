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

--igualesComponentes:: (NaveEspacial->Int)->(NaveEspacial->Int)->(NaveEspacial->Int)->(NaveEspacial->Int)->NaveEspacial->NaveEspacial->Bool
--
igualesComponentes f1 f2 f3 f4 x y = f1 x == f1 y && f2 x == f2 y && f3 x == f3 y && f4 x == f4 y


--Ejercicio 1
foldNave :: (Componente->a)->(Componente->a->a->a)->NaveEspacial->a
foldNave f g (Base c) = f c
foldNave f g (Módulo c n1 n2) = g  c (foldNave f g n1) (foldNave f g n2)


--Ejercicio 2
capacidad :: NaveEspacial -> Int
capacidad = foldNave (toInt.esComponente Contenedor) (\c x y -> x + y + toInt(esComponente Contenedor c))


poderDeAtaque :: NaveEspacial -> Int
poderDeAtaque = foldNave(toInt.esComponente Cañón) (\c x y -> x + y + toInt(esComponente Cañón c))


puedeVolar :: NaveEspacial -> Bool
puedeVolar = foldNave (esComponente Motor) (\c x y -> x || y || (esComponente Motor c))


mismoPotencial :: NaveEspacial -> NaveEspacial -> Bool
mismoPotencial =(\x y ->(capacidad x)==(capacidad y) &&(poderDeAtaque x)==(poderDeAtaque y) && (poderDeDefensa x) == (poderDeDefensa y) && (poderDeVuelo x) == (poderDeVuelo y))
--mismoPotencial = igualesComponentes capacidad poderDeAtaque poderDeDefensa poderDeVuelo


----Ejercicio 3

--PRE: La lista es no vacía.
mayorCapacidad :: [NaveEspacial] -> NaveEspacial
mayorCapacidad = foldr (\x y -> if((capacidad x)<(capacidad y)) then y else x) (Base Motor)

----Ejercicio 4

transformar :: (Componente -> Componente) -> NaveEspacial -> NaveEspacial
transformar f = foldNave (\c-> Base (f c)) (\c x y->Módulo (f c) x y )

---- Ejercicio 5
fstTripla :: (a,b,c) -> a
fstTripla (x,_,_) =  x

sndTripla :: (a,b,c) -> b
sndTripla (_,y,_) = y

thrdTripla :: (a,b,c) -> c
thrdTripla (_,_,z) = z

--En el caso Base c puse lo mismo que en defiendeTipoPeligroPequeño porque me sale fuera de scope. Mejorar

desenlaceAlImpacto :: TipoPeligro -> NaveEspacial -> NaveEspacial
desenlaceAlImpacto t (Base c) = if t==Pequeño && esComponente Escudo c then (Base c) else Base Contenedor
desenlaceAlImpacto t (Módulo c n1 n2)
 | defiendeTipoPeligroGrande || defiendeTipoPeligroPequeño = (Módulo c n1 n2)
 | otherwise = (Base Contenedor)
 where defiendeTipoPeligroGrande = t==Grande && esComponente Escudo c && (poderDeAtaque n1 >0 || poderDeAtaque n2 >0)
       defiendeTipoPeligroPequeño = t==Pequeño && esComponente Escudo c



impactar :: Peligro -> NaveEspacial -> NaveEspacial
impactar p (Base c)
 | (sndTripla p) == 0 = desenlaceAlImpacto (thrdTripla p) (Base c)
 | otherwise = (Base c)
impactar p (Módulo c n1 n2)
 | (sndTripla p) == 0 = desenlaceAlImpacto (thrdTripla p) (Módulo c n1 n2)
 | (fstTripla p) == Babor = (Módulo c (impactar (fstTripla p, (sndTripla p) -1, thrdTripla p) n1) n2)
 | (fstTripla p) == Estribor = (Módulo c n1 (impactar (fstTripla p, (sndTripla p) -1, thrdTripla p) n2))
--el patron de recursion del ejercicio no se ajusta con el esquema de foldNave y seria poco declarativo

---- Ejercicio 6
maniobrar :: NaveEspacial -> [Peligro] -> NaveEspacial
maniobrar = foldl (flip impactar)

---- Ejercicio 7
pruebaDeFuego :: [Peligro] -> [NaveEspacial] -> [NaveEspacial]
pruebaDeFuego = (\x y -> filter puedeVolar  [(flip maniobrar) x i| i <-y ])

--pruebaDeFuego = (filter puedeVolar) (foldl (flip maniobrar))


---- Ejercicio 8

--Uhm, esto parece funcionar....
--El parametro a ahora es int->int. La idea la tomé del ejercicio de take que hicimos en clase, pensando a foldr como una funcion que devuelve funciones.

componentesPorNivel :: NaveEspacial -> Int -> Int
componentesPorNivel n x = foldNave(\_ -> \i -> toInt $ i == 0) (\_ frecu grecu -> \i -> toInt(i==0) + toInt(i/=0)*frecu(i-1) + grecu(i-1)) n x

dimensiones :: NaveEspacial -> (Int, Int)
dimensiones = foldNave (\c -> (1,1)) (\c x y -> ((max (fst x) (fst y)) + 1, 1))
--Falta la parte del ancho
