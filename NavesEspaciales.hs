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

---------------------------------- Funciones Auxiliares ----------------------------------
toInt:: Bool-> Int
toInt True = 1
toInt False = 0

mismosComponentes:: Componente-> Componente->Bool
mismosComponentes c1 c2 = c1 == c2

poderDeVuelo:: NaveEspacial -> Int
poderDeVuelo = foldNave(toInt.mismosComponentes Motor) (\c x y -> x + y + toInt(mismosComponentes Motor c))

poderDeDefensa:: NaveEspacial -> Int
poderDeDefensa = foldNave(toInt.mismosComponentes Escudo) (\c x y -> x + y + toInt(mismosComponentes Escudo c))

fstTripla :: (a,b,c) -> a
fstTripla (x,_,_) =  x

sndTripla :: (a,b,c) -> b
sndTripla (_,y,_) = y

thrdTripla :: (a,b,c) -> c
thrdTripla (_,_,z) = z

desenlaceAlImpacto :: TipoPeligro -> NaveEspacial -> NaveEspacial
desenlaceAlImpacto t (Base c) = if t==Pequeño && mismosComponentes Escudo c then (Base c) else Base Contenedor
desenlaceAlImpacto t (Módulo c n1 n2)
 | defiendeTipoPeligroGrande || defiendeTipoPeligroPequeño = (Módulo c n1 n2)
 | otherwise = (Base Contenedor)
 where defiendeTipoPeligroGrande = t==Grande && mismosComponentes Escudo c && (poderDeAtaque n1 >0 || poderDeAtaque n2 >0)
       defiendeTipoPeligroPequeño = t==Pequeño && mismosComponentes Escudo c

largo :: NaveEspacial -> Int
largo = foldNave (\c -> 1) (\c x y -> ((max  x y) + 1))

ancho :: NaveEspacial -> Int
ancho = (\c -> maximum [componentesPorNivel c i | i <-[0..largo c] ])



---------------------------------- Ejercicios ----------------------------------
--Ejercicio 1
foldNave :: (Componente->a)->(Componente->a->a->a)->NaveEspacial->a
foldNave f g (Base c) = f c
foldNave f g (Módulo c n1 n2) = g  c (foldNave f g n1) (foldNave f g n2)


--Ejercicio 2
capacidad :: NaveEspacial -> Int
capacidad = foldNave (toInt.mismosComponentes Contenedor) (\c x y -> x + y + toInt(mismosComponentes Contenedor c))


poderDeAtaque :: NaveEspacial -> Int
poderDeAtaque = foldNave(toInt.mismosComponentes Cañón) (\c x y -> x + y + toInt(mismosComponentes Cañón c))


puedeVolar :: NaveEspacial -> Bool
puedeVolar = foldNave (mismosComponentes Motor) (\c x y -> x || y || (mismosComponentes Motor c))


mismoPotencial :: NaveEspacial -> NaveEspacial -> Bool
mismoPotencial =(\x y ->(capacidad x)==(capacidad y) &&(poderDeAtaque x)==(poderDeAtaque y) && (poderDeDefensa x) == (poderDeDefensa y) && (poderDeVuelo x) == (poderDeVuelo y))


----Ejercicio 3
--PRE: La lista es no vacía.
mayorCapacidad :: [NaveEspacial] -> NaveEspacial
mayorCapacidad = foldr (\x y -> if((capacidad x)<(capacidad y)) then y else x) (Base Motor)


----Ejercicio 4
transformar :: (Componente -> Componente) -> NaveEspacial -> NaveEspacial
transformar f = foldNave (\c-> Base (f c)) (\c x y->Módulo (f c) x y )


---- Ejercicio 5
impactar :: Peligro -> NaveEspacial -> NaveEspacial
impactar p (Base c)
 | (sndTripla p) == 0 = desenlaceAlImpacto (thrdTripla p) (Base c)
 | otherwise = (Base c)
impactar p (Módulo c n1 n2)
 | (sndTripla p) == 0 = desenlaceAlImpacto (thrdTripla p) (Módulo c n1 n2)
 | (fstTripla p) == Babor = (Módulo c (impactar (fstTripla p, (sndTripla p) -1, thrdTripla p) n1) n2)
 | (fstTripla p) == Estribor = (Módulo c n1 (impactar (fstTripla p, (sndTripla p) -1, thrdTripla p) n2))
--el patron de recursion del ejercicio no se ajusta con el esquema de foldNave, pues solo se hacen llamados recursivos para una rama de la nave.
--Ademas la codificación quedaria poco declarativa.


---- Ejercicio 6
maniobrar :: NaveEspacial -> [Peligro] -> NaveEspacial
maniobrar = foldl (flip impactar)


---- Ejercicio 7
pruebaDeFuego :: [Peligro] -> [NaveEspacial] -> [NaveEspacial]
pruebaDeFuego = (\x y -> filter puedeVolar  [(flip maniobrar) x i| i <-y ])
--Decidimos devolver las naves que son capaces de pasar la prueba.


---- Ejercicio 8
componentesPorNivel :: NaveEspacial -> Int -> Int
componentesPorNivel n x = foldNave(\_ -> \i -> toInt $ i == 0) (\_ frecu grecu -> \i -> toInt(i==0) + toInt(i/=0)*frecu(i-1) + grecu(i-1)) n x


dimensiones :: NaveEspacial -> (Int, Int)
dimensiones = (\c->(largo c, ancho c))
