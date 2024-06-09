



data Ninja = Ninja {
nombreNinja :: String,
herramientas :: [Herramienta],
rangoDelNinja :: Float,
jutsu :: [Jutsu]
}deriving (Eq)

data Herramienta = Herramienta {
nombreHerramienta :: String,
cantidadDisponible :: Float
}deriving (Eq)

kunai = Herramienta {
nombreHerramienta= "kunai",
cantidadDisponible = 3
}

shurikan = Herramienta{
nombreHerramienta = "shurikan",
cantidadDisponible =3
} 

data Mision = Mision {
cantidadRequerida :: Int,
rangoRecomendable :: Float,
ninjasEnemigos :: [Ninja],
recompensa :: Herramienta 
}


obtenerHerramientas :: Float -> Herramienta -> Ninja -> Ninja 
obtenerHerramientas cantidadEspecifica unaHerramienta unNinja 
    | limiteDeHerramientas unNinja cantidadEspecifica = mapHerramientas (mapCantidadDiponible (+ cantidadEspecifica) unaHerramienta :) unNinja  
    | otherwise = mapHerramientas (mapCantidadDiponible (+ cantidadLimite ) unaHerramienta :) unNinja

    where cantidadLimite = 100 - cantidadEspecifica - sumaDeHerramientas unNinja

limiteDeHerramientas :: Ninja -> Float -> Bool 
limiteDeHerramientas unNinja cantidadEspecifica =  sumaDeHerramientas unNinja + cantidadEspecifica <= 100

sumaDeHerramientas :: Ninja -> Float 
sumaDeHerramientas unNinja = (sum.map cantidadDisponible) (herramientas unNinja)

mapHerramientas :: ([Herramienta] -> [Herramienta]) -> Ninja -> Ninja
mapHerramientas f unNinja = unNinja {herramientas = f $ herramientas unNinja }

mapCantidadDiponible :: (Float -> Float) -> Herramienta -> Herramienta
mapCantidadDiponible f unaHerramienta = unaHerramienta {cantidadDisponible = f $ cantidadDisponible unaHerramienta}

usarHerramienta :: Herramienta -> Ninja -> Ninja
usarHerramienta = eliminar  

eliminar :: Herramienta -> Ninja -> Ninja 
eliminar unaHerramienta  = mapHerramientas (filter (/= unaHerramienta)) 

-- PARTE B -- a 

esDesafiante :: Mision -> [Ninja] -> Bool 
esDesafiante unaMision grupoDeNinjas = algunoAmateur grupoDeNinjas unaMision && sonUnaBanda unaMision

algunoAmateur :: [Ninja] -> Mision -> Bool 
algunoAmateur grupoDeNinjas unaMision = any ((<rangoRecomendable unaMision). rangoDelNinja) grupoDeNinjas

sonUnaBanda :: Mision -> Bool 
sonUnaBanda  = (>=2). length .ninjasEnemigos 


-- PARTE B -- b
esCopada :: Mision -> Bool 
esCopada unaMision = elem (recompensa unaMision) recompensaCopada

recompensaCopada :: [Herramienta]
recompensaCopada = [shurikan,kunai]


--PARTE B -- c 
esFactible :: [Ninja] -> Mision -> Bool 
esFactible grupoDeNinjas unaMision = (not.esDesafiante unaMision)  grupoDeNinjas && ninjasSuficientes unaMision grupoDeNinjas || totalHerramientasGrupo grupoDeNinjas > 500


ninjasSuficientes :: Mision -> [Ninja] -> Bool 
ninjasSuficientes unaMision grupoDeNinjas = length grupoDeNinjas >= cantidadRequerida unaMision

totalHerramientasGrupo :: [Ninja] -> Float 
totalHerramientasGrupo = sum.map sumaDeHerramientas  

-- PARTE B - d 

fallarMision :: Mision -> [Ninja] -> [Ninja]
fallarMision unaMision =  disminuirRangos.recortarBurros unaMision 

disminuirRangos :: [Ninja] -> [Ninja] 
disminuirRangos  = map (mapRangos (subtract 2))


recortarBurros :: Mision -> [Ninja] -> [Ninja] 
recortarBurros unaMision = filter ((== rangoRecomendable unaMision). rangoDelNinja)  

mapRangos  :: (Float -> Float) -> Ninja -> Ninja 
mapRangos f unNinja = unNinja {rangoDelNinja = f $ rangoDelNinja unNinja}

-- PARTE B - e 

cumplirMision :: Mision -> [Ninja] -> [Ninja]
cumplirMision unaMision = obtenerRecompensa unaMision.promocionarRango 

promocionarRango :: [Ninja] -> [Ninja] 
promocionarRango = map (mapRangos (+1))


obtenerRecompensa :: Mision -> [Ninja] -> [Ninja]
obtenerRecompensa unaMision = map (darleRecompensaIndividual (recompensa unaMision)) 

darleRecompensaIndividual :: Herramienta -> Ninja -> Ninja
darleRecompensaIndividual unaHerramienta = obtenerHerramientas (cantidadDisponible unaHerramienta) unaHerramienta  

-- PUNTO B -- f
type ClonesDeSombra = Int 
type Jutsu = Mision -> Mision
clonesDeSombra :: ClonesDeSombra -> Jutsu 
clonesDeSombra cantidadDeClones unaMision  = reducirNinjasRequeridos unaMision cantidadDeClones


reducirNinjasRequeridos :: Mision -> ClonesDeSombra -> Mision
reducirNinjasRequeridos unaMision cantidadDeClones = mapRequeridos (subtract cantidadDeClones) unaMision

mapRequeridos :: (Int -> Int ) -> Mision -> Mision
mapRequeridos f unaMision = unaMision {cantidadRequerida = max 1 .f $ cantidadRequerida unaMision} 



fuerzaDeUnCentenar :: Jutsu
fuerzaDeUnCentenar  = reducirEnemigosDebiles  

reducirEnemigosDebiles :: Mision ->  Mision 
reducirEnemigosDebiles  = mapEnemigos (filter rangoBajo) 

mapEnemigos :: ([Ninja] -> [Ninja]) -> Mision -> Mision 
mapEnemigos f unaMision = unaMision {ninjasEnemigos = f $ ninjasEnemigos unaMision}

rangoBajo :: Ninja -> Bool 
rangoBajo = (<500).rangoDelNinja

-- HACER LA MISION --

seCumpleLaMision :: [Ninja] -> Mision -> Bool  
seCumpleLaMision grupoDeNinjas unaMision = (esCopada.aplicarJutsusIndividuales grupoDeNinjas ) unaMision || (esFactible grupoDeNinjas.aplicarJutsusIndividuales grupoDeNinjas) unaMision

aplicarJutsusIndividuales :: [Ninja] -> Mision -> Mision 
aplicarJutsusIndividuales grupoDeNinjas unaMision = foldl (\x f -> f x ) unaMision (jutsuDeNinja grupoDeNinjas)

jutsuDeNinja :: [Ninja] -> [Jutsu]
jutsuDeNinja = map jutsu 

-- PARTE C --
granGuerraNinja = Mision {
cantidadRequerida = 100000,
rangoRecomendable = 100,
ninjasEnemigos = infinitosZetsu,
recompensa = abanicoUchiha
}

abanicoUchiha = Herramienta {
nombreHerramienta = "abanico",
cantidadDisponible = 1
}

infinitosZetsu :: [Ninja]
infinitosZetsu = map zetsu [1..]

zetsu :: Int -> Ninja  
zetsu unNumero = Ninja ("zetsu" ++ show unNumero) [] 600 [] 

--Se puede llevar a cabo ya que la cantidad de enemigos termina de evaluar dps de evaluar 2 y el rango requerido lo evalua sobra una cantidad finita de ninjas