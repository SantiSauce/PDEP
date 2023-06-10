-- Un candidato tiene nombre, edad, carisma y capacidades. Las capacidades sirven para convencer a la gente (mejor explicado más adelante).


import Text.Show.Functions()
import Data.Bool (bool)


data Candidato = Candidato {
    nombre :: String,
    edad :: Double,
    carisma :: Double,
    capacidades :: [Capacidad]
} deriving Show

-- 1
-- Las capacidades del candidato para convencer son:
-- facha: (60 - edad del candidato)  + carisma del candidato * 3
-- liderazgo: edad del candidato * 10
-- riqueza: carisma del candidato + edad del candidato / 50
-- corrupto: resta 100 (en este caso no ayuda a convencer)
-- tiktoker: 100
-- -- flogger: 0 ( ya pasó de moda (?) )

type Capacidad = Candidato -> Double

facha :: Capacidad
facha candidato = (60 - edad candidato) + (carisma candidato * 3)

liderazgo :: Capacidad
liderazgo = (*10) . edad

riqueza :: Capacidad
riqueza candidato= carisma candidato + (edad candidato / 50)

corrupto :: Capacidad
corrupto _ = -100

tiktoker :: Capacidad
tiktoker _ = 100

flogger :: Capacidad
flogger _ = 0

candidatoPrueba :: Candidato
candidatoPrueba = Candidato "Santiago" 22 25 [facha]

-- Modelar a los siguientes candidatos:
-- Cintia, que tiene 40 años, su carisma es de 12 y las capacidades que tiene son liderazgo, riqueza y es tiktoker.
-- Marcos, que tiene 45 años, su carisma es de 10 y las capacidades que tiene son facha, liderazgo, corrupto.

cintia :: Candidato
cintia = Candidato "Cintia" 40 12 [liderazgo, riqueza, flogger]

marcos :: Candidato
marcos = Candidato "Marcos" 45 10 [facha, liderazgo]

-- Se desea saber si una persona tiene capacidades inútiles, se cumple cuando la persona tiene al menos una capacidad con convencimiento menor o igual a 0.
-- 	ej: tieneCapacidadInutil marcos 
--                 True


tieneCapacidadesInutiles :: Candidato -> Bool
tieneCapacidadesInutiles candidato = any (<=0)  (map (aplicarCapacidad candidato) (capacidades candidato))

aplicarCapacidad candidato capacidad = capacidad candidato

-- Saber los candidatos que entre sus capacidades no tienen ninguna que reste puntos o que no hagan nada.

tienenCapacidadesUtiles :: [Candidato] -> [Candidato]
tienenCapacidadesUtiles candidatos = filter (not . tieneCapacidadesInutiles) candidatos

-- Se desea poder evaluar las capacidades de un candidato y obtener el valor de la suma de su convencimiento. Se debe devolver un candidato evaluado: (nombre, sumaCapacidades). Si la suma es menor a 0 debe devolver 0.
-- 	ej: evaluarCandidato cintia
--        (“cintia”, 512.8)	(400 + 12 + 0.8 + 100)

data CandidatoEvaluado = CandidatoEvaluado {
    nombre2 :: String,
    sumaCapacidades :: Double
} deriving Show

evaluarCandidato :: Candidato -> CandidatoEvaluado
evaluarCandidato candidato = CandidatoEvaluado (nombre candidato) (sumarCapacidades candidato)

sumarCapacidades candidato = foldl (\acumulador capacidad -> acumulador + aplicarCapacidad candidato capacidad) 0 (capacidades candidato)

-- Definir la función “elMejor/3” que dado dos elementos y una operación define cuál es el mejor de los dos. Se considera que un elemento es mejor que otro, si la operación a ese elemento, éste tiene un mayor resultado que el segundo. Se espera como resultado el elemento. En caso de empate devuelve el primero.

elMejorDe3 :: Candidato -> Candidato -> Capacidad -> Candidato
elMejorDe3 candidato1 candidato2 capacidadAAplicar 
    | (aplicarCapacidad candidato1 capacidadAAplicar) >= (aplicarCapacidad candidato2 capacidadAAplicar) = candidato1
    | otherwise = candidato2

-- Definir la función “votacion/2” que dada una cantidad de votantes y una lista de candidatos devuelve una nueva lista con el nombre del candidato y la cantidad de votos que consiguió.
-- La cantidad de votos se define por: cantidadVotantes * convencimientoCandidato / totalConvencimientoDeTodosLosCandidatos.

votacion2 :: Double -> [Candidato] -> [(String, Double)]
votacion2 cantidadVotantes candidatos = map (calcularVotos cantidadVotantes candidat) candidatos


calcularVotos cantidadVotantes candidato = (nombre candidato, (fromIntegral cantidadVotantes * sumarCapacidades candidato / totalConvencimiento))

totalConvencimiento = sum (map sumarCapacidades candidatos)

