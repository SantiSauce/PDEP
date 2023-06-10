import Text.Show.Functions()

personaDePrueba :: Persona
personaDePrueba = Persona 18 [recibirse "diseñoDeInteriores", viajar ["paris"]] "Elías" 15 []

eugenia :: Persona
eugenia = Persona 22 [recibirse "diseñoDeInteriores", viajar ["paris"], enamorarse personaDePrueba] "Eugenia" 5000 []

martina :: Persona
martina =  Persona 20 [comboPerfecto,recibirse "medicina", enamorarse mateo, viajar["Barcelona"]] "Martina" 500 []

mateo :: Persona
mateo = Persona 21 [recibirse "arquitectura", viajar ["londres"]] "Mateo" 0 []

agustin :: Persona
agustin = Persona 20 [sigueTodoIgual] "Agustin" 100 []

type Suenio = Persona -> Persona


data Persona = Persona {
        edad :: Int,
        suenios :: [Suenio],
        nombre :: String,
        felicidonios :: Int,
        habilidades :: [String]
        } deriving Show



-- __Punto 1)__
muyFeliz :: Persona -> Bool
muyFeliz = (>100) . felicidonios  
moderadamenteFeliz :: Persona -> Bool
moderadamenteFeliz = (>50) . felicidonios 
pocoFeliz :: Persona -> Bool
pocoFeliz  = (<50) . felicidonios 


-- Punto a
coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion persona
    | muyFeliz persona = felicidonios persona * edad persona
    | moderadamenteFeliz persona = length (suenios persona) * felicidonios persona
    | otherwise = div (felicidonios persona) 2

-- Punto b
gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion persona
    | muyFeliz persona = felicidonios persona * length (suenios persona)
    | moderadamenteFeliz persona = edad persona * length (suenios persona)
    | otherwise = (length (suenios persona)) * 2


-- __Punto 2)__
-- Punto a
tieneNombreLargo :: Persona -> Bool
tieneNombreLargo  = (>10) . length . nombre

-- Punto b
esPersonaSuertuda :: Persona -> Bool
esPersonaSuertuda = (even . (*3) . coeficienteDeSatisfaccion) 

-- Punto c 
tieneNombreLindo :: Persona -> Bool
tieneNombreLindo = (== 'a') . last . nombre

-- __Punto 3)__

-- funciones auxiliares
sumarEdad :: Int -> Persona -> Persona
sumarEdad aniosASumar persona = persona { edad = edad persona + aniosASumar }

sumarFelicidonios :: Int -> Persona -> Persona
sumarFelicidonios felicidoniosASumar persona = persona { felicidonios = felicidonios persona + felicidoniosASumar }

agregarHabilidad :: String -> Persona -> Persona
agregarHabilidad nuevaHabilidad persona = persona { habilidades = habilidades persona++[nuevaHabilidad] }
-- funciones auxiliares

recibirse :: String -> Suenio
recibirse nuevaCarrera 
    = (sumarFelicidonios (1000 * length nuevaCarrera) . agregarHabilidad nuevaCarrera)

viajar :: [String] -> Suenio
viajar listaDeCiudades
    = (sumarEdad (length listaDeCiudades) . sumarFelicidonios (100 *  length listaDeCiudades)) 

enamorarse :: Persona -> Suenio
enamorarse amado = sumarFelicidonios (felicidonios amado)

sigueTodoIgual :: Suenio
sigueTodoIgual = id

comboPerfecto :: Suenio
comboPerfecto = (sumarFelicidonios 100) . (viajar ["Paris", "Berazategui"]) . (recibirse "Medicina")

   
-- PARTE 2

-- __Punto 4)__

noTieneSuenios = null . suenios
find lista = head . (filter lista)
cumplirSuenio n persona 
    |noTieneSuenios persona = persona
    |otherwise = ((suenios persona) !! n) persona

quitarSuenios persona = persona{suenios = []}

type Fuente = (Persona -> Persona)


-- a) Fuente Minimalista
fuenteMinimalista :: Fuente
fuenteMinimalista persona = (cumplirSuenio 0 persona) {suenios = (tail . suenios) persona}

-- b) Fuente Copada

fuenteCopada :: Fuente
fuenteCopada persona = quitarSuenios (foldr ($) persona (suenios persona))

-- c) Fuente a Pedido
fuentePedido :: Int -> Fuente
fuentePedido = cumplirSuenio

-- d) Fuente Sorda
fuenteSorda :: Fuente
fuenteSorda = id

-- __Punto 5)__ Fuente ganadora
fuenteGanadora :: Ord a => Persona -> [Fuente] -> (Persona -> a) -> (a -> a -> Bool) -> Fuente
fuenteGanadora persona fuentes caracteristica comparacion = foldl1 (compararFuentes persona caracteristica comparacion) fuentes
compararFuentes :: Ord a => Persona -> (Persona -> a) -> (a -> a -> Bool) -> Fuente -> Fuente -> Fuente 
compararFuentes persona caracteristica comparacion fuenteLista fuenteQueGana
    | comparacion (caracteristica (fuenteLista persona)) (caracteristica (fuenteQueGana persona)) = fuenteLista
    | otherwise = fuenteQueGana


listaDeFuentes1 = [fuenteMinimalista, fuenteCopada]
listaDeFuentes2 = [fuenteSorda, fuenteCopada]
listaDeFuentes3 = [fuenteSorda, fuenteMinimalista]
listaDeFuentes4 = [fuenteSorda]



-- __Punto 6)__ Reportes

sueniosValiosos :: Persona -> [Suenio]
sueniosValiosos persona = filter ((> 100) . felicidonios . ($ persona)) (suenios persona)

--
algunSuenioRaro :: Persona -> Bool
algunSuenioRaro persona = any ((== felicidonios persona) . felicidonios . ($ persona)) (suenios persona)

--
felicidadTotal :: [Persona] -> Int
felicidadTotal = sum . map (felicidonios.fuenteCopada)

-- __Punto 7)__

soniador = Persona {
    edad = 7,
    suenios = [],
    nombre = "Soñador",
    felicidonios = 100,
    habilidades = []
}

agregarSuenioInfinitasVeces persona suenio = persona { suenios = suenios persona ++ [suenio] ++ suenios (agregarSuenioInfinitasVeces persona suenio)}
soniadorInfinito = agregarSuenioInfinitasVeces soniador (recibirse "Ingenieria en Sistemas")

felicidoniosMinimalistas = felicidonios (fuenteMinimalista soniadorInfinito)
felicidoniosCopados = felicidonios (fuenteCopada soniadorInfinito)
felicidoniosAPedido n = felicidonios (fuentePedido n soniadorInfinito)
felicidoniosSordos = felicidonios (fuenteSorda soniadorInfinito)

{- Persona con sueños infinitos
    -A) Al invocar la "fuenteMinimalista" con el "soniadorInfinito" con infinitos sueños, 
        la fuente podrá ser aplicada y el primer sueño de la persona será cumplido.
    
    -B) Al invocar la "fuenteCopada" con el "soniadorInfinito" con infinitos sueños, 
        la fuente nunca terminará de ser aplicada ya que el "soniadorInfinito" siempre seguirá teniendo infinitos sueños que aplicar.
    
    -C) Al invocar la "fuentePedido" con el "soniadorInfinito" con infinitos sueños, y con un número de sueño deseado (por ejemplo 500789), 
        la fuente podrá ser aplicada y el sueño ubicado en la posición 500789 de la lista de sueños de la persona será cumplido.
    
    -D) Al invocar la "fuenteSorda" con el "soniadorInfinito" con infinitos sueños, la fuente podrá ser aplicada y la persona quedara igual.

    Este comportamiento se da debido a la evaluacion diferida de Haskell. 
    Esto significa que Haskell va a evaluar lo que queremos ejecutar hasta que cumpla con eso que queremos ejecutar.
    En el caso B, como queremos cumplir TODOS los sueños, Haskell nunca terminará de ejecutar la tarea.
-}