-- Lo que entendimos es que contamos con juguetes, lo que nos interesa de los juguetes es que tienen un nombre, un dueño, un nivel de facha (número decimal), y varios accesorios. Además sabemos cuando están “vivos” y cuando no.

import Text.Show.Functions()
import Data.Bool (bool)


data Juguete = Juguete {
    nombre :: String,
    duenio :: String,
    facha :: Int,
    accesorios :: [Accesorio],
    vivo :: Bool
} deriving Show

data Accesorio = Accesorio {
    efecto :: Efecto,
    eficacia :: Int
} deriving Show

type Efecto = Double -> Juguete -> Juguete


lucirAmenazante :: Efecto
lucirAmenazante eficacia juguete = juguete {facha = facha juguete + 10 + eficacia}

vieneAndy :: Efecto
vieneAndy juguete = juguete {vivo = False}

masSteel :: Efecto
masSteel eficacia juguete = cambiar_nombre("Max Steel") . modificar_facha (eficacia * largoNombre juguete) $ juguete

quemadura :: Int -> Int -> Efecto
quemadura quemadura eficacia juguete = juguete { facha = facha juguete - (quemadura * (eficacia +2))}


largoNombre = length . nombre

modificar_facha cantidad juguete = juguete {
    facha = facha juguete + cantidad
}

cambiar_nombre nombre juguete = juguete {
    nombre = nombre
}
-- PUNTO 2
-- serpienteEnBota: es un accesorio cuyo efecto es lucirAmenzante y su eficacia es 2.
-- radio: es un accesorio cuyo efecto es vieneAndy y su eficacia es 3.
-- revolver: es un accesorio cuyo efecto es masSteel y su eficacia es 5.
-- escopeta: es un accesorio cuyo efecto es masSteel y su eficacia es 20.
-- lanzaLlamas: es un accesorio cuyo efecto es quemadura de grado 3, con una eficacia de 8,5.

serpienteEnBota :: Accesorio
serpienteEnBota = Accesorio {efecto = lucirAmenazante, eficacia = 2}

radio :: Accesorio
radio = Accesorio { efecto = vieneAndy, eficacia = 3}

revolver :: Accesorio
revolver = Accesorio { efecto = masSteel, eficacia = 5}

escopeta :: Accesorio
escopeta = Accesorio { efecto = masSteel, eficacia = 20}

lanzaLlamas :: Accesorio
lanzaLlamas = Accesorio { efecto = quemadura 3, eficacia = 8.5}

-- De los juguetes nos importan tres:
-- Woody cuyo dueño es Andy, está vivo, su facha es 100 y sus accesorios son una serpiente en la bota y un revólver.
-- Soldado, cuyo dueño es Andy, está vivo, su facha es 5 y sus accesorios son un lanza llamas y una radio.
-- Barbie, cuyo dueño es Dany, no está viva, su facha es 95,5 y sus accesorios son un lanza llamas, una escopeta y un revólver (obviamente los guarda todos en la cartera).

woody :: Juguete
woody = Juguete "woody" "Andy" 100 [serpienteEnBota, revolver] True

soldado :: Juguete
soldado = Juguete "soldado" "Andy" 5 [lanzaLlamas, radio] True

barbie :: Juguete
barbie = Juguete "barbie" "Dany" 95.5 [lanzaLlamas, escopeta, revolver] False

-- Nos gustaría saber si un juguete es impaktante. Esto se cumple cuando tiene al menos un accesorio con más de 10 de eficacia.

esJugueteImpaktante :: Juguete -> Bool
esJugueteImpaktante juguete = any (>10) (map eficacia (accesorios juguete))

-- Queremos saber si un juguete es disxélico. Un juguete es disxélico cuando el nombre tiene las mismas letras que “andy” pero en desorden.

esJugueteDislexico :: Juguete -> Bool
esJugueteDislexico = (== sort "andy") . sort . nombre

-- De un cajón de juguetes se desea saber:
-- Cuántos juguetes hay que sean impaktantes.
-- Cuántos juguetes hay cuya cantidad de letras del nombre sea mayor a 6.
-- Cuántos juguetes hay que sean disxélicos y no estén vivos.

cantidad_de_juguetes_impaktantes :: [Juguete] -> Int
cantidad_de_juguetes_impaktantes = length . filter esJugueteImpaktante

cantidad_de_juguetes_con_letras_mayor_a_6 :: [Juguete] -> Int
cantidad_de_juguetes_con_letras_mayor_a_6 = length . filter ((>6) . length . nombre) 

cantidad_de_juguetes_dislexicos_no_vivos :: [Juguete] -> Int
cantidad_de_juguetes_dislexicos_no_vivos = length . filter (esJugueteDislexico || (not . vivo))

-- Calcular el nivel de amor que tiene Andy por un juguete. El nivel de amor de un juguete se mide, luego de aplicar el efecto de sus accesorios, como: facha + cantidad de caracteres en el nombre * 5 - cantidad de accesorios * 7. Cabe aclarar que si el juguete está vivo este valor es el doble (es el sueño de cualquier pibe).
-- Ej: el nivel de amor de Woody sería 168 (137 + 9 * 5 - 2 * 7) pero como sigue vivo, es 336.


nivel_de_amor :: Juguete -> Int
nivel_de_amor juguete = calculo_amor . (aplicarAccesorios juguete )

calculo_amor juguete = (bool id (*2) (vivo juguete)) $ facha juguete + ((*5) . length . nombre) juguete - ((*7) . length . accesorios) juguete

aplicarAccesorios :: Juguete -> Juguete
aplicarAccesorios juguete = foldl aplicarAccesorio juguete (accesorios juguete)

aplicarAccesorio:: Juguete -> Accesorio -> Juguete
aplicarAccesorio juguete accesorio = 
    (efecto accesorio) (eficacia accesorio) juguete