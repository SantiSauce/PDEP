import Text.Show.Functions
import Data.List
import Data.String (IsString)

type Inscripciones = [Inscripcion]
type Inscripcion = (Anio, [(Inscripto, NivelDePelea)])
type Anio = Int
type Inscripto = String
type NivelDePelea = Int

type Torneos = [Torneo]
type Torneo = (Anio, Rondas)
type Rondas = [Ronda]
type Ronda = [Combate]
type Combate = (Inscripto, Inscripto, TiempoDePeleaEnMinutos)
type TiempoDePeleaEnMinutos = Int

inscripciones :: Inscripciones
inscripciones = [
    (2006, [("Goku", 80000), ("Tenshinhan", 35000), ("Roshi", 30000), ("Chaos", 1000), ("MrSatan", 20000), ("Kibito", 42000), ("Yamcha", 30000)]),
    (2007, [("goku",85000), ("gohan",70000), ("piccolo",63000),("mrSatan",20000), ("yamcha",3000), ("mightyMask",15000)]),
    (2008, [("goku",85000), ("gohan",70000), ("piccolo",63000), ("mrSatan",20000), ("vegeta",80000), ("tenshinhan",35000), ("roshi",30000)])
]

torneos :: Torneos
torneos = [
    (2006,[
        [("boo","kibito",19), ("roshi","yamcha",100), ("tenshinhan","chaos",47), ("goku","mrSatan",1)],[("goku","tenshinhan",756), ("boo","roshi",2)], [("goku","boo",1440)]
        ]
    ),
    (2007,[
        [("gohan","freezer",7), ("spopovich","androide18",21), ("piccolo","roque",23), ("supremoKaio","krillin",91), ("mrSatan", "roshi",32), ("mightyMask", "raditz",9), ("goku","vegeta",16),("yamcha","cell",104)], [("gohan","spopovich",10), ("piccolo","supremoKaio",68), ("mrSatan", "mightyMask",36), ("goku","yamcha",9)], [("goku","piccolo",115), ("gohan","mrSatan",15)], [("goku","gohan",270)]
        ]
    ),
    (2008,[
        [("goku","trunks",150), ("piccolo","mrSatan",20), ("vegeta","gohan",300), ("roshi","tenshinhan",180)],[("goku","mrSatan",30), ("vegeta","roshi",40)], [("goku","vegeta",400)]
        ]
    )
]

-- --------------------------------------------------------------------
-- Funciones Basicas
--  Punto 1
--      Item a
concatenarListas :: [[a]] -> [a]
concatenarListas = concat

valueAtKey :: [(a, b)] -> a -> b
valueAtKey elementoABuscar []= 