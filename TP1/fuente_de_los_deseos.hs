-- Defino Persona
type Persona = (Int, Int, String, Int, [String])


-- Punto 1
--  Parte a

coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion (edad, cantidadSueños, _, felicidonios, _)
    |   felicidonios > 100 = felicidonios * edad
    |   felicidonios <=100 && felicidonios > 50 = cantidadSueños * felicidonios
    |   otherwise = felicidonios `div` 2

-- Caso de prueba 1
persona1 :: Persona
persona1 = (25, 3, "Santiago", 101, ["Programar"])

-- Caso de prueba 2
persona2 :: Persona
persona2 = (21, 2, "Evangelina", 100, ["Programar"])

-- Caso de prueba 3
persona3 :: Persona
persona3 = (21, 3, "Santiago Roja", 50, ["Programar"])


--  Parte b

gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion (edad, cantidadSueños, _, felicidonios, _)
    |   felicidonios > 100 = felicidonios * cantidadSueños
    |   felicidonios <= 100 && felicidonios > 50 = edad * cantidadSueños
    |   otherwise = cantidadSueños * 2 

-- Caso de prueba 4
persona4 :: Persona
persona4 = (25, 2, "Santiago", 101, ["Programar"])

-- Caso de prueba 5
persona5 :: Persona
persona5 = (26, 2, "Santiago", 100, ["Programar"])

-- Caso de prueba 6
persona6 :: Persona
persona6 = (21, 1, "Maximiliano", 12, ["Programar"])

-- ---------------------------------------------------------

-- Punto 2
--  Parte a

tieneNombreLargo :: Persona -> Bool
tieneNombreLargo (_, _, nombre, _, _) = ((>10) . length) nombre

--  Parte b

esPersonaSuertuda :: Persona -> Bool
esPersonaSuertuda =  even . (*3) . coeficienteDeSatisfaccion

--  Parte c

tieneNombreLindo :: Persona -> Bool
tieneNombreLindo  (_, _, nombre, _, _) = ((== 'a') . last) nombre

-- Punto 3

recibirseDeUnaCarrera :: Persona -> String -> Persona
recibirseDeUnaCarrera (edad, sueños, nombre, felicidonios, habilidad) carrera = (edad, sueños, nombre, felicidonios + ((length carrera) * 1000), habilidad ++ [carrera]) 

viajarACiudades :: Persona -> [String] -> Persona
viajarACiudades (edad, sueños, nombre, felicidonios, habilidad) ciudades = (edad + 1, sueños, nombre, felicidonios + length ciudades, habilidad)

enamorarseDeOtraPersona :: Persona -> Persona -> Persona
enamorarseDeOtraPersona (edad, sueños, nombre, felicidonios, habilidad) (edad2, sueños2, nombre2, felicidonios2, habilidad2) = (edad, sueños, nombre, felicidonios + felicidonios2, habilidad)

todoSigueIgual :: Persona -> Persona
todoSigueIgual persona = persona

comboPerfecto :: Persona -> Persona
-- comboPerfecto (edad, sueños, nombre, felicidonios, habilidad) = viajarACiudades (recibirseDeUnaCarrera "medicina" (edad, sueños, nombre, felicidonios + 100, habilidad)) ["berazategui", "paris"]
comboPerfecto persona = (viajarACiudades . recibirseDeUnaCarrera persona) "medicina" ["paris", "berazategui"] 
-- falta sumar los 100 felicidonios