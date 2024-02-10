import Data.Char (toUpper)
import Data.List (genericLength)

-- Ejercicio 1: Descuento
calcularDescuento :: Double -> Double -> Double
calcularDescuento precio descuento = precio - (precio * descuento / 100)

-- Función IVA
calcularIVA :: Double -> Double -> Double
calcularIVA precio iva = precio + (precio * (iva / 100))

-- Función que recibe un diccionario de precios
funcionEnCesta :: [(Double, Double)] -> (Double -> Double -> Double) -> Double
funcionEnCesta cesta funcion = sum [funcion precio descuento | (precio, descuento) <- cesta]

-- Ejemplo y reporte técnico de la función aplicarFuncionCesta
{-|
Ejemplo:
FuncionEnCesta [(100, 10), (200, 15), (150, 5)] calcularDescuento
Resultado esperado: 402.5-}


-- Definición de una función de ejemplo para sumar 1
sumar1 :: Int -> Int
sumar1 x = x + 1
-- Ejercicio 2: Aplica una función a cada elemento de la lista.
aplicarFuncionLista :: (entrada -> salida) -> [entrada] -> [salida]
-- Caso base: Lista vacía
aplicarFuncionLista _ [] = [] 
-- Caso recursivo: Aplicar a primer elemento y al resto
aplicarFuncionLista sumar1 (elemento : restoLista) = sumar1 elemento : aplicarFuncionLista sumar1 restoLista
-- Uso de la función aplicarFuncionLista con la función sumar1 y una lista de números
resultado :: [Int]
resultado = aplicarFuncionLista sumar1 [1, 2, 3, 4, 5]
-- Muestra el resultado
main :: IO ()
main = print resultado


-- Ejercicio 3: Función para calcular la longitud de cada palabra
-- Función para crear una lista de tuplas con palabras y sus longitudes
listaTuplasLongitudes :: String -> [(String, Int)]
listaTuplasLongitudes = zip <$> words <*> (map length . words)


-- Ejercicio 4: Función calificaciones
calificarEscala :: [(String, Int)] -> [(String, String)]
calificarEscala rendimiento = [(asignatura, convertirCalificacion nota) | (asignatura, nota) <- rendimiento]
  where
    convertirCalificacion :: Int -> String
    convertirCalificacion nota
      | nota >= 95 = "Excelente"
      | nota >= 85 = "Notable"
      | nota >= 75 = "Bueno"
      | nota >= 70 = "Suficiente"
      | otherwise = "Desempeño insuficiente"


-- Ejemplo y reporte técnico de la función obtenerCalificaciones
{-|
Ejemplo:
obtenerCalificaciones [("Matematicas", 90), ("Historia", 78), ("Ingles", 98)]
Resultado esperado: [("MATEMATICAS", "Bueno"), ("HISTORIA", "Desempeño insuficiente"), ("INGLES", "Excelente")]
--}

-- Calcular el modulo de un vector
calcularModuloVector :: [Float] -> Float
calcularModuloVector = sqrt . sum . map (^2)

-- Ejemplo
{-|
Ejemplo:
calcularModuloVector [3.0, 4.0]
Resultado esperado: 5.0

Reporte Técnico:
La función calcularModuloVector toma una lista de números de punto flotante y calcula el módulo del vector usando la fórmula matemática.
-}

-- Ejercicio 6: Función que identifica valores atípicos


-- Función encontrarValoresAtipicos
encontrarValoresAtipicos :: [Double] -> [Double]
encontrarValoresAtipicos muestra =
  let mediaMuestra = sum muestra / genericLength muestra
      desviacion = sqrt (sum (map (\x -> (x - mediaMuestra)^2) muestra) / genericLength muestra)
  in filter (\x -> abs ((x - mediaMuestra) / desviacion) > 3) muestra

-- Ejemplo y reporte técnico de la función encontrarValoresAtipicos
{-|
Ejemplo: encontrarValoresAtipicos [1.0, 2.0, 3.0, 10.0, 15.0, 100.0, 110.0]
Resultado esperado: [100.0, 110.0]

Reporte Técnico:
La función encontrarValoresAtipicos toma una lista de números de punto flotante y devuelve una nueva lista que contiene solo los valores atípicos.
Calcula la media y la desviación estándar de la muestra y filtra los valores que están a más de 3 desviaciones estándar de la media.
-}
