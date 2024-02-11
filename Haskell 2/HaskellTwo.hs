import System.IO
import Data.Char(toUpper) 



-- EJERCICIO 1
-- Funciones para calcular el seno, coseno, tangente, exponencial y logaritmo neperiano
sinFunc :: Floating a => a -> a
sinFunc x = sin x
cosFunc :: Floating a => a -> a
cosFunc x = cos x
tanFunc :: Floating a => a -> a
tanFunc x = tan x
expFunc :: Floating a => a -> a
expFunc x = exp x
logFunc :: Floating a => a -> a
logFunc x = log x

-- Función para calcular y mostrar la tabla
calcularTabla :: String -> (Double -> Double) -> Double -> IO ()
calcularTabla nombreFuncion funcion valor = do
  putStrLn $ "Calculadora Científica - " ++ nombreFuncion
  putStrLn $ "Tabla para el valor " ++ show valor ++ ":"
  putStrLn $ replicate 25 '-'
  putStrLn $ "   Valor  |  Resultado"
  putStrLn $ replicate 25 '-'
  mapM_ (\x -> putStrLn $ "   " ++ show x ++ replicate (8 - length (show x)) ' ' ++ "|   " ++ show (funcion x)) [1..valor]

-- Función principal interactiva
main :: IO ()
main = do
  putStrLn "Seleccione una función:"
  putStrLn "1. Seno"
  putStrLn "2. Coseno"
  putStrLn "3. Tangente"
  putStrLn "4. Exponencial"
  putStrLn "5. Logaritmo Neperiano"
  putStrLn "0. Salir"

  putStr "Opción: "
  hFlush stdout
  opcion <- getLine

  case opcion of
    "1" -> do
      putStr "Ingrese un valor: "
      hFlush stdout
      input <- getLine
      let valor = read input :: Double
      calcularTabla "Seno" sinFunc valor
    "2" -> do
      putStr "Ingrese un valor: "
      hFlush stdout
      input <- getLine
      let valor = read input :: Double
      calcularTabla "Coseno" cosFunc valor
    "3" -> do
      putStr "Ingrese un valor: "
      hFlush stdout
      input <- getLine
      let valor = read input :: Double
      calcularTabla "Tangente" tanFunc valor
    "4" -> do
      putStr "Ingrese un valor: "
      hFlush stdout
      input <- getLine
      let valor = read input :: Double
      calcularTabla "Exponencial" expFunc valor
    "5" -> do
      putStr "Ingrese un valor: "
      hFlush stdout
      input <- getLine
      let valor = read input :: Double
      calcularTabla "Logaritmo Neperiano" logFunc valor
    "0" -> putStrLn "¡Hasta luego!"
    _   -> putStrLn "Opción no válida"


-- EJERCICIO 2
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []          -- Caso base: la lista vacía no tiene elementos
filtrar f (x:xs)
  | f x       = x : filtrar f xs  -- Si el elemento cumple la condición, lo agregamos a la nueva lista
  | otherwise = filtrar f xs       -- Si no cumple, lo ignoramos y continuamos con el resto de la lista

{-
-- Ejemplo de uso: Filtrar los números pares de una lista
esPar :: Int -> Bool
esPar x = x `mod` 2 == 0

listaOriginal :: [Int]
listaOriginal = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

resultadoFiltrado :: [Int]
resultadoFiltrado = filtrar esPar listaOriginal
-}

-- EJERCICIO 3 RECIBE LISTA Y REGRESA LISTA
calificacionesCorrespondientes :: [Int] -> [String]
calificacionesCorrespondientes [] = []  -- Caso base: la lista vacía devuelve una lista vacía
calificacionesCorrespondientes (x:xs) = determinarCalificacion x : calificacionesCorrespondientes xs
  where
    determinarCalificacion :: Int -> String
    determinarCalificacion nota
      | nota >= 95 = "Excelente"
      | nota >= 85 = "Notable"
      | nota >= 75 = "Bueno"
      | nota >= 70 = "Suficiente"
      | otherwise = "Desempeño insuficiente"


-- EJERCICIO 4 DICCIONARIO DE CALIFICACIONES
asignaturasYNotas :: [(String, Int)] -> [(String, String)]
asignaturasYNotas base = [(map toUpper materia, convertirCalifs calificacion) | (materia, calificacion) <- base]
  where
    convertirCalifs :: Int -> String
    convertirCalifs calificacion
      | calificacion >= 95 && calificacion <= 100 = "Excelente"
      | calificacion >= 85 && calificacion <= 94 = "Notable"
      | calificacion >= 75 && calificacion <= 84 = "Bueno"
      | calificacion >= 70 && calificacion <= 74 = "Suficiente"
      | otherwise = "Desempeño insuficiente"




-- EJERCICIO 5 EJERCICIO DE LOS INMUEBLES:
type Inmueble = (Int, Int, Int, Bool, Char) -- (año, metros, habitaciones, garaje, zona)

buscarInmueblesPorPresupuesto :: [Inmueble] -> Int -> [((Int, Int, Int, Bool, Char), Int)]
buscarInmueblesPorPresupuesto inmuebles presupuesto =
  [ (inmueble, calcularPrecio inmueble) | inmueble <- inmuebles, calcularPrecio inmueble <= presupuesto ]
  where
    calcularPrecio :: Inmueble -> Int
    calcularPrecio (año, metros, habitaciones, garaje, zona)
      | zona == 'A' = round $ fromIntegral (metros * 1000 + habitaciones * 5000 + if garaje then 15000 else 0) * (1 - fromIntegral (2024 - año) / 100)
      | zona == 'B' = round $ fromIntegral (metros * 1000 + habitaciones * 5000 + if garaje then 15000 else 0) * (1 - fromIntegral (2024 - año) / 100) * 1.5
      | otherwise = 0  -- Manejar otras zonas según sea necesario


{-
USO DE LA FUNCION:
> let inmuebles = [(2000, 100, 3, True, 'A'), (2012, 60, 2, True, 'B'), (1980, 120, 4, False, 'A'), (2005, 75, 3, True, 'B'), (2015, 90, 2, False, 'A')]
> buscarInmueblesPorPresupuesto inmuebles "presupuesto"

-}
