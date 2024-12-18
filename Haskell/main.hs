import System.IO
import Data.List

-- Чтение матрицы из файла
readMatrix :: FilePath -> IO [[Double]]
readMatrix fileName = do
    content <- readFile fileName
    let rows = lines content
    return $ map (map read . words) rows

-- Запись матрицы в файл
writeMatrix :: FilePath -> [[Double]] -> IO ()
writeMatrix fileName matrix = do
    let content = unlines $ map (unwords . map show) matrix
    writeFile fileName content

-- Приведение матрицы к ступенчатому виду методом Гаусса
gaussianElimination :: [[Double]] -> [[Double]]
gaussianElimination [] = []
gaussianElimination (row:rows) =
    let rowPivot = head row
        rowNormalized = map (/ rowPivot) row
        eliminateRow r = zipWith (-) r (map (* head r) rowNormalized)
        newRows = map eliminateRow rows
    in rowNormalized : gaussianElimination (filter (not . null) (map (drop 1) newRows))

padMatrix :: [[a]] -> [[a]]
padMatrix matrix = map padRow matrix
  where
    -- Вычисляем длину первой строки
    firstRowLength = length (head matrix)
    
    -- Функция для добавления нулей в начало строки
    padRow row = replicate (firstRowLength - length row) 0 ++ row

-- Приведение к каноническому виду методом Жордана
jordanElimination :: [[Double]] -> [[Double]]
jordanElimination matrix = reverse $ jordanHelper (reverse matrix)
  where
    jordanHelper [] = []
    jordanHelper (row:rows) =
        let rowPivot = head row
            rowNormalized = row
            eliminateRow r = zipWith
                             (\a b -> if last (init r) >= 0 then a - b else a + b)  -- Вычисляем, вычитать или складывать
                             r
                             (0:rowNormalized)  -- Сопоставляем с нормализованной строкой
            newRows = map eliminateRow  rows
        in rowNormalized : jordanHelper newRows

-- Решение СЛАУ
solveSLAU :: [[Double]] -> [Double]
solveSLAU matrix = map last $ jordanElimination $ gaussianElimination matrix

printMatrix :: Show a => [[a]] -> IO ()
printMatrix matrix = mapM_ putStrLn (map (unwords . map show) matrix)

-- Основная функция
main :: IO ()
main = do
    -- Входной файл
    let inputFileName = "input.txt"
    -- Выходной файл
    let outputFileName = "output.txt"

    -- Чтение матрицы
    matrix <- readMatrix inputFileName

    -- Проверка на корректность матрицы
    if all ((== length (head matrix)) . length) matrix
        then do
            -- Решение СЛАУ
            let solution = solveSLAU matrix
            -- Запись результата в файл
            let gauss = gaussianElimination matrix
            printMatrix gauss
            let aboba = padMatrix matrix
            printMatrix aboba
            let jordan = jordanElimination gauss
            printMatrix jordan
            writeMatrix outputFileName [solution]
            putStrLn "Решение записано в output.txt"
        else
            putStrLn "Ошибка: Матрица некорректна (неодинаковое количество элементов в строках)."
