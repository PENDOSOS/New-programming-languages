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

padMatrix :: Num a => [[a]] -> [[a]]
padMatrix [] = [] 
padMatrix (firstRow:rows) = firstRow : map padRow rows
  where
    firstRowLength = length firstRow
    
    padRow row = replicate (firstRowLength - length row) 0 ++ row

jordanElimination :: [[Double]] -> [[Double]]
jordanElimination matrix = reverse $ jordanHelper (reverse matrix)
  where
    jordanHelper [] = []
    jordanHelper (row:rows) =
        --let rowNormalized = row
        let eliminateRow r = zipWith
                             (\a b -> if last (init r) >= 0 then a - b else a + b)
                             r
                             row
            newRows = map eliminateRow  rows
        in row : jordanHelper newRows

-- Решение СЛАУ
solveSLAU :: [[Double]] -> [Double]
solveSLAU matrix = map last $ jordanElimination $ padMatrix $ gaussianElimination matrix

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
            let gauss = gaussianElimination matrix
            let padded = padMatrix gauss
            printMatrix padded
            putStrLn "------------------"
            let jordan = jordanElimination padded
            printMatrix jordan
            writeMatrix outputFileName [solution]
            putStrLn "Решение записано в output.txt"
        else
            putStrLn "Ошибка: Матрица некорректна (неодинаковое количество элементов в строках)."
