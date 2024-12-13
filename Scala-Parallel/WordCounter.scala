import java.io.File
import scala.io.Source
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object WordCounter extends App {
  val file = new File("123.txt")

  // Разбиваем файл на несколько частей
  val parts = splitFile(file, 5)

  // Создаем Future для каждой части файла
  val futures: List[Future[Int]] = parts.map(countWordsInPart).toList

  // Ожидаем завершения всех Future и получаем результаты
  val results = Await.result(Future.sequence(futures), Duration.Inf)

  // Объединяем результаты и выводим общее количество слов в файле
  val totalWordCount = results.sum
  println(s"Total word count: $totalWordCount")

  // Функция для разбития файла на части
  def splitFile(file: File, numParts: Int): Array[File] = {
    val fileLength = file.length()
    val partSize = fileLength / numParts

    (0 until numParts).map { partIndex =>
      val partStart = partIndex * partSize
      val partEnd = if (partIndex == numParts - 1) fileLength else (partIndex + 1) * partSize

      val partFile = new File(s"${file.getAbsolutePath}.part$partIndex")
      val randomAccessFile = new java.io.RandomAccessFile(file, "r")
      randomAccessFile.seek(partStart)

      val partWriter = new java.io.FileWriter(partFile)
      var bytesRead = 0
      while (bytesRead < partEnd - partStart) {
        partWriter.write(randomAccessFile.read())
        bytesRead += 1
      }

      // Убедимся, что последнее слово в части не было разделено на две части
      while (randomAccessFile.read() != ' '.toByte && randomAccessFile.getFilePointer < fileLength) {
        partWriter.write(randomAccessFile.read())
      }

      partWriter.close()
      partFile
    }.toArray
  }

  // Функция для подсчета слов в части файла
  def countWordsInPart(file: File): Future[Int] = Future {
    val source = Source.fromFile(file)
    val wordCount = source.getLines().flatMap(_.split("\\s+")).size
    source.close()
    wordCount
  }
}