# Текстовый редактор на Ruby

def create_file(filename)
    File.open(filename, 'w+')
  end
  
  def open_file(filename)
    File.open(filename, 'r+')
  end
  
  def save_file(file)
    #file.rewind
    #file.truncate(0)
    puts "Введите текст (для завершения ввода нажмите Ctrl+D):"
    text = $stdin.read
    file.write(text)
    file.flush
    puts "Файл успешно сохранен."
  end
  
  def display_file(file)
    file.rewind
    content = file.read
    puts "Содержимое файла:"
    puts content
  end
  
  def main_menu
    puts "Текстовый редактор"
    puts "1. Создать файл"
    puts "2. Открыть файл"
    puts "3. Сохранить файл"
    puts "4. Выйти из редактора"
    print "Выберите действие: "
    choice = gets.chomp.to_i
    choice
  end
  
  file = nil
  
  loop do
    choice = main_menu
  
    case choice
    when 1
      print "Введите имя файла: "
      filename = gets.chomp
      file = create_file(filename)
      puts "Файл успешно создан."
    when 2
      print "Введите имя файла: "
      filename = gets.chomp
      file = open_file(filename)
      puts "Файл успешно открыт."
      display_file(file)
    when 3
      if file.nil?
        puts "Сначала создайте или откройте файл."
      else
        save_file(file)
      end
    when 4
      exit
    else
      puts "Неверный выбор. Пожалуйста, повторите."
    end
  end