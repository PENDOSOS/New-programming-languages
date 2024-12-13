(defun split-string (string delimiter)
  "Splits a string into a list of substrings based on a delimiter."
  (loop for i = 0 then (1+ j)
        as j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))

(defun convert-csv-to-json (csv-file json-file)
  (with-open-file (input csv-file :direction :input)
    (with-open-file (output json-file :direction :output :if-exists :supersede)
      ;; Write the opening bracket for the JSON array
      (format output "[")

      ;; Read the header line of the CSV file
      (let ((header (split-string (read-line input nil nil) #\,)))
        ;; Loop through the remaining lines of the CSV file
        (loop for line = (read-line input nil nil)
              while line
              do (let ((values (split-string line #\,)))
                   ;; Write the opening curly brace for the JSON object
                   (format output "{")

                   ;; Loop through the header and values, writing each as a JSON property
                   (loop for header-item in header
                         for value in values
                         do (format output "\"~a\": \"~a\"" header-item value)
                         unless (eql (car (last values)) value)
                         do (format output ","))

                   ;; Write the closing curly brace for the JSON object
                   (format output "}")

                   ;; If this is not the last line, write a comma to separate the objects
                   (unless (null (peek-char nil input nil nil))
                     (format output ",")))))

      ;; Write the closing bracket for the JSON array
      (format output "]"))))

(convert-csv-to-json "input.csv" "output.json")