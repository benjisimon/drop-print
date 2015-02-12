;;
;; Work with our files
;;
(require <com.benjisimon.dropprint.imports>)

(define (file-handler (file :: File))
  (let ((name :: String (file:get-name)))
    (cond ((name:matches "^.*[.]txt$") text-handler)
          ((name:matches "^.*[.]scm$") scm-handler)
          (else unknown-handler))))

(define (text-handler feedback (file :: File) (buffer :: PrintStream) )
  (let ((fio :: FileInputStream (FileInputStream file)))
  (let next-byte ((byte :: int (fio:read)))
    (cond ((= -1 byte) #t)
          (else
           (buffer:write byte)
           (next-byte (fio:read)))))))

(define (scm-handler feedback (file :: File) (buffer :: PrintStream) )
  (feedback "Eval'ing and printing scheme")
  (let ((results (open-output-string)))
    (try-catch
     (begin
       (parameterize ((current-output-port results))
         (with-input-from-file (file:get-absolute-path)
           (lambda ()
             (let ((code (read)))
               (feedback "Eval'ing: " code)
               (eval code)))))
       (feedback "Eval result: " (get-output-string results))
       (buffer:print (get-output-string results))
       (buffer:print "\n\n"))
     (ex java.lang.Throwable
         (feedback "Error: " ex)))))

(define (unknown-handler feedback (file :: File) (buffer :: PrintStream))
  (feedback "Ignoring unknown file type: " (file:get-name)))
      
                      
(define (scan-directory feedback (buffer :: PrintStream))
  (let ((root :: File (File "/mnt/sdcard/DropPrint")))
    (if (not (root:isDirectory))
      (root:mkdir))
    (let ((files :: File[] (root:listFiles)))
      (let loop ((i 0))
        (cond ((< i files:length)
               (feedback "Discovered: " (files i))
               (let ((handler (file-handler (files i))))
                 (handler feedback (files i) buffer)
                 ((files i):delete))
               (loop (+ i 1)))
              (else
               #f))))))
