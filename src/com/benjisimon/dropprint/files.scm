;;
;; Work with our files
;;
(require <com.benjisimon.dropprint.imports>)
(require <com.benjisimon.dropprint.images>)
(require <com.benjisimon.dropprint.qrcode>)
(require <com.benjisimon.dropprint.util>)

(define (file-handler (file :: File))
  (let ((name :: String ((file:get-name):to-lower-case)))
    (cond ((name:matches "^.*[.]txt$") text-handler)
          ((name:matches "^.*[.]scm$") scm-handler)
          ((name:matches "^.*[.]jpg$") image-handler)
          ((name:matches "^.*[.]png$") image-handler)
          ((name:matches "^.*[.]gif$") image-handler)
          ((name:matches "^.*[.]qrc$") qrcode-handler)
          ((name:matches "^.*[.]qrcode$") qrcode-handler)
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

(define (qrcode-handler feedback (file :: File) (buffer :: PrintStream))
  (let ((contents (file->string file)))
    (qrcode-write feedback contents buffer)))

(define (image-handler feedback (file :: File) (buffer :: PrintStream) )
  (let ((bitmap :: Bitmap (BitmapFactory:decodeFile (file:get-absolute-path))))
    (image-write feedback bitmap buffer)))

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
                 (buffer:print "\n-------\n")
                 ((files i):delete))
               (loop (+ i 1)))
              (else
               #f))))))
