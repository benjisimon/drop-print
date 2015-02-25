;;
;; Our top level main app
;;
(require 'android-defs)
(require <com.benjisimon.dropprint.imports>)
(require <com.benjisimon.dropprint.device>)
(require <com.benjisimon.dropprint.files>)
(require <com.benjisimon.dropprint.util>)


(define (logi message)
  (Log:i "main.scm" message))

(define (handler callback)
  (object (Handler)
          ((handleMessage (message :: Message))
           (callback message)
           #!void)))

(activity main
  (on-create-view
   (<kawa.standard.Scheme>:registerEnvironment)
   (let ((log-area (android.widget.TextView (this) ))
         (*buffer* :: String "DropPrint started"))
     (define (feedback . words)
       (let ((buffer :: string ""))
         (for-each (lambda (w)
                     (set! buffer (string-append buffer (any->string w))))
                   words)
         (set! *buffer* (string-append buffer "\n" *buffer*))
         (log-area:post (runnable (lambda ()
                                    (log-area:set-text *buffer*))))))



     
     (feedback "Setting up printer handlers")
     (let* ((printer :: BluetoothSocket #!null)
            (buffer :: PrintStream #!null)
            (connected? #f)
            (tested? #f)
            (monitor :: Thread  (Thread (runnable (lambda ()
                                                    (let loop ()
                                                      (sleep 1)
                                                      (cond ((and (not (eq? printer #!null)) (not connected?))
                                                             (feedback "Trying to connect")
                                                             (printer:connect)
                                                             (set! connected? #t)
                                                             (let ((out :: OutputStream (printer:get-output-stream)))
                                                               (set! buffer (PrintStream out)))
                                                             (feedback "Connected!"))
                                                            ((and connected? (not tested?))
                                                             (feedback "Print welcome message")
                                                             (buffer:print (string-append "DropPrint is Ready\n\n"))
                                                             (set! tested? #t))
                                                            ((and connected? tested?)
                                                             (scan-directory feedback buffer)))
                                                      (loop)))))))
       (monitor:start)
       (let ((device :: BluetoothDevice (find-device feedback))
             (uuid :: java.util.UUID   (<java.util.UUID>:fromString "00001101-0000-1000-8000-00805F9B34FB")))
         (cond ((not (eq? device #!null))
                (feedback "Device address: " device)
                (feedback "Handing control to polling thread")
                (let* ((socket :: BluetoothSocket (device:createRfcommSocketToServiceRecord uuid)))
                  (set! printer socket)))
               (else
                (feedback "No device found. This isn't good.")))
         log-area)))))
