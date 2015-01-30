;;
;; Our top level main app
;;
(require 'android-defs)

(define-alias Log android.util.Log)
(define-alias Handler android.os.Handler)
(define-alias Message android.os.Message)
(define-alias PrinterClassFactory com.zkc.helper.printer.PrinterClassFactory)
(define-alias PrinterClass com.zkc.helper.printer.PrinterClass)
(define-alias Thread java.lang.Thread)

(define (logi message)
  (Log:i "main.scm" message))

(define (handler callback)
  (object (Handler)
          ((handleMessage (message :: Message))
           (callback message)
           #!void)))

(define (status-string code)
  (case code
    ((0) "None")
    ((1) "Listening")
    ((2) "Connecting")
    ((3) "Connected")
    ((4) "Lost Connection")
    ((5) "Failed Connection")
    ((6) "Successful Connection")
    ((7) "Scanning")
    ((8) "Scanning Stopped")
    (else (string-append "Unknown code: " (code:to-string)))))


(activity main
  (on-create-view
   (<kawa.standard.Scheme>:registerEnvironment)
   (let ((log-area (android.widget.TextView 
                    (this)))
         (*buffer* :: String "Hello From DropPrint"))
     (define (feedback . words)
       (let ((buffer :: string ""))
         (for-each (lambda (w)
                     (set! buffer (string-append buffer (w:to-string))))
                   words)
         (set! *buffer* (string-append buffer "\n" *buffer*))
         (log-area:post (runnable (lambda ()
                                    (log-area:set-text *buffer*))))))
     (define (find-device printer :: PrinterClass)
       (feedback "Searching for printer")
       (let ((devices (printer:get-device-list)))
         (feedback "Found " (devices:size) " possible devices")
         (let loop ((i 0))
           (cond ((< i (devices:size))
                  (let ((d (devices:get i)))
                    (feedback "  Considering: " d:deviceName)
                    (if (equal? d:deviceName "DL58")
                      d:deviceAddress
                      (loop (+ i 1)))))
                 (else #f)))))

     (feedback "Setting up printer handlers")
     (logi "Getting ready")
     (let* ((message-handler (handler
                              (lambda (msg)
                                (feedback "Got message message"))))
            (device-handler (handler
                             (lambda (msg)
                               (feedback "Got device message"))))
            (printer (PrinterClassFactory:create (this)
                                                message-handler device-handler))
            (current-printer-state -1)
            (test-sent #f)
            (monitor ::Thread  (Thread (runnable (lambda ()
                                                   (let loop ()
                                                     (let ((state (printer:get-state)))
                                                       (if (not (= current-printer-state state))
                                                         (begin
                                                           (set! current-printer-state state)
                                                           (feedback "Printer State = " (status-string state))))
                                                       (if (and (= state PrinterClass:STATE_LISTEN) (not test-sent))
                                                         (begin
                                                           (set! test-sent #t)
                                                           (feedback "Sending a test print!")
                                                           (printer:printText "Hello World\r\n")))
                                                       (sleep 1)
                                                       (loop))))))))
       (monitor:start)
       (let ((device (find-device printer)))
         (cond (device
                (printer:open (this))
                (feedback "Trying to connect to: " device)
                (printer:connect device))
               (else
                (feedback "No device found. This isn't good."))))
       log-area))))
