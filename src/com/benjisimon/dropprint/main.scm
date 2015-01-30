;;
;; Our top level main app
;;
(require 'android-defs)

(activity main
  (on-create-view
   (let ((log-area (android.widget.TextView 
                    (this)
                    text: "Hello from DropPrint")))
     (define (feedback . words)
       (let ((buffer :: string (log-area:get-text)))
         (set! buffer (string-append buffer "\n"))
         (for-each (lambda (w)
                     (set! buffer (string-append buffer w)))
                   words)
         (log-area:set-text buffer)))
     (feedback "xyz")
     log-area)))
                   
                   
