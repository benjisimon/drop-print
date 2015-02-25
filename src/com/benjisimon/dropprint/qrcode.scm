;;
;; The qrcode is responsible for rendering QR codes
;;
(require <com.benjisimon.dropprint.imports>)
(require <com.benjisimon.dropprint.util>)
(require <com.benjisimon.dropprint.images>)

(define (qrcode-write feedback text (buffer :: PrintStream))
  (let* ((writer :: MultiFormatWriter (MultiFormatWriter))
         (result :: BitMatrix (writer:encode text BarcodeFormat:QR_CODE 384 384))
         (w (result:get-width))
         (h (result:get-height))
         (pixels (int[] length: (* w h)))
         (bitmap :: Bitmap (Bitmap:createBitmap w h Bitmap:Config:ARGB_8888)))
    (feedback "Constructed " w "x" h " QRCode from [" text "]")
    (for-each-coord w h
                    (lambda (x y)
                      (set! (pixels (+ (* y w) x)) (if (result:get x y) Color:BLACK Color:WHITE))))
    (bitmap:set-pixels pixels 0 w 0 0 w h)
    (image-write feedback bitmap #f buffer)))
  
  
