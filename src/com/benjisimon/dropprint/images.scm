;;
;; Work with images that are going to get printed
;;
(require <com.benjisimon.dropprint.imports>)
(require <com.benjisimon.dropprint.util>)

(define (logi . message)
  (Log:i "images.scm" 
         (apply string-append
                (map (lambda (w)
                       (w:to-string))
                     message))))


(define (scale-bitmap feedback (src :: Bitmap) max-width) :: Bitmap
  (let* ((src-w (src:get-width))
         (src-h (src:get-height))
         (matrix :: Matrix (Matrix)))
    (feedback "Scaling: " src-w "x" src-h " to fit in max-width=" max-width)
    (let ((action (cond ((<= src-w max-width)
                         'skip)
                        ((<= src-h max-width)
                         (matrix:postRotate 90)
                         '(rotate 90))
                        ((> src-w src-h)
                         (matrix:postRotate 90)
                         (matrix:postScale (/ max-width src-h)
                                           (/ max-width src-h))
                         `(rotate 90 scale ,(/ max-width src-h)))
                        (else
                         (matrix:postScale (/ max-width src-w)
                                           (/ max-width src-w))
                         `(scale ,(/ max-width src-w))))))
      (feedback "scale action: " action)
      (Bitmap:createBitmap src 0 0 src-w src-h matrix #t))))

(define (make-image-buffer feedback (stream :: PrintStream))
  (let ((bit-index :: int 0)
        (accumulator :: int 0))
    (lambda (value)
      (cond ((eq? value 'flush)
             (if (> 0 bit-index)
               (begin
                 (stream:write accumulator)
                 (stream:flush)))
             (set! bit-index 0)
             (set! accumulator 0))
            (else
             (let ((bit :: int value))
               (set! accumulator
                     (bitwise-ior (bitwise-arithmetic-shift-left accumulator 1)
                                  bit))
               (set! bit-index (+ 1 bit-index))
               (if (= bit-index 8)
                   (begin
                     (stream:write accumulator)
                     (set! bit-index 0)
                     (set! accumulator 0)))))))))
                      
            

(define (image-write feedback (full :: Bitmap) (stream :: PrintStream))
  (let* ((scaled (scale-bitmap feedback full 384))
         (stream-buffer (make-image-buffer feedback stream)))
    (let* ((img-w (scaled:get-width))
           (h (scaled:get-height))
           (bytes-per-row (int-val (ceiling (/ img-w 8))))
           (bit-w (* bytes-per-row 8))
           (row-header (bytevector #x1F #x10 bytes-per-row #x00))
           (snapshot :: PrintStream (PrintStream "/sdcard/dp.img"))
           (snapshot-buffer (make-image-buffer feedback snapshot)))                   
      (feedback "Writing " img-w "x" h " image (" bit-w " bits wide)") 
      (let loop ((x 0) (y 0))
        (cond ((= y h) 'done)
              (else
               (if (= x 0)
                 (begin
                   (stream-buffer 'flush)
                   (write-bytevector row-header stream)
                   (snapshot-buffer 'flush)
                   (write-bytevector row-header snapshot)))
               (let* ((bit (if (>= x img-w)
                             0
                             (rgb->bit (scaled:get-pixel x y)))))
                 (stream-buffer bit)
                 (snapshot-buffer bit)
                 (cond ((= (inc x) bit-w)
                        (loop 0 (inc y)))
                       (else
                        (loop (inc x) y)))))))
      (stream-buffer 'flush)
      (snapshot-buffer 'flush))))

;;
;; From: http://www.had2know.com/technology/rgb-to-gray-scale-converter.html
;;
(define (rgb->bit pixel)
  (let* ((red (Color:red pixel))
         (green (Color:green pixel))
         (blue (Color:blue pixel))
         (gray (+ (* 0.299 red) (* 0.587 green) (* 0.114 blue))))
    (cond ((> gray 128) 0)
          (else 1))))
