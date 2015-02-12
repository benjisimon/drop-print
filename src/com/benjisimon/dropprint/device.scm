;;
;; A module for working with devices
;;
(require <com.benjisimon.dropprint.imports>)

(define (find-device feedback)
  (feedback "Searching for printer")
  (let* ((adapter :: BluetoothAdapter (<android.bluetooth.BluetoothAdapter>:get-default-adapter))
         (devices :: Set (adapter:get-bonded-devices)))
    (feedback "Found " (devices:size) " possible devices")
    (let loop ((iter :: Iterator (devices:iterator)))
      (if (iter:has-next)
        (let ((item :: BluetoothDevice (iter:next)))
          (cond ((equal? (item:getName) "DL58")
                 (feedback "Found it: " (item:getName))
                 item)
                (else
                 (feedback "Skipping: " (item:getName))
                 (loop iter))))
        #!null))))
