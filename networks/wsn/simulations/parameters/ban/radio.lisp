;; # ****************************************************************************
;; # *  Copyright: National ICT Australia,  2009 - 2010                         *
;; # *  Developed at the ATP lab, Networked Systems research theme              *
;; # *  Author(s): Athanassios Boulis, Yuriy Tselishchev                        *
;; # *  This file is distributed under the terms in the attached LICENSE file.  *
;; # *  If you do not find this file, copies can be found by writing to:        *
;; # *                                                                          *
;; # *      NICTA, Locked Bag 9013, Alexandria, NSW 1435, Australia             *
;; # *      Attention:  License Inquiry.                                        *
;; # *                                                                          *
;; # ***************************************************************************/

((rx-modes
  #s(rx-mode
     :name high
     :data-rate 1024000
     :modulation DIFFQPSK
     :bits-per-symbol 2
     :bandwidth 20e6
     :noise-bandwidth 1000e3
     :noise-floor -104.0
     :sensitivity -87.0
     :power-consumed 3.1e-3)
  #s(rx-mode
     :name low
     :data-rate 512000
     :modulation DIFFBPSK
     :bits-per-symbol 1
     :bandwidth 20e6
     :noise-bandwidth 1000e3
     :noise-floor -104.0
     :sensitivity -91.0
     :power-consumed 3.1e-3)
   #s(rx-mode
     :name ideal
     :data-rate 1024000
     :modulation ideal
     :bits-per-symbol 2
     :bandwidth 20e6
     :noise-bandwidth 1000e3
     :noise-floor -104.0
     :sensitivity -87.0
     :power-consumed 3.1e-3))
 (tx-levels
  #s(tx-level :output-power -10.0 :power-consumed 3.00e-3) ;; dBm, and Watts
  #s(tx-level :output-power -12.0 :power-consumed 2.96e-3) ;; dBm, and Watts
  #s(tx-level :output-power -15.0 :power-consumed 2.93e-3) ;; dBm, and Watts
  #s(tx-level :output-power -20.0 :power-consumed 2.90e-3) ;; dBm, and Watts
  #s(tx-level :output-power -25.0 :power-consumed 2.90e-3)) ;; dBm, and Watts
 (sleep-levels
  #s(sleep-level :name idle :power 0.05e-3))
 (transitions
  rx
   (rx nil
    tx #s(transition-element :delay 0.02d-3 :power 3e-3)
    sleep #s(transition-element :delay 0.194d-3 :power 3e-3))
  tx
   (rx #s(transition-element :delay 0.02d-3 :power 3e-3)
    tx nil
    sleep #s(transition-element :delay 0.194d-3 :power 3e-3))
  sleep
   (rx #s(transition-element :delay 0.05d-3 :power 1.5e-3)
    tx #s(transition-element :delay 0.05d-3 :power 1.5e-3)
    sleep nil)))
