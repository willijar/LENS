; ****************************************************************************
; *  Copyright: National ICT Australia,  2009 - 2010                         *
; *  Developed at the ATP lab, Networked Systems research theme              *
; *  Author(s): Athanassios Boulis, Yuriy Tselishchev                        *
; *  This file is distributed under the terms in the attached LICENSE file.  *
; *  If you do not find this file, copies can be found by writing to:        *
; *                                                                          *
; *      NICTA, Locked Bag 9013, Alexandria, NSW 1435, Australia             *
; *      Attention:  License Inquiry.                                        *
; *                                                                          *
; ***************************************************************************/

((rx-modes
  #s(rx-mode
     :name normal
     :data-rate 250e3
     :modulation PSK
     :bits-per-symbol 4
     :bandwidth 20e6
     :noise-bandwidth 194e6
     :noise-floor -100.0
     :sensitivity -95.0
     :power-consumed 62e-3)
   #s(rx-mode
     :name ideal
     :data-rate 250e3
     :modulation ideal
     :bits-per-symbol 4
     :bandwidth 20e6
     :noise-bandwidth 194e6
     :noise-floor -100.0
     :sensitivity -95.0
     :power-consumed 62e-3))
 (tx-levels
  #s(tx-level :output-power   0.0 :power-consumed 57.42d-3) ;; dBm, and Watts
  #s(tx-level :output-power  -1.0 :power-consumed 55.18d-3) ;; dBm, and Watts
  #s(tx-level :output-power  -3.0 :power-consumed 50.69d-3) ;; dBm, and Watts
  #s(tx-level :output-power  -5.0 :power-consumed 46.2d-3) ;; dBm, and Watts
  #s(tx-level :output-power  -7.0 :power-consumed 42.24d-3) ;; dBm, and Watts
  #s(tx-level :output-power -10.0 :power-consumed 36.3d-3) ;; dBm, and Watts
  #s(tx-level :output-power -15.0 :power-consumed 32.67d-3) ;; dBm, and Watts
  #s(tx-level :output-power -25.0 :power-consumed 29.04d-3)) ;; dBm, and Watts
 (sleep-levels
  #s(sleep-level :name idle :power 1.4e-3))
 (transitions
  rx
   (rx nil
    tx #s(transition-element :delay 0.01d0 :power 62e-3)
    sleep #s(transition-element :delay 0.194d0 :power 62e-3))
  tx
   (rx #s(transition-element :delay 0.01d-3 :power 62e-3)
    tx nil
    sleep #s(transition-element :delay 0.194d-3 :power 62e-3))
  sleep
   (rx #s(transition-element :delay 0.05d-3 :power 1.4e-3)
    tx #s(transition-element :delay 0.05d-3 :power 1.4e-3)
    sleep nil)))
