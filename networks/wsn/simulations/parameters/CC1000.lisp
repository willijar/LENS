;; CC1000 radio characteristics

((rx-modes
  #s(rx-mode
     :name normal
     :data-rate 19.2d3 ;;Hz
     :modulation FSK
     :bits-per-symbol 1
     :bandwidth 10d3 ;; Hz
     :noise-bandwidth 30d3 ;;Hz
     :noise-floor -105 ;; dBm
     :sensitivity -98 ;;dBm
     :power-consumed 22.2d-3) ;;W
  #s(rx-mode
     :name ideal
     :data-rate 19.2d3 ;;Hz
     :modulation ideal
     :bits-per-symbol 1
     :bandwidth 10d3 ;; Hz
     :noise-bandwidth 30d3 ;;Hz
     :noise-floor -105 ;; dBm
     :sensitivity -98 ;;dBm
     :power-consumed 22.2d-3)) ;;W
 (tx-levels
  #s(tx-level :output-power 10.0 :power-consumed 80.1d-3) ;; dBm, and Watts
  #s(tx-level :output-power  5.0 :power-consumed 44.5d-3) ;; dBm, and Watts
  #s(tx-level :output-power  0.0 :power-consumed 31.2d-3) ;; dBm, and Watts
  #s(tx-level :output-power -5.0 :power-consumed 26.7d-3) ;; dBm, and Watts
  #s(tx-level :output-power -10.0 :power-consumed 23.7d-3) ;; dBm, and Watts
  #s(tx-level :output-power -15.0 :power-consumed 22.2d-3) ;; dBm, and Watts
  #s(tx-level :output-power -20.0 :power-consumed 15.9d-3)) ;; dBm, and Watts
 (sleep-levels
  #s(sleep-level :name idle :power 0.0006e-3))
 (transitions
  (rx
   (rx nil)
   (tx #s(transition-element :delay 0.01 :power 22.2e-3))
   (sleep #s(transition-element :delay 0.2 :power 22.2e-3)))
  (tx
   (rx #s(transition-element :delay 0.01 :power 22.2e-3))
   (tx nil)
   (sleep #s(transition-element :delay 0.2 :power 22.2e-3)))
  (sleep
   (rx #s(transition-element :delay 0.05 :power 0.5e-3))
   (tx #s(transition-element :delay 0.05 :power 0.5e-3))
   (sleep nil))))
;; RX MODES
;; ;;Name, dataRate(kbps), modulationType, bitsPerSymbol, bandwidth(MHz), noiseBandwidth(MHz), noiseFloor(dBm), sensitivity(dBm), powerConsumed(mW)
;; normal, 19.2, FSK, 1, 10, 30, -105, -98, 22.2
;; IDEAL, 19.2, IDEAL, 1, 10, 30, -105, -98, 22.2

;; TX LEVELS
;; Tx_dBm 10 5 0 -5 -10 -15 -20
;; Tx_mW 80.1 44.4 31.2 26.7 23.7 22.2 15.9

;; DELAY TRANSITION MATRIX
;; ;;State switching times (time to switch from column state to row state, in msec)
;; ;;      RX      TX      SLEEP
;; RX      -       0.01    0.2
;; TX      0.01    -       0.2
;; SLEEP   0.05    0.05    -

;; POWER TRANSITION MATRIX
;; ;;      RX      TX      SLEEP
;; RX      -       22.2    22.2
;; TX      22.2     -      22.2
;; SLEEP   0.5     0.5     -

;; SLEEP LEVELS
;; idle 0.0006, -, -, -, -
