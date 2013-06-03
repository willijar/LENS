(in-package :lens.wsn)

(deftype radio-control-command()
  (member set-state set-mode set-tx-output set-sleep-level set-carrier-freq
          set-cca-threshold set-cs-interrupt-on set-cs-interrupt-off
          set-encoding))

(deftype radio-state () (member rx tx sleep))

(defclass radio(comms-module)
  ((radio-parameters-file
    :parameter t :type string :reader radio-parameters-file
    :documentation "the file that cointains most radio parameters")
   (mode :parameter t :initform nil :reader mode
         :documentation "we can choose a mode to begin with. Modes are
         defined in the RadioParametersFile. Empty string means use
         the first mode defined)")
   (state :type symbol :parameter t :initform 'rx :accessor state
          :documentation "we can choose a radio state to begin
          with. RX and TX are always there. according to the radio
          defined we can choose from a different set of sleep states")
   (tx-output-power
    :type symbol :parameter t :initform nil :accessor tx-output-power
    :documentation "we can choose a Txpower to begin with. Possible tx
    power values are defined in the RadioParametersFile. Empty string
    means use the first tx power defined (which is also the highest)")
   (sleep-level
    :type symbol :parameter t :initform nil :accessor sleep-level
    :documentation "we can choose a sleep level which will be used when a transition to SLEEP state is requested. Empty string means use first level defined (will usually be the fastest and most energy consuming sleep state)")
   (carrier-frequency
    :type double :parameter t :initform 2.4E9 :accessor carrier-frequency
    :documentation "the carrier frequency (in Hz) to begin with.")
   (collision-model
    :type symbol :parameter t :initform :additive :reader collision-model
    :documentation "none, simple, additive or advance interference")
   (cca-threshold
    :type real :parameter t :initform -95 :reader cca-threshold
    :documentation "the threshold of the RSSI register (in dBm) were above it channel is NOT clear")
   (symbols-for-rssi
    :type integer :parameter t :initform 8 :reader symbols-for-rssi)
   (carrier-sense-interrupt-enabled
    :type boolean :parameter t :initform nil
    :reader carrier-sense-interrupt-enabled)
   (max-phy-frame-size
    :initform 1024
    :type integer :parameter t :initform nil :reader max-phy-frame-size
    :properties (:units "B")
    :documentation "in bytes")
   (header-overhead :initform 6 :documentation "in bytes - 802.15.4=6bytes")
   (avg-busy-frame
    :type real :initform 1 :parameter t :reader avg-busy-frame
    :documentation "integration time for measuring avg busy time")
   (buffer-size :initform 16))
  (:gates
   (mac :inout)
   (wireless-channel :inout))
  (:metaclass module-metaclass)