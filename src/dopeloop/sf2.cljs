(ns dopeloop.sf2
  (:require
    [applied-science.js-interop :as j]
    ["spessasynth_core" :as synth]))

(def
  gm-drum-map-defaults
  {"acoustic bass drum" {:rootNote 35}
   "bass drum 1" {:rootNote 36}
   "kick" {:rootNote 36}
   "side stick" {:rootNote 37}
   "acoustic snare" {:rootNote 38}
   "snare" {:rootNote 38}
   "hand clap" {:rootNote 39}
   "clap" {:rootNote 39}
   "electric snare" {:rootNote 40}
   "low floor tom" {:rootNote 41}
   "closed hi-hat" {:rootNote 42 :exclusiveClass 1}
   "closedhat" {:rootNote 42 :exclusiveClass 1}
   "hat-closed" {:rootNote 42 :exclusiveClass 1}
   "high floor tom" {:rootNote 43}
   "pedal hi-hat" {:rootNote 44 :exclusiveClass 1}
   "low tom" {:rootNote 45}
   "open hi-hat" {:rootNote 46 :exclusiveClass 1}
   "openhat" {:rootNote 46 :exclusiveClass 1}
   "hat-open" {:rootNote 46 :exclusiveClass 1}
   "low-mid tom" {:rootNote 47}
   "hi-mid tom" {:rootNote 48}
   "crash cymbal 1" {:rootNote 49}
   "crash" {:rootNote 49}
   "high tom" {:rootNote 50}
   "ride cymbal 1" {:rootNote 51}
   "ride" {:rootNote 51}
   "chinese cymbal" {:rootNote 52}
   "ride bell" {:rootNote 53}
   "tambourine" {:rootNote 54}
   "splash cymbal" {:rootNote 55}
   "cowbell" {:rootNote 56}
   "crash cymbal 2" {:rootNote 57}
   "vibraslap" {:rootNote 58}
   "ride cymbal 2" {:rootNote 59}
   "hi bongo" {:rootNote 60}
   "low bongo" {:rootNote 61}
   "mute hi conga" {:rootNote 62}
   "open hi conga" {:rootNote 63}
   "low conga" {:rootNote 64}
   "high timbale" {:rootNote 65}
   "low timbale" {:rootNote 66}
   "high agogo" {:rootNote 67}
   "low agogo" {:rootNote 68}
   "cabasa" {:rootNote 69}
   "maracas" {:rootNote 70}
   "short whistle" {:rootNote 71}
   "long whistle" {:rootNote 72}
   "short guiro" {:rootNote 73}
   "long guiro" {:rootNote 74}
   "percussion" {:rootNote 75}
   "claves" {:rootNote 75}
   "hi wood block" {:rootNote 76}
   "low wood block" {:rootNote 77}
   "mute cuica" {:rootNote 78}
   "open cuica" {:rootNote 79}
   "mute triangle" {:rootNote 80 :exclusiveClass 2}
   "open triangle" {:rootNote 81 :exclusiveClass 2}})

(defn create-synth-sample [name root-note sample-rate audio-data]
  (doto (synth/EmptySample.)
    (j/assoc! "name" name)
    (j/assoc! "originalKey" root-note)
    (j/assoc! "sampleRate" sample-rate)
    (j/call :setAudioData audio-data sample-rate)))

(defn load-sf2-samples [samples]
  (->> samples
       (keep
         (fn [s]
           (let [buffer (:buffer s)
                 sample-name (:sample-name s)
                 channels (j/get buffer "numberOfChannels")
                 sample-rate (j/get buffer "sampleRate")
                 defaults (get gm-drum-map-defaults
                               (.toLowerCase (.trim sample-name)) {})
                 root-note (or (:rootNote defaults) 60)
                 exclusive-class (or (:exclusiveClass defaults) 0)
                 left-sample (create-synth-sample
                               sample-name
                               root-note sample-rate
                               (.getChannelData buffer 0))]
             (when (= channels 2)
               (let [right-sample (create-synth-sample
                                    (str sample-name "_R")
                                    root-note sample-rate
                                    (.getChannelData buffer 1))]
                 (j/call left-sample :setLinkedSample
                         right-sample (j/get synth/sampleTypes "leftSample"))))
             {:sample left-sample
              :root-note root-note
              :exclusive-class exclusive-class})))
       vec))

(defn create-zone! [instrument sample root-note exclusive-class pan]
  (let [zone (j/call instrument :createZone sample)]
    (j/assoc! (.-keyRange zone) "min" root-note)
    (j/assoc! (.-keyRange zone) "max" root-note)
    (j/call zone :setGenerator
            (j/get synth/generatorTypes "overridingRootKey") root-note)
    (when (> exclusive-class 0)
      (j/call zone :setGenerator
              (j/get synth/generatorTypes "exclusiveClass") exclusive-class))
    (when pan
      (j/call zone :setGenerator (j/get synth/generatorTypes "pan") pan))))

(defn create-instrument-zones! [instrument loaded-samples]
  (doseq [{:keys [sample root-note exclusive-class]} loaded-samples]
    (if-let [linked-sample (j/get sample "linkedSample")]
      (do
        (create-zone! instrument sample root-note exclusive-class -500)
        (create-zone! instrument linked-sample root-note exclusive-class 500))
      (create-zone! instrument sample root-note exclusive-class nil))))

(defn create-sound-bank [instrument-name]
  (-> (synth/BasicSoundBank.)
      (j/assoc-in! [:soundBankInfo :name] instrument-name)
      (j/assoc-in! [:soundBankInfo :engineer] "https://dopeloop.ai")))

(defn add-samples-to-sound-bank! [sound-bank samples]
  (let [loaded-samples (load-sf2-samples samples)
        samples-to-add (mapcat (fn [s]
                                 (let [sample (:sample s)]
                                   (cons sample
                                         (when-let [linked
                                                    (j/get sample
                                                           :linkedSample)]
                                           [linked]))))
                               loaded-samples)]
    (doseq [sample samples-to-add]
      (j/call-in sound-bank [:samples :push] sample))
    loaded-samples))

(defn create-sf2-instrument [loaded-samples]
  (let [instrument (doto (synth/BasicInstrument.)
                     (j/assoc! "name" "instrument"))]
    (j/call-in instrument [:globalZone :setGenerator]
               (j/get synth/generatorTypes "initialFilterFc") 13500)
    (create-instrument-zones! instrument loaded-samples)
    instrument))

(defn create-sf2-preset [instrument-name instrument loaded-samples]
  (let [root-notes (map :root-note loaded-samples)
        min-root (when (seq root-notes) (apply min root-notes))
        max-root (when (seq root-notes) (apply max root-notes))
        preset (doto (synth/BasicPreset.)
                 (j/assoc! "name" instrument-name)
                 (j/assoc! "bank" 0)
                 (j/assoc! "preset" 0)
                 (j/assoc! "zones" #js []))
        zone (j/call preset :createZone instrument)]
    (when (and min-root max-root)
      (j/assoc! (.-keyRange zone) "min" min-root)
      (j/assoc! (.-keyRange zone) "max" max-root))
    preset))

(defn render-beat-to-sf2-buffer [samples instrument-name]
  (let [sound-bank (create-sound-bank instrument-name)
        loaded-samples (add-samples-to-sound-bank! sound-bank samples)
        instrument (create-sf2-instrument loaded-samples)
        preset (create-sf2-preset instrument-name instrument loaded-samples)]
    (j/call-in sound-bank [:instruments :push] instrument)
    (j/call-in sound-bank [:presets :push] preset)
    (j/call sound-bank :writeSF2)))
