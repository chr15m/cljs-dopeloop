(ns dopeloop.render
  (:require
    ["virtual-audio-graph" :refer [bufferSource createVirtualAudioGraph]]))

; a clip is a series of notes and definitions for the instruments that make the sounds for those notes

(def clip-definition
  {:tempo 120 ; BPM
   :notes [{:instrument "...id..."
            ;:note "F#3" ; optional for one-shot samples
            :beat 0 ; beat number to start on
            :length nil ; cut sound after this many beats
            }]
   :instruments [{:id "..."
                  ;:kind :oneshot
                  ;:role :bassdrum ; for outputting midi drums
                  :buffer "...array..." ; if this is a sample based instrument
                  }]})

(defn beats-to-seconds [bpm beat]
  (* (/ (/ 60 bpm) 2) beat))

(defn lookup-sample
  "Look up a sample from a sample based instrument.
  Returns the buffer containing that sample."
  [note clip]
  (let [instrument-id (:instrument note)
        instrument (->> (:instruments clip)
                        (filter #(= (:id %) instrument-id))
                        first)
        buffer (:buffer instrument)]
    buffer))

(defn render-clip-to-audiograph
  "Create the virtual-audio-graph nodes required to render a clip."
  [clip]
  (->> clip
       :notes
       (map-indexed
         (fn [idx note]
           [idx (bufferSource #js {:buffer (lookup-sample note clip)
                                   :playbackRate 1 ; TODO: playback at other pitches
                                   :startTime (beats-to-seconds (:tempo clip) (:beat note))
                                   #_#_ :stopTime 0})]))
       (into {})))

(defn render-audio-graph-to-buffer
  "Render a virtual-audio-graph to a buffer.
  Takes an array of virtual-audio-graph nodes, the bpm, and number of beats.
  Returns a rendered buffer via promise."
  [audio-graph bpm beats]
  (let [ctx (js/OfflineAudioContext 2 (beats-to-seconds bpm beats) 44100)
        graph (createVirtualAudioGraph #js {:context ctx})]
    (.update graph (clj->js audio-graph))
    (.startRendering ctx)))
