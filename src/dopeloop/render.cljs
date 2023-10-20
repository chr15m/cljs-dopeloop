(ns dopeloop.render
  (:require
    [dopeloop.main :refer [beats-to-seconds]]
    ["virtual-audio-graph" :refer [bufferSource]]
    ["virtual-audio-graph$default" :as createVirtualAudioGraph]
    ["wav-encoder" :as wav-encoder]))

; a clip is a series of notes and definitions for the
; instruments that make the sounds for those notes

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

(defn instrument-from-sample [id buffer]
  {:id id
   :buffer buffer})

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
           [idx (bufferSource
                  "output"
                  #js {:buffer (lookup-sample note clip)
                       :playbackRate 1 ; TODO: pitched playback
                       :startTime (beats-to-seconds (:tempo clip)
                                                    (:beat note))
                       #_#_ :stopTime 0})]))
       (into {})))

(defn render-audio-graph-to-buffer
  "Render a virtual-audio-graph to a buffer.
  Takes an array of virtual-audio-graph nodes, and either:
  * the number of seconds to render, or;
  * the bpm, and number of beats to render.
  Returns a rendered buffer via promise."
  ([audio-graph seconds]
   (let [ctx (js/OfflineAudioContext.
               2 (* seconds 44100) 44100)
         graph (createVirtualAudioGraph #js {:audioContext ctx})]
     (.update graph (clj->js audio-graph))
     (.startRendering ctx)))
  ([audio-graph bpm beats]
   (render-audio-graph-to-buffer audio-graph (beats-to-seconds bpm beats))))

(defn render-audio-buffer-to-wav
  "Create a File object in wav format from the passed in audio buffer."
  [audio-buffer file-name]
  (let [wave-structure (clj->js {:sampleRate (aget audio-buffer "sampleRate")
                                 :channelData (.map (to-array (range (aget audio-buffer "numberOfChannels")))
                                                    #(.getChannelData audio-buffer %))})]
    (-> (.encode wav-encoder wave-structure)
        (.then (fn [data] (js/File. (js/Array. data) (str file-name ".wav") #js {:type "audio/wave"}))))))
