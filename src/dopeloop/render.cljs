(ns dopeloop.render
  (:require
    [dopeloop.main :refer [beats-to-seconds]]
    ["virtual-audio-graph" :refer [bufferSource gain] :as vag]
    ["virtual-audio-graph$default" :as createVirtualAudioGraph]
    ["wav-encoder" :as wav-encoder]))

; a clip is a series of notes and definitions for the
; instruments that make the sounds for those notes

(def clip-definition
  {:tempo 120 ; BPM
   :length 16
   :notes [{:instrument "...id..."
            ;:note "F#3" ; optional for one-shot samples
            :beat 0 ; beat number to start on
            :length nil ; cut sound after this many beats
            ;:channel ; optional channel id for .it export (references channels below)
            }]
   :instruments [{:id "..."
                  ;:kind :oneshot
                  ;:role :bassdrum ; for outputting midi drums
                  :buffer "...array..." ; if this is a sample based instrument
                  :volume 1
                  :mute false}]
   :channels [{:name "ch 1" ; optional for .it export
               :id 0 ; channel id reference for .it export
               }]})

(defn instrument-from-sample [id buffer]
  {:id id
   :buffer buffer})

(defn lookup-instrument
  "Look up a instrument from a clip note definition.
  Returns the instrument definition."
  [note clip]
  (let [instrument-id (:instrument note)
        instrument (->> (:instruments clip)
                        (filter #(= (:id %) instrument-id))
                        first)]
    instrument))

(defn index-of [col i]
  (first (keep-indexed #(when (= %2 i) %1) col)))

(defn render-clip-to-it-struct
  "Renders a clip to an impulse tracker datastructure for itwriter."
  [clip]
  (let [named-channels (->> clip
                            :notes
                            (map-indexed
                              (fn [_idx note]
                                (let [instrument (lookup-instrument note clip)
                                      instrument-index (index-of (:instruments clip) instrument)]
                                  {:channel (:id instrument)
                                   :data [(:beat note)
                                          {:note (or (:note note) "C-5")
                                           :instrument instrument-index
                                           :vol (->> (* (:volume instrument) (:volume note) 64)
                                                     (js/Math.floor)
                                                     (js/Math.min 64)
                                                     (str "v"))}]})))
                            (group-by :channel)
                            (map (fn [[channel channel-data]]
                                   [channel
                                    (->> channel-data
                                         (map :data)
                                         (into {}))]))
                            (into {}))
        channel-data (->> (map :id (:channels clip))
                          (map-indexed (fn [_idx k]
                                         (or
                                           (k named-channels)
                                           {}))))
        channel-names (map :name (:channels clip))]
    {:title (:name clip)
     :bpm (:tempo clip) 
     :mixvol 128
     :samples (map
                (fn [instrument]
                  (let [buffer (:buffer instrument)]
                    {:name (:sample-name instrument)
                     :buffer buffer}))
                (:instruments clip))
     :channelnames channel-names
     :patterns [{:rows (:length clip)
                 :channels channel-data}]
     :order [0]}))

(defn lookup-node-fn
  [node-fn]
  (if (fn? node-fn)
    node-fn
    (aget vag (name node-fn))))

(defn instantiate-audio-graph-nodes
  [audio-graph]
  (->> audio-graph
       (mapv (fn [[id node]]
               (if (vector? node)
                 [id (apply (lookup-node-fn (first node)) (clj->js (rest node)))]
                 [id node])))
       (into {})
       clj->js))

(defn render-clip-to-audiograph
  "Create the virtual-audio-graph nodes required to render a clip."
  [clip]
  (->> clip
       :notes
       (map-indexed
         (fn [idx note]
           (let [instrument (lookup-instrument note clip)]
             (when (not (:mute instrument))
               {(* idx 2)
                (gain "output" #js {:gain
                                    (* (:volume note)
                                       (:volume instrument))})
                (inc (* idx 2))
                (bufferSource
                  (* idx 2)
                  #js {:buffer (:buffer instrument)
                       :playbackRate 1 ; TODO: pitched playback
                       :startTime (beats-to-seconds (:tempo clip)
                                                    (:beat note))})}))))
       (into {})))

(defn play-audio-graph
  [audio-graph]
  (.update (createVirtualAudioGraph)
           (instantiate-audio-graph-nodes audio-graph)))

(defn stop-audio-graph
  [virtual-audio-graph]
  (.update virtual-audio-graph #js {}))

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
     (.update graph (instantiate-audio-graph-nodes audio-graph))
     (.startRendering ctx)))
  ([audio-graph bpm beats]
   (render-audio-graph-to-buffer audio-graph (beats-to-seconds bpm beats))))

(defn render-audio-buffer-to-wav
  "Create a File object in wav format from the passed in audio buffer."
  [audio-buffer file-name]
  (let [wave-structure
        (clj->js {:sampleRate (aget audio-buffer "sampleRate")
                  :channelData (.map (-> audio-buffer
                                         (aget "numberofChannels")
                                         range
                                         to-array)
                                     #(.getChannelData audio-buffer %))})]
    (-> (.encode wav-encoder wave-structure)
        (.then (fn [data]
                 (js/File. (js/Array. data)
                           (str file-name ".wav")
                           #js {:type "audio/wave"}))))))
