(ns dopeloop.audiograph
  (:require
    [dopeloop.main :refer [beats-to-seconds]]
    ["virtual-audio-graph" :as vag]
    ["virtual-audio-graph$default" :as createVirtualAudioGraph]))

(js/console.log "vag" vag)
(js/console.log createVirtualAudioGraph)

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
