(ns dopeloop.data
  (:require
    [cognitect.transit :as t]
    [applied-science.js-interop :as j]))

(def fx-vol-slides
  [[:unchanged ["Unchanged" 1 "0"]]
   [:x2o3 ["× ⅔" (/ 2 3.0) "6"]]
   [:x1o2 ["× ½" 0.5 "7"]]
   [:-1o4 ["- 4th" (/ 1 4) "5"]]
   [:+1o8 ["+ 8th" (/ 1 8) "C"]]
   [:+1o16 ["+ 16th" (/ 1 16) "B"]]
   [:x1.5 ["× 1.5" 1.5 "E"]]
   [:x2  ["× 2" 2 "F"]]])

; serialize and deserialize transit

(deftype AudioBufferHandler []
  Object
  (tag [_this _v] "audio-buffer")
  (rep [_this v]
    (let [fields [:sampleRate :length :numberOfChannels]
          buffers (map #(-> (.getChannelData v %)
                            (aget "buffer")
                            (js/Uint8Array.))
                       (range (aget v "numberOfChannels")))]
      (j/assoc! (j/select-keys v fields) :buffers buffers)))
  (stringRep [_this _v] nil))

(defn deserialize-audio-buffer [v]
  (let [buffers (get v "buffers")
        options (clj->js (dissoc v "buffers"))
        audio-buffer (js/AudioBuffer. options)]
    (doseq [i (range (get v "numberOfChannels"))]
      (let [buff (-> buffers (nth i) (aget "buffer"))
            f32 (js/Float32Array. buff)]
        (.copyToChannel audio-buffer f32 i)))
    audio-buffer))

(def w
  (t/writer :json
            {:handlers {js/AudioBuffer (AudioBufferHandler.)}}))

(def r
  (t/reader :json
            {:handlers {"audio-buffer" deserialize-audio-buffer}}))

(defn dopeloop-serialize [structure]
  (t/write w structure))

(defn dopeloop-deserialize [transit]
  (t/read r transit))

