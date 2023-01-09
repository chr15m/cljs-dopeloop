(ns dopeloop.main)

(def audio-context
  (or (aget js/window "AudioContext")
      (aget js/window "webkitAudioContext")))

(defn get-peak-volume
  "Find the peak sample height in an array of audio data."
  [channel-data-array]
  (let [loop-source-array-copy (js/Float32Array.from channel-data-array)]
    (.sort loop-source-array-copy) ; sort to find the max sample
    (let [largest (aget loop-source-array-copy (dec (aget loop-source-array-copy "length")))
          smallest (first loop-source-array-copy)]
      (max largest (js/Math.abs smallest)))))

(defn arrays-to-audio-buffer
  "Convert a set of arrays to a multi-channel AudioBuffer. Defaults to audio context sample rate."
  [ctx arrays & [sample-rate]]
  (let [channel-count (count arrays)
        audio-buffer (.createBuffer ctx channel-count (aget (first arrays) "length") (or sample-rate (aget ctx "sampleRate")))]
    (doseq [b (range channel-count)]
      (let [array-buffer (.getChannelData audio-buffer b)]
        (.copyToChannel array-buffer (nth arrays b) b)))))

; *** iOS Safari detection *** ;

(defn on-ios?
  "Returns true if the current platform is an iOS device."
  []
  (or (= (.indexOf (aget js/navigator "platform") "iP") 0)
      (and
        (-> js/navigator .-userAgent (.includes "Mac"))
        (not= (type (aget js/document "ontouchend")) "undefined"))))

; *** seamless looping of audio buffers *** ;

(defn loop-audio-buffer!
  "Starts an AudioBuffer looping from the position `start-at`."
  [ctx audio-buffer & [start-at]]
  (let [src (.createBufferSource ctx)
        start-at (or start-at 0)
        start-when (+ (aget ctx "currentTime") 0.00)]
    (aset src "buffer" audio-buffer)
    (.connect src (aget ctx "destination"))
    (aset src "loop" true)
    (aset src "t_start" (- start-when start-at))
    (.start src start-when start-at)
    src))

(defn stop-source!
  "Stops an AudioBufferSourceNode and disconnects it from the audio graph."
  [src]
  (when src
    (.disconnect src)
    (.stop src 0))
  nil)

(defn compute-seamless-loop-offset
  "Compute the offset to start a new loop from so that it matches the old loop.
  The two loops can have different counts of beats in them and be different BPMs.
  Args are the same as `seamless-loop-audio-buffer!`."
  [ctx audio-buffer playing-source old-beats-count new-beats-count]
  (when playing-source
    (let [new-duration (aget audio-buffer "duration")
          old-duration (aget playing-source "buffer" "duration")
          offset (mod (- (aget ctx "currentTime") (aget playing-source "t_start")) old-duration)
          offset-in-old-slices (/ offset (/ old-duration old-beats-count))
          offset-in-new-slices (mod offset-in-old-slices new-beats-count)
          offset-rejigged (* offset-in-new-slices (/ new-duration new-beats-count))]
      offset-rejigged)))

(defn seamless-loop-audio-buffer!
  "Replaces a playing audio buffer source loop with a new audio source looped at the same position,
  accounting for different numbers of beats in each loop.

  * `audio-buffer` is an AudioBuffer with channels filled with sound data.
  * `playing-source` is the already playing AudioBufferSourceNode which will be ignored if nil.
  * `old-beats-count` is optionally the number of beats in the playing loop, defaults to 1.
  * `new-beats-count` is optionally the number of beats in the new loop, defaults to 1."
  [ctx audio-buffer playing-source & [old-beats-count new-beats-count]]
  (let [offset (compute-seamless-loop-offset ctx audio-buffer playing-source (or old-beats-count 1) (or new-beats-count 1))
        src (loop-audio-buffer! ctx audio-buffer offset)]
    (when playing-source
      (stop-source! playing-source))
    src))
