(ns dopeloop.main)

(def audio-context
  (try
    (or (aget js/window "AudioContext")
        (aget js/window "webkitAudioContext"))
    (catch :default _e nil)))

(defn beats-to-seconds [bpm beat]
  (* (/ 60 bpm 4) beat))

(defn get-peak-volume
  "Find the peak sample height in an array of audio data."
  [channel-data-array]
  (let [loop-source-array-copy (js/Float32Array.from channel-data-array)]
    (.sort loop-source-array-copy) ; sort to find the max sample
    (let [largest (aget loop-source-array-copy (dec (aget loop-source-array-copy "length")))
          smallest (first loop-source-array-copy)]
      (max largest (js/Math.abs smallest)))))

(defn normalize-channel-data
  "Normalize all channels so peaks are at max volume."
  [audio-buffer]
  (let [channels (map #(.getChannelData audio-buffer %)
                      (range (aget audio-buffer "numberOfChannels")))
        peaks (map #(get-peak-volume %) channels)
        highest-peak (apply js/Math.max peaks)]
    (doseq [c channels]
      (.map c (fn [v i a] (aset a i (/ v highest-peak)))))
    audio-buffer))

(defn arrays-to-audio-buffer
  "Convert a set of arrays to a multi-channel AudioBuffer. Defaults to audio context sample rate."
  [ctx arrays & [sample-rate]]
  (let [channel-count (count arrays)
        audio-buffer (.createBuffer ctx channel-count (aget (first arrays) "length") (or sample-rate (aget ctx "sampleRate")))]
    (doseq [b (range channel-count)]
      (let [array-buffer (.getChannelData audio-buffer b)]
        (.copyToChannel array-buffer (nth arrays b) b)))))

(defn make-audio-buffer
  "Create an empty audio buffer with length based on bpm and beat count."
  [audio-context bpm beats]
  (let [beat-seconds (/ 60 bpm)
        sample-rate (aget audio-context "sampleRate")
        frames-per-beat (int (* beat-seconds sample-rate))
        frame-count (* beats frames-per-beat)]
    (.createBuffer audio-context 2 frame-count sample-rate)))

(defn fetch-sample
  "Fetch a remote sample and return the buffer containing it."
  [url & [actx]]
  (let [ctx (or actx (audio-context.))]
  (-> (js/fetch url)
      (.then #(.arrayBuffer %))
      (.then #(.decodeAudioData ctx %)))))

; *** seamless looping of audio buffers *** ;

(defn loop-audio-buffer!
  "Starts an AudioBuffer looping from the position `start-at`."
  [ctx audio-buffer & [start-at audio-margin]]
  (let [src (.createBufferSource ctx)
        start-at (or start-at 0)
        start-when (+ (aget ctx "currentTime") audio-margin)]
    (aset src "buffer" audio-buffer)
    (.connect src (aget ctx "destination"))
    (aset src "loop" true)
    (aset src "t_start" (- start-when start-at))
    (.start src start-when start-at)
    src))

(defn stop-source!
  "Stops an AudioBufferSourceNode and disconnects it from the audio graph."
  [src & [stop-at]]
  (when src
    (.stop src (or stop-at 0))
    (js/setTimeout #(.disconnect src) (inc (* (or stop-at 0) 1000))))
  nil)

(defn compute-seamless-loop-offset
  "Compute the offset to start a new loop from so that it matches the old loop.
  The two loops can have different counts of beats in them and be different BPMs.
  Args are the same as `seamless-loop-audio-buffer!`."
  [ctx audio-buffer playing-source old-beats-count new-beats-count & [audio-margin]]
  (when playing-source
    (let [new-duration (aget audio-buffer "duration")
          old-duration (aget playing-source "buffer" "duration")
          offset (mod (- (+ (aget ctx "currentTime")
                            (or audio-margin 0))
                         (aget playing-source "t_start")) old-duration)
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
  [ctx audio-buffer playing-source & [old-beats-count new-beats-count audio-margin-ms]]
  (let [audio-margin (-> audio-margin-ms (or 0) (/ 1000))
        offset (compute-seamless-loop-offset ctx audio-buffer playing-source (or old-beats-count 1) (or new-beats-count 1) audio-margin)
        src (loop-audio-buffer! ctx audio-buffer offset audio-margin)]
    (stop-source! playing-source (+ (aget ctx "currentTime") (or audio-margin 0)))
    src))

(defn get-loop-position
  "Get the current playing position of a loop in seconds.
  Also returns the duration in seconds so the percentage or slice can be calculated."
  [ctx playing-source]
  (let [duration (aget playing-source "buffer" "duration")]
    [(mod (- (aget ctx "currentTime") (aget playing-source "t_start")) duration) duration]))

; *** platform specific utils *** ;

(defn on-ios?
  "Returns true if the current platform is an iOS device."
  []
  (or (= (.indexOf (aget js/navigator "platform") "iP") 0)
      (and
        (-> js/navigator .-userAgent (.includes "Mac"))
        (not= (type (aget js/document "ontouchend")) "undefined"))))

(defn toggle-audio-context [context pause]
  (cond
    (and pause context (= (aget context "state") "running"))
    (.suspend context)
    (and (not pause) context (= (aget context "state") "suspended"))
    (js/setTimeout #(.resume context) 500)))

(defn manage-audio-context-ios
  "On iOS attach handlers to manage the AudioContext so it resumes
  correctly and prevents audio dropouts. `get-context-fn` is a function
  to get the audio context you want to manage."
  [get-context-fn]
  (when (on-ios?)
    (let [audio-context (get-context-fn)]
      (.addEventListener js/window "blur" #(toggle-audio-context audio-context true))
      (.addEventListener js/window "focus" #(toggle-audio-context audio-context false)))))

; *** cordova plugin wrappers *** ;

(defn on-device-ready
  "Cordova's deviceready event which signals APIs are ready to use."
  [callback]
  (.addEventListener js/document "deviceready" callback false))

; cordova plugin add https://github.com/chr15m/cordova-plugin-get-volume
; cordova plugin add https://github.com/MCluck90/cordova-plugin-android-volume

(defn get-volume
  "Get current media volume on Cordova native devices.
  The callback is never called on non-native platforms."
  [callback]
  (try (-> js/window .-cordova .-plugins .-VolumeControl (.getVolume callback)) (catch :default _e nil))
  (try (-> js/window .-androidVolume (.getMusic (fn [vol] (callback (/ vol 100.0))))) (catch :default _e nil)))

(defn poll-device-volume
  "Poll the device's native volume at `rate-ms` via Cordova plugins and run `callback`
  with the volume as a float between 0 and 1."
  [rate-ms callback]
  (js/setInterval
    #(get-volume callback)
    rate-ms))

; cordova plugin add cordova-plugin-screen-orientation

(defn lock-screen-orientation
  "Locks the screen orientation."
  [orientation]
  (try (-> js/window .-screen .-orientation (.lock orientation)) (catch :default _e nil)))

; cordova plugin add https://github.com/EddyVerbruggen/Insomnia-PhoneGap-Plugin.git

(defn wake-screen-lock
  "Keeps the screen awake. Pass true to keep awake and false to let it sleep again."
  [wake?] (try
            (if wake?
              (-> js/window .-plugins .-insomnia .keepAwake)
              (-> js/window .-plugins .-insomnia .allowSleepAgain))
            (catch :default _e nil)))

; *** higher level buffer play functions from PocketSync *** ;

(defn play! [state]
  (wake-screen-lock true)
  (swap! state update-in [:audio] assoc
         :playing true
         :context (audio-context.)))

(defn close-and-stop-audio! [{:keys [source context] :as audio}]
  (stop-source! source)
  (when context (.close context))
  audio)

(defn stop! [state]
  (wake-screen-lock false)
  (swap! state update-in [:audio]
         #(-> %
              close-and-stop-audio!
              (dissoc
               :updating
               :update-again
               :source
               :context
               :playing
               :position))))

(defn poll-playback-position! [state callback]
  (js/requestAnimationFrame
    (fn []
      (let [audio-context (-> @state :audio :context)
            audio-source (-> @state :audio :source)]
        (when (and audio-context audio-source)
          (callback (get-loop-position audio-context audio-source))))
      (poll-playback-position! state callback))))
