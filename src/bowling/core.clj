(ns bowling.core
  "Ten-pin bowling game scorecard.
  Build an API that implements a ten-pin bowling scorecard"
  (:require [clojure.spec :as s]))

;; -- spec ----------------------------------------------------------------------------------------

(s/def ::ball ;; (not a strike or a spare - poorly named definition...)
  (s/and (s/tuple number? number?)
         ;; Has to be less than 10!
         (s/and #(< (apply + %) 10))))

(s/def ::spare
  (s/and (s/tuple number? number?)
         ;; Has to equal 10
         #(= 10 (apply + %))
         ;; ...and can't be a strike!
         (fn [ball] (every? #(not= 0 %) ball))))

(s/def ::strike
  ;; Has to equal 10 on the first ball!
  (s/tuple #{10} #{0}))

(s/def ::frame
  ;; A frame can either be a ball, spare or a strike
  (s/or :ball   ::ball
        :spare  ::spare
        :strike ::strike))

(s/def ::final-frame
  (s/or :ball          (s/tuple ::ball)
        :spare         (s/tuple ::spare (s/or :ball ::ball :strike ::strike))
        :single-strike (s/tuple ::strike ::ball)
        :double-strike (s/tuple ::strike ::strike (s/or :ball ::ball :strike ::strike))))

(s/def ::frames
  (s/or :incomplete (s/coll-of ::frame :kind vector? :min-count 0 :max-count 9)
        :complete   (s/and (s/cat :frame (s/* ::frame)
                                  :final-frame (s/? ::final-frame))
                           #(= 10 (count %)))))

(s/def ::score
  (s/and number? #(<= % 300)))

(s/def ::scorecard
  (s/keys :req-un [::score ::frames]))

;; -- api -----------------------------------------------------------------------------------------

(defn scorecard
  "Returns an empty score card"
  []
  {:score 0 :frames []})

(defn- score-frames [frames]
  (loop [frames frames score  0]
    (let [[frame [ball1 ball2] [ball3 _] & _] frames
          ball1 (or ball1 0)
          ball2 (or ball2 0)
          ball3 (or ball3 0)]
      (cond
        ;; Finished iterating through frames, return next score
        (empty? frames)
        score

        ;; Handle 'strike' condition
        (s/valid? ::strike frame)
        (recur (rest frames)
               (if (= 10 ball1)
                 (+ score 10 10 ball3)
                 (+ score 10 ball1 ball2)))

        ;; Handle 'spare' condition
        (s/valid? ::spare frame)
        (recur (rest frames) (apply + score ball1 frame))

        ;; Else, simply tally the frame
        :else (recur (rest frames) (apply + score frame))))))

;; I'm going to deviate a little and return the next state of the scorecard (with a :score key) instead
;; of returning just the score
(defn score-frame
  "Given a scorecard and the next frame return the next scoreboard"
  [{:keys [frames] :as scorecard} frame]
  {:pre [(s/valid? ::scorecard scorecard)
         (or (s/valid? ::frame frame) (s/valid? ::final-frame frame))]}

  (if (s/valid? ::final-frame frame)
    (let [score (+ (score-frames (conj frames (first frame)))
                   (score-frames (rest frame)))]
      {:score score :frames (conj frames frame)})

    (let [frames (conj frames frame)]
      {:score (score-frames frames) :frames frames})))

(defn score-game
  "Determines if a game is complete - if so, provide the final score"
  [scorecard]
  {:pre [(s/valid? ::scorecard scorecard)]}
  (if (= 10 (-> scorecard :frames count))
    (:score scorecard)
    (throw (IllegalStateException. "Game incomplete"))))