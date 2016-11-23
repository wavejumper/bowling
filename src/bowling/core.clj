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
  ;; Has to equal 10 on first ball!
  (s/tuple #{10} #{0}))

(s/def ::frame
  ;; A frame can either be a ball, spare or a strike
  (s/or :ball   ::ball
        :spare  ::spare
        :strike ::strike))

(s/def ::final-frame
  (s/or :ball          ::ball
        :spare         (s/tuple ::spare (s/or :ball ::ball :strike ::strike))
        :single-strike (s/tuple ::strike ::ball)
        :double-strike (s/tuple ::strike ::strike (s/or :ball ::ball :strike ::strike))))

(s/def ::frames
  (s/and (s/cat :frame       (s/* ::ball)
                :final-frame (s/? ::final-frame)) ;; The final frame is a special case (bonuses)

         ;; There is a maximum of 10 frames to a scorecard
         #(<= (count %) 10)))

(s/def ::score
  (s/and number? #(<= % 300)))

(s/def ::scorecard
  (s/keys :req-un [::score ::frames]))

;; -- api -----------------------------------------------------------------------------------------

(defn scorecard
  "Returns an empty score card"
  []
  {:score 0 :frames []})

(defn- score-strike [score frame]
  (+ score (* 2 (apply + frame))))

(defn- score-spare [score frame]
  (+ score (apply + frame) (first frame)))

(defn- score-ball [score frame]
  (+ score (apply + frame)))

(declare score-frame)

;; Handle bonuses, etc
(defn- score-final-frame [score prev-frame curr-frame]
  (condp = (first (s/conform ::final-frame curr-frame))
    :ball
    ;; No bonuses given, simply use `score-ball` logic
    (score-ball score curr-frame)

    :double-strike
    ;; Double strike = 3 balls
    (let [[strike1 strike2 final-ball] curr-frame]
      (as-> (score-frame {:score score :frames [prev-frame]} strike1) $
            (score-frame {:score (:score $) :frames [strike1]} strike2)
            (score-frame {:score (:score $) :frames [strike2]} final-ball)
            (:score $)))

    ;; else, either :spare or :single-strike which means two balls...
    ;; simply re-use `score-frame` logic
    (let [[ball1 ball2] curr-frame]
      (as-> (score-frame {:score score :frames [prev-frame]} ball1) $
            (score-frame {:score (:score $) :frames [ball1]} ball2)
            (:score $)))))

(s/fdef score-final-frame
  :args (s/cat :score      ::score
               :prev-frame ::frame
               :curr-frame ::final-frame)
  :ret ::score)

;; I'm going to deviate a little and return the next state of the scorecard (with a :score key) instead
;; of returning just the score
(defn score-frame
  "Given a score card, score a frame"
  [scorecard frame]
  (let [prev-frame (-> scorecard :frames last)
        next-card  (update scorecard :frames conj frame)]
    (cond
      ;; Handle special case of final frame
      (= 9 (count (:frames scorecard)))
      (update next-card :score #(score-final-frame % prev-frame frame))

      (s/valid? ::strike prev-frame)
      (update next-card :score #(score-strike % frame))

      (s/valid? ::spare  prev-frame)
      (update next-card :score #(score-spare % frame))

      (s/valid? ::ball prev-frame)
      (update next-card :score #(score-ball % frame))

      :else (throw (IllegalArgumentException. "Invalid frame")))))

(s/fdef score-frame
  :args (s/cat :scorecard ::scorecard
               :frame     (s/or :frame ::frame :final-frame ::final-frame))
  :ret  ::scorecard)

(defn score-game
  "Determines if a game is complete - if so, provide the final score"
  [scorecard]
  (if (= 10 (count scorecard))
    (:score scorecard)
    (throw (IllegalStateException. "Game incomplete"))))

(s/fdef score-game
  :args (s/cat :scorecard ::scorecard)
  :ret  ::score)