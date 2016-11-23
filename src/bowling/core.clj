(ns bowling.core
  "Ten-pin bowling game scorecard.

  Build an API that implements a ten-pin bowling scorecard"
  (:require [clojure.spec :as s]))

;; -- spec ----------------------------------------------------------------------------------------

(s/def ::ball ;; (not a strike or a spare - poorly named...)
  (s/and (s/tuple number? number?)
         ;; Has to be less than 10!
         (s/and #(< (apply + %) 10))))

(s/def ::spare
  (s/and (s/tuple number? number?)
         ;; Has to equal 10
         #(= 10 (apply + %))
         ;; Can't be a strike!
         (fn [ball] (every? #(not= 0 %) ball))))

(s/def ::strike
  ;; Has to equal 10 on first ball!
  (s/tuple #{10} #{0}))

(s/def ::frame
  ;; A frame can either be a ball, spare or strike
  (s/or :ball   ::ball
        :spare  ::spare
        :strike ::strike))

(s/def ::final-frame
  (s/or :ball   ::ball
        :spare  (s/tuple ::spare (s/or ::ball ::strike))
        :strike (s/tuple ::strike ::strike (s/or ::ball ::strike))))

(s/def ::frames
  (s/and (s/cat :frame       (s/* ::ball)
                :final-frame (s/? ::final-frame)) ;; The final frame is a special case

         ;; There is a maximum of 10 frames to a scorecard
         #(<= (count %) 10)))

(s/def ::score
  (s/and number? #(<= % 300)))

(s/def ::scorecard
  (s/keys :req-un [::score ::frames]))

;; -- impl ----------------------------------------------------------------------------------------

;; Create an empty score card
(defn scorecard []
  {:score  0
   :frames []})

(defn- score-strike [score frame]
  (+ score (* 2 (apply + frame))))

(defn- score-spare [score frame]
  (+ score (apply + frame) (first frame)))

;; I'm going to deviate a little and return the next state of the scorecard (with a :score key) instead
;; of returning just the score
(defn score-frame [card frame]
  (let [prev-frame (-> card :frames last)
        next-card  (update card :frames conj frame)]
    (cond
      (s/valid? ::strike prev-frame)
      (update next-card :score #(score-strike % frame))

      (s/valid? ::spare  prev-frame)
      (update next-card :score #(score-spare % frame))

      (s/valid? ::ball prev-frame)
      (update next-card :score #(+ % (apply + frame)))

      :else (throw (IllegalArgumentException. "")))))

(s/fdef score-frame
  :args (s/cat :card ::scorecard :frame ::frame)
  :ret  ::scorecard)

;; Determine if a game is complete - if so, provide the final score
(defn score-game [card]
  (if (= 10 (count card))
    (:score card)
    (throw (IllegalStateException. "Game incomplete"))))