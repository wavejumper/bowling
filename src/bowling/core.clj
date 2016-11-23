(ns bowling.core
  "Ten-pin bowling game scorecard.

  Build an API that implements a ten-pin bowling scorecard"
  (:require [clojure.spec :as s]))

;; -- spec ----------------------------------------------------------------------------------------

(s/def ::ball ;; (not a strike or a spare - poorly named...)
  (s/and (s/tuple number? number?)
         ;; Has to be less than 10!
         (s/and #(< 10 (apply + %)))))

(s/def ::spare
  (s/and (s/tuple number? number?)
         ;; Has to equal 10
         #(= 10 (apply + %))
         ;; Can't be a strike!
         #(every? (not= 0 %) %)))

(s/def ::strike
  ;; Has to equal 10 on first ball!
  (s/tuple #{10} #{0}))

(s/def ::frame
  ;; A frame can either be a ball, spare or strike
  (s/or ::ball ::spare ::strike))

(s/def ::final-frame
  (s/or ::ball
        (s/tuple ::spare (s/or ::ball ::strike))
        (s/tuple ::strike ::strike (s/or ::ball ::strike))))

(s/def ::frames
  (s/and (s/cat :frame       (s/* ::ball)
                :final-frame (s/? ::final-frame)) ;; The final frame is a special case

         ;; There is a maximum of 10 frames to a scorecard
         #(<= (count %) 10)))

(s/def ::score
  (s/and number? #(<= % 300)))

(s/def ::scoreboard
  (s/keys :req-un [::score ::frames]))

;; -- impl ----------------------------------------------------------------------------------------

;; Create an empty score card
(defn scorecard []
  {:score  0
   :frames []})

;; Given a score card, score a frame

(defn- score-strike [score frame])

(defn- score-spare [score frame])

;; I'm going to deviate a little and return the next state of the scorecard (with a :score key) instead
;; of returning just the score
(defn score-frame [card frame]
  (let [prev-frame (last card)
        next-card  (update card :frames conj frame)]
    (cond
      (s/valid? ::strike prev-frame)
      (update next-card :score #(score-strike % frame))

      (s/valid? ::spare  prev-frame)
      (update next-card :score #(score-spare % frame))

      (s/valid? ::ball prev-frame)
      (update next-card :scpre #(+ % (apply + frame)))

      :else (throw (IllegalArgumentException. "")))))

;; Determine if a game is complete - if so, provide the final score
(defn score-game [card]
  (if (= 10 (count card))
    (:score card)
    (throw (IllegalStateException. "Game incomplete"))))