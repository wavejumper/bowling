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

(s/def ::scorecard
  (s/and (s/cat :frame       (s/* ::ball)
                :final-frame (s/? ::final-frame)) ;; The final frame is a special case

         ;; There is a maximum of 10 frames to a scorecard
         #(<= (count %) 10)))

;; -- impl ----------------------------------------------------------------------------------------

;; Create an empty score card
(defn scorecard []
  [])

;; Given a score card, score a frame
(defn score-frame [card frame]
  (let [prev-frame (last card)]
    (cond
      (s/valid? ::strike prev-frame)
      nil

      (s/valid? ::spare  prev-frame)
      nil

      (s/valid? ::ball prev-frame)
      (apply + frame)

      :else (throw (IllegalArgumentException. "")))))

;; Determine if a game is complete - if so, provide the final score
(defn score-game [card]
  
  (throw (IllegalStateException. "Game incomplete")))