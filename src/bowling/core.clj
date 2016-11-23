(ns bowling.core
  "Ten-pin bowling game scorecard.

  Build an API that implements a ten-pin bowling scorecard"
  (:require [clojure.spec :as s]))

;; -- spec ----------------------------------------------------------------------------------------

(s/def ::scorecard any?)

(s/def ::result any?)

;; -- impl ----------------------------------------------------------------------------------------

;; Create an empty score card
(defn scorecard []
  {})

;; Given a score card, score a frame
(defn score-frame [card result]
  card)

;; Determine if a game is complete - if so, provide the final score
(defn game-complete? []
  true)