(ns bowling.core-test
  (:require [clojure.test :refer :all]
            [bowling.core :refer :all]))

;; I could use property based testing with core.spec, but I'll keep it simple!

(def ^:private strike [10 0])

(deftest score-frame-test
  ;; Wikipedia example of a strike
  (is (= (score-frame {:score  10
                       :frames [[10 0]]}
                      [3 6])
         {:score 28
          :frames [[10 0] [3 6]]}))

  ;; Wikipedia example of a spare
  (is (= (score-frame {:score  10
                       :frames [[7 3]]}
                      [4 2])
         {:score 20
          :frames [[7 3] [4 2]]}))

  ;; Standard ball
  (is (= (score-frame {:score  7
                       :frames [[4 3]]}
                      [4 3])
         {:score  14
          :frames [[4 3] [4 3]]}))

  ;; Strike 'bonus' example from wikipedia
  (is (= (-> (scorecard)
             (score-frame strike)
             (score-frame [3 6])
             :score)
         28))

  ;; Double 'pinfall' example from wikipedia
  (is (= (-> (scorecard)
             (score-frame strike)
             (score-frame strike)
             (score-frame [9 0])
             :score)
         57))

  ;; Turkey's pinfall from wikipedia
  (is (= (-> (scorecard)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame [0 9])
             :score)
         78))

  ;; Test bonus logic...
  ;; Check the 'perfect game' equals 300
  (is (= (-> (scorecard)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame [strike strike strike])
             :score)
         300))

  (is (= (-> (scorecard)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame [[7 3] [10 0]])
             :score)
         277))

  (is (= (-> (scorecard)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame strike)
             (score-frame [[6 0]])
             :score)
         258))

  ;; Test bad states

  ;; Test bad scorecard input(s)
  (is (thrown? AssertionError (score-frame nil strike)))
  (is (thrown? AssertionError (score-frame {:score "1" :frames []} strike)))

  (let [bad-frame [1]]
    (is (thrown? AssertionError (score-frame {:score 0 :frames bad-frame} strike)))
    (is (thrown? AssertionError (score-frame (scorecard) bad-frame)))))