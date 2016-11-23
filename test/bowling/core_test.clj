(ns bowling.core-test
  (:require [clojure.test :refer :all]
            [bowling.core :refer :all]))

;; I could use property based testing with core.spec, but I'll keep it simple...

(deftest score-frame-test
  ;; Wikipedia example of strike
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
          :frames [[4 3] [4 3]]})))

;; (run-tests)