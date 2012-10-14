; Eric C. Harshbarger - "Go First" dice
; Fairness Report
;
; Dice Count: 4
; Face Count: 12
; Die 1: 1, 8, 11, 14, 19, 22, 27, 30, 35, 38, 41, 48
; Die 2: 2, 7, 10, 15, 18, 23, 26, 31, 34, 39, 42, 47
; Die 3: 3, 6, 12, 13, 17, 24, 25, 32, 36, 37, 43, 46
; Die 4: 4, 5,  9, 16, 20, 21, 28, 29, 33, 40, 44, 45

(def dice-count   4)
(def face-count  12)
(def die0  (list  1, 8, 11, 14, 19, 22, 27, 30, 35, 38, 41, 48))
(def die1  (list  2, 7, 10, 15, 18, 23, 26, 31, 34, 39, 42, 47))
(def die2  (list  3, 6, 12, 13, 17, 24, 25, 32, 36, 37, 43, 46))
(def die3  (list  4, 5,  9, 16, 20, 21, 28, 29, 33, 40, 44, 45))
(def dices (hash-map :die0 die0, :die1 die1, :die2 die2, :die3 die3))

; Outcome n: D1=x  D2=x  Ranking: DX,DX

(defn pick-a-dice []
  (dices (nth (keys dices) (rand-int dice-count))))

(defn throw-a-dice []
  (let [dice (pick-a-dice)]
    (nth dice (rand-int face-count))))

(throw-a-dice)

;
