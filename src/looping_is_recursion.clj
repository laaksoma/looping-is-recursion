(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                    (if (zero? n)
                      acc
                      (recur (* base acc) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [helper (fn [a-elem]
                   (if (empty? (rest a-elem))
                     (first a-elem)
                     (recur (rest a-elem))))]
      (helper a-seq))))

(defn seq= [seq1 seq2]
  (if (= (count seq1) (count seq2))
    (let [helper (fn [a-seq b-seq]
                  (cond
                    (and (empty? a-seq) (empty? b-seq)) true
                    (= (first a-seq) (first b-seq))     (recur (rest a-seq) (rest b-seq))
                    :else                               false))]
      (helper seq1 seq2))
    false))

(defn find-first-index [pred a-seq]
  (if (empty? a-seq)
    nil
    (loop [index 0
           seq1 a-seq]
      (cond
        (empty? seq1)       nil
        (pred (first seq1)) index
        :else               (recur (inc index) (rest seq1))))))

(defn avg [a-seq]
  (if (empty? a-seq)
    0
    (loop [sum 0
           div (count a-seq)
           ind 0
           seq1 a-seq]
      (if (= ind div)
        (/ sum div)
        (recur (+ sum (first seq1)) div (inc ind) (rest seq1))))))

(defn parity [a-seq]
  (loop [seq1 a-seq
         set1 #{}]
    (cond
      (empty? seq1)                 set1
      (contains? set1 (first seq1)) (recur (rest seq1) (disj set1 (first seq1)))
      :else                         (recur (rest seq1) (conj set1 (first seq1))))))

(defn fast-fibo [n]
  (if (or (= n 0) (= n 1))
    n
    (loop [F-1 0
           Fn 1
           ind 1]
      (if (= ind n)
        Fn
        (recur Fn (+ Fn F-1) (inc ind))))))

(defn cut-at-repetition [a-seq]
  (loop [orig-seq a-seq
         new-v    []
         help-set #{}]
    (cond
      (empty? orig-seq)                     new-v
      (contains? help-set (first orig-seq)) new-v
      :else                                 (recur (rest orig-seq)
                                                   (conj new-v (first orig-seq))
                                                   (conj help-set (first orig-seq))))))




