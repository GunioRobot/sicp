(ns sicp.ex2_66)

(defn lookup [given-key set-of-records]
  (if (empty? set-of-records)
    false
    (let [record (entry set-of-records)]
      (cond (= given-key (get-key record)) record
            (< given-key (get-key record)) (lookup given-key (left-branch set-of-records))
            (> given-key (get-key record)) (lookup given-key (right-branch set-of-records))))))