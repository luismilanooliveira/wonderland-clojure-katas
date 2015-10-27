(ns alphabet-cipher.coder)

(defn char-to-int [x]
  (- (int x) (int \a)))

(defn int-to-char [x]
  (char (+ x (int \a))))

(defn normalize [x]
  (if (< x (char-to-int \a))
    (+ x (char-to-int \z) 1)
        (if (> x (char-to-int \z))
            (- x (char-to-int \z) 1)
            x)))

(defn encode [keyword message]
  (let [keyword-keys (map char-to-int keyword)
        message-keys (map char-to-int message)
        summed-keys  (map + message-keys (cycle keyword-keys))
        normalized-keys (map normalize summed-keys)]
    (apply str (map int-to-char normalized-keys))))

(defn decode [keyword message]
  (let [keyword-keys (map char-to-int keyword)
        message-keys (map char-to-int message)
        diffed-keys  (map - message-keys (cycle keyword-keys))
        normalized-keys (map normalize diffed-keys)]
  (apply str (map int-to-char normalized-keys))))

(defn decipher [cipher message]
  "decypherme")

