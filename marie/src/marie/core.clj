(ns marie.core)

(defrecord Instruction [function arguments])
(defrecord Data [value])
(defrecord Halt [])
(defrecord Marie [memory AC pc])

(defn makeMarie 
  ([]
    (Marie. {} 0 0)
  )
  ([memory]
    (Marie. memory 0 0)
  )
  ([memory ac]
    (Marie. memory ac 0)
  )
)

(defn updateMemory [marie memory]
  (Marie. memory (:AC marie) (:pc marie))
)

(defn updateAC [marie ac]
  (Marie. (:memory marie) ac (:pc marie))
)

(defn incrementPC [marie]
  (Marie. (:memory marie) (:AC marie) (+ (:pc marie) 1))
)

(defn getData [marie addr]
  (:value (get (:memory marie) addr))
)

(defn load [marie args]
  (let [addr (get args 0) val (:value (get (:memory marie) addr))]
    (if (nil? val)
      (updateAC marie 0)
      (updateAC marie val)
    )
  )
)

(defn store [marie args]
  (let [addr (get args 0) newmem (assoc (:memory marie) addr (Data. (:AC marie)))]
    (updateMemory marie newmem)
  )
)

(defn add [marie args]
  (let [addr (get args 0) arg (getData marie addr)]
    (updateAC marie (+ arg (:AC marie)))
  )
)


(defn run [marie]
  (let [addr (:pc marie) instruction (get (:memory marie) addr)]
    (if (instance? Instruction instruction)
      (let [func (:function instruction)
            args (:arguments instruction)
            newMarie (func marie args)]
        (do
          (println newMarie)
          (println "")
          (run (incrementPC newMarie))
        )
      )
      marie
    )
  )
)

(def m {0 (Instruction. load [4])
        1 (Instruction. add [5])
        2 (Instruction. store [4])
        3 (Halt.)
        4 (Data. 1)
        5 (Data. 2)
       }
)

(run (makeMarie m))
