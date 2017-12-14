(ns logic-demo.core
  (:require [clojure.core.logic :as l]))

(l/run* [q]
  (l/== q 5))
