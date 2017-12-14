(ns )

(require '[clojure.core.logic :as l])

;; unify variables with plain values
(l/run* [q]
  (l/== q :D))

;; multiple logic variables
(l/run* [q r]
  (l/== q :D)
  (l/== r :C))

;; variables can be left undefined
(l/run* [q r]
  (l/== q 10))

;; definitions can't be contradictory
(l/run* [q]
  (l/== q 10)
  (l/== q 15))

;; variables can be unified with other variables
(l/run* [q r]
  (l/== q 10)
  (l/== r q))

;; and we can change the order
(l/run* [q r]
  (l/== q r)
  (l/== q 10))

;; not all variables need to be exposed
(l/run* [q]
  (l/fresh [r]
    (l/== q r)
    (l/== r :D)))

;; we don't need to limit ourselves to plain values
(l/run* [q]
  (l/== q [:a :b]))

;; define things in parts
(l/run* [q]
  (l/fresh [r]
    (l/== q [r 10])
    (l/== r 5)))

;; some standard things we'll use
(l/run* [q]
  (l/firsto [1 2 3] q))

(l/run* [q]
  (l/firsto q :first))

(l/run* [a-seq]
  (l/firsto a-seq :first)
  (l/fresh [another-seq]
    (l/resto a-seq another-seq)
    (l/firsto another-seq :second)))

;; time to branch
(l/run* [q]
  (l/conde [(l/== q :one)]
           [(l/== q :two)]
           [(l/== q :three)]))

;; now can generate combinations
(l/run* [q]
  (l/fresh [r1 r2]
    (l/== q [r1 r2])
    (l/conde [(l/== r1 :a)]
             [(l/== r1 :b)])
    (l/conde [(l/== r2 1)]
             [(l/== r2 2)])))

;;the branches can have multiple constraints
(l/run* [q]
  (l/fresh [r1 r2]
    (l/== q [r1 r2])
    (l/conde [(l/== r1 :a)]
             [(l/== r1 :b)])
    (l/conde [(l/== r2 1)]
             [(l/== r2 2)
              (l/== r1 :a)
              ;; this branch only succeeds when r1 is :a
              ])))

;; there's a helper for that
(l/run* [q]
  (l/fresh [r1 r2]
    (l/== q [r1 r2])
    (l/membero r1 [:a :b])
    (l/membero r2 [1 2])))

(l/run* [q]
  (l/membero q [1 2 3]))

(l/run* [q]
  (l/membero 2 [1 2 3]))

(l/run* [q]
  (l/membero 2 [1 q 3]))

(l/run* [q]
  (l/membero q [1 2 3])
  (l/membero q [2 3 4]))

;; let's sey we want to generate a schedule for a
;; small two track conference

;; here's some values to represent time slots
(def times [[:12:00 :12:30]
            [:12:30 :13:00]
            [:13:00 :13:30]
            [:13:30 :14:00]])

;; and these will be used to represent our two stages
(def places [:main-stage :side-stage])

;; lets generate all [time place] combinations
(l/run* [time place]
  (l/membero time times)
  (l/membero place places))

(pprint *1)

;; batter save these
(def times-and-places
  (vec (l/run* [time place]
         (l/membero time times)
         (l/membero place places))))

(pprint times-and-places)

;; time to express some constraints
;; these constraints/relations are just clojure functions
(defn main-stage [spec]
  (l/fresh [time]
    (l/== spec [time :main-stage])))

(l/run* [spec]
  (l/== spec [[:12:00 :12:30] :main-stage])
  (main-stage spec))

;; we will have one double length session
;; we need some additional constraints for it
(defn same-place [spec1 spec2]
  (l/fresh [time1 time2 place]
    (l/== spec1 [time1 place])
    (l/== spec2 [time2 place])))

(l/run* [q]
  (same-place [:_ :side-stage] [:_ :main-stage]))

(defn consecutive [spec1 spec2]
  (l/fresh [start1 end1 start2 end2]
    (l/firsto spec1 [start1 end1])
    (l/firsto spec2 [start2 end2])
    (l/== end1 start2)))

(pprint times-and-places)

(l/run* [spec1 spec2]
  (l/membero spec1 times-and-places)
  (l/membero spec2 times-and-places)
  (same-place spec1 spec2)
  (consecutive spec1 spec2))

(pprint *1)

(defn double-length [spec1 spec2]
  (same-place spec1 spec2)
  (consecutive spec1 spec2))

;; our opening talk is fixed to a single slot
(defn opening-spec [spec]
  (l/== spec [[:12:00 :12:30] :main-stage]))

(defn workshop-spec [spec1 spec2]
  (double-length spec1 spec2))

;; core.logic also allows != constraints
(defn different-time [spec1 spec2]
  (l/fresh [time1 time2]
    (l/firsto spec1 time1)
    (l/firsto spec2 time2)
    (l/!= time1 time2)))

;; a small interlude,
;; turned out I needed this helper constraint
(defn shortero [seq1 seq2]
  (l/conde
    [(l/emptyo seq1)]
    [(l/fresh [fst1 fst2 rst1 rst2]
       (l/firsto seq1 fst1)
       (l/firsto seq2 fst2)
       (l/resto seq1 rst1)
       (l/resto seq2 rst2)
       (shortero rst1 rst2))]))

(l/run* [q]
  (l/fresh [a-seq]
    (l/== a-seq [1 2 3])
    (shortero q a-seq)))

(defn subseto-helper [seq1 seq2]
  (l/conde
    [(l/emptyo seq1)]
    [(l/fresh [fst rst]
       (l/firsto seq1 fst)
       (l/resto seq1 rst)
       (l/membero fst seq2)
       (subseto-helper rst seq2))]))

(defn subseto [seq1 seq2]
  (l/fresh []
    (shortero seq1 seq2)
    (l/distincto seq1)
    (subseto-helper seq1 seq2)))

(l/run* [q]
  (l/!= q 5)
  (l/!= q 10))

(l/run* [q]
  (l/fresh [seq1 seq2]
    (l/== seq1 [1 2 3])
    (subseto q seq1)))

(defn programme-spec [prog]
  (l/fresh [opening clj-web-programming
            cljs-workshop-1 cljs-workshop-2
            f-sharp-database f-sharp-dsl
            haskell-profunctor-optics
            haskell-frp
            all]

    (l/== all [opening clj-web-programming cljs-workshop-1
               cljs-workshop-2 f-sharp-database f-sharp-dsl
               haskell-frp haskell-profunctor-optics])

    (subseto all times-and-places)

    (l/== prog [[:opening opening]
                [:clj-web-programming clj-web-programming]
                [:f-sharp-database f-sharp-database]
                [:f-sharp-dsl f-sharp-dsl]
                [:cljs-workshop-1 cljs-workshop-1]
                [:cljs-workshop-2 cljs-workshop-2]
                [:haskell-frp haskell-frp]
                [:haskell-profunctor-optics haskell-profunctor-optics]])

    (opening-spec opening)

    (different-time f-sharp-dsl f-sharp-database)

    (workshop-spec cljs-workshop-1 cljs-workshop-2)
    (different-time cljs-workshop-1 clj-web-programming)
    (different-time cljs-workshop-2 clj-web-programming)

    (different-time haskell-frp haskell-profunctor-optics)))

(l/run 1 [programme]
  (programme-spec programme))

(pprint *1)

;; here's another example:
;; find which courses you can currently take, given their
;; prerequisites
(def courses
  [[:databases []]
   [:intro-math []]
   [:intro-programming []]
   [:second-programming [:intro-programming]]
   [:algorithms [:intro-programming :intro-math]]
   [:database-programming [:databases :intro-programming]]
   [:web-programming [:database-programming]]])

(defn not-in [x lst]
  (l/conde
    [(l/emptyo lst)]
    [(l/fresh [fst rst]
       (l/firsto lst fst)
       (l/resto lst rst)
       (l/!= x fst)
       (not-in x rst))]))

(defn possible-courses [taken-courses result]
  (l/fresh [course course-name course-reqs]
    (l/membero course courses)
    (l/== course [course-name course-reqs])
    (subseto course-reqs taken-courses)
    (not-in course-name taken-courses)
    (l/== course-name result)))

(l/run* [result]
  (possible-courses [:intro-math :intro-programming] result))

;; split these people into groups of 3 or 4 such that
;; they have a common time when they can meet
(def people
  [[:name1 [[:mon [:18 :20]]
            [:tue [:16 :18 :20]]
            [:wed [:18]]]]
   [:name2 [[:mon [:20]]
            [:tue [:18 :20]]
            [:wed [:16 :18 :20]]]]
   [:name3 [[:mon [:16 :18]]
            [:tue [:16]]
            [:wed [:16]]]]
   [:name4 [[:mon [:20 :22]]
            [:tue [:20 :22]]
            [:wed [:20 :22]]]]
   [:name5 [[:mon [:18]]
            [:tue [:18 :22]
             :wed [:20 :22]]]]
   [:name6 [[:mon [:22]]
            [:tue [:20 :22]]
            [:wed [:20 :22]]]]
   [:name7 [[:mon [:16 :20]]
            [:tue [:16 :18 :20]]
            [:wed [:18 :20]]]]])

(defn mapo [rel lst res]
  (l/conde [(l/emptyo lst) (l/== res [])]
           [(l/fresh [fst rst result-fst result-rst]
              (l/conso fst rst lst)
              #_(l/firsto lst fst)
              #_(l/resto lst rst)
              (rel fst result-fst)
              (mapo rel rst result-rst)
              (l/conso result-fst result-rst res))]))

(l/run* [q]
  (mapo l/firsto [[:1 1] [:2 2] [:3 3]] q))

(defn secondo [lst x]
  (l/fresh [fst rst]
    (l/conso fst rst lst)
    (l/firsto rst x)))

(l/run* [q]
  (mapo secondo [[:1 1] [:2 2] [:3 3]] q))

(defn a-time [all-times result]
  "all-times is a sequence of day-times.
day-times is a pair [weekday times]
weekday is one of :mon, :tue, :wed, :thu,:fri, :sat, :sun
times is sequence of elements describing a time of day, like
:18, :20 or :22

a-time is a pair [weekday time], like [:wed :16]"
  (l/fresh [day-times day times clock]
    (l/membero day-times all-times)
    (l/== [day times] day-times)
    (l/membero clock times)
    (l/== result [day clock])))

(l/run* [q]
  (a-time [[:mon [:22]]
           [:tue [:20 :22]]
           [:wed [:20 :22]]]
          q))

(defn group-of-3-4 [all-people result]
  (l/fresh [lower upper all-names]
    (l/== lower [1 2 3])
    (l/== upper [1 2 3 4])
    (shortero lower result)
    (shortero result upper)
    (subseto result all-people)))

(defn for-eacho [lst rel]
  (l/conde
    [(l/emptyo lst)]
    [(l/fresh [fst rst]
       (l/conso fst rst lst)
       (rel fst)
       (for-eacho rst rel))]))

(l/run* [q]
  (l/== q true)
  (for-eacho [[:a :b :c] [:b :d] [:b]]
             (fn [x] (l/membero :b x))))

(defn a-time-of-person [person time]
  (l/fresh [persons-times]
    (secondo person persons-times)
    (a-time persons-times time)))

(l/run* [time]
  (a-time-of-person [:name7 [[:mon [:16 :20]]
                             [:tue [:16 :18 :20]]
                             [:wed [:18 :20]]]]
                    time))

(defn common-time [all-people result-time]
  (for-eacho all-people
             (fn [person] (a-time-of-person person result-time))))

(l/run* [time]
  (common-time [[:name6 [[:mon [:22]]
                         [:tue [:20 :22]]
                         [:wed [:20 :22]]]]
                [:name7 [[:mon [:16 :20]]
                         [:tue [:16 :18 :20]]
                         [:wed [:18 :20]]]]]
               time))

;; people was defined earlier
(pprint people)

(l/run 1 [people-seq]
  (subseto people-seq people)
  (shortero [1 2 3 4] people-seq)
  (common-time people-seq [:mon :20]))

(pprint *1)

;; now let's define a group of people together with a common time

(defn group-with-time [all-people result]
  (l/fresh [group time names]
    (group-of-3-4 all-people group)
    (common-time group time)
    (mapo l/firsto group names)
    (l/== result [time names])))

(l/run 1 [result-group]
  (group-with-time people result-group))

;;how about two groups

(defn two-groups-1 [all-people result]
  (l/fresh [group1 group2]
    (group-with-time all-people group1)
    (group-with-time all-people group2)
    (l/== result [group1 group2])))

(l/run 1 [result-groups]
  (two-groups-1 people result-groups))

;;uh oh, we needed different groups

(defn not-ino [elem a-seq]
  (l/conde
    [(l/emptyo a-seq)]
    [(l/fresh [fst rst]
       (l/conso fst rst a-seq)
       (l/!= fst elem)
       (not-ino elem rst))]))

(l/run* [x]
  (l/membero x [:a :b :c :d])
  (not-ino x [:a :c]))

(defn dont-intersect [seq1 seq2]
  (for-eacho seq1 (fn [elem] (not-ino elem seq2))))

(l/run* [x y]
  (subseto [x y] [[:a :b :c]
                  [:a :d :e]
                  [:d :e :f]
                  [:b :f :g]])
  (dont-intersect x y))

(pprint *1)

;; now let's try this again
(defn two-groups [all-people result]
  (l/fresh [time1 names1 time2 names2]
    (group-with-time all-people [time1 names1])
    (group-with-time all-people [time2 names2])
    (l/!= time1 time2)
    (dont-intersect names1 names2)
    (l/== result [[time1 names1]
                  [time2 names2]])))

(l/run 1 [result]
  (two-groups people result))
