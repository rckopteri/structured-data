(ns structured-data)

(defn do-a-thing [x]
  (let [athing (+ x x)]
    (Math/pow  athing athing)))

(defn spiff [v]
  (let [[first-value third-value] [(get v 0) (get v 2)]]
      (+ first-value third-value)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[first-value third-value] [(get v 0) (get v 2)]]
    (+ first-value third-value)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (== (- x1 x2) (- y1 y2))
      true false)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [x y] point]

     (if (and
           (<= y y2) (>= y y1) (<= x x2) (>= x x1))

      true false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer [[xx1 yy1] [xx2 yy2]] inner]
   (if (and (contains-point? outer [xx1 yy1])
            (contains-point? outer [xx1 yy2])
            (contains-point? outer [xx2 yy1])
            (contains-point? outer [xx2 yy2] )) true false)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [original-authors (get book :authors)
        new-authors (assoc original-authors (count original-authors) new-author)]
    (assoc book :authors new-authors))) ; Return results

(defn alive? [author]
  (if (nil? (get author :death-year)) true false))

(defn count-collection-length [x]
  (count x))

(defn element-lengths [collection]
  (map count-collection-length collection))


(defn second-elements [collection]
  (map
    (fn [x] (get x 1))
    collection
    ))


(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (let [star-seq (repeat n "*")]
       (apply str star-seq)))

(defn toggle [a-set elem]
  (let [set_size (count a-set)]
    (if(contains? a-set elem)
      (disj a-set elem) (conj a-set elem))))

;Not needed, just testing if it works. and it works - copied from interweb
(defmacro for-loop [[sym init check change :as params] & steps]
 `(loop [~sym ~init value# nil]
    (if ~check
      (let [new-value# (do ~@steps)]
        (recur ~change new-value#))
      value#)))

(defn contains-duplicates? [a-seq]
  (let [original-size (count a-seq) set-size (count (set a-seq))]
    ; Since set doesn't contain duplicates we return false when size doesn't match
    (if(== set-size original-size) false  true)))

(defn old-book->new-book [book]
  (let [original-authors (get book :authors)
        changed-authors (set original-authors)]
    (assoc book :authors changed-authors)))


(defn has-author? [book author]
  (let [book-authors (get (old-book->new-book book) :authors)]
    (if(contains? book-authors author) true false)))

(defn authors [books]
  (let [authors-map (map :authors books)]
    (apply clojure.set/union authors-map)))

(defn all-author-names [books]
  (let [author-names (authors books)]
    (set (map :name author-names))))

(defn author->string [author]
  (let [name (str (get author :name))
        born (str (get author :birth-year))
        death (str (get author :death-year))]
    (if (not= "" born) (str name " (" born " - " death ")") (str name) )))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [book-title (get book :title)
        authors (apply str (authors->string (get book :authors)))]
    (str book-title "," " written by " authors)))

(defn books->string [books]
  (let [book-count (count books)
        book-titles (apply str (interpose ", " (map book->string books)))]
    (cond
      (< book-count 1) (str "No books.")
      (= book-count 1) (str book-count " book. " book-titles ".")
      :else (str book-count " books. " book-titles ".")
      )
    ))

(defn books-by-author [author books]
  (filter (fn [x ](has-author? x author )) books)
  )

(defn author-by-name [name authors]
 ; Loop auhtors and return if a match
  (let [fn-check (fn [x] (if(= name (get x :name)) true false))
        author (filter fn-check authors)]
    (if(= 1 (count author)) (first author) nil)

    )
  )

(defn living-authors [authors]
  (let [fn-check (fn [x] (alive? x))
    living-seq (filter fn-check authors)]
    living-seq)
  )
;(living-authors authors-set)
(defn has-a-living-author? [book]
  (if(= (count (living-authors (get book :authors))) 0) false true)
  )

(defn books-by-living-authors [books]
  (let [book-seq (filter (fn [x] (= (has-a-living-author? x) true)) books)]
    book-seq
    )
  )

; %________%
