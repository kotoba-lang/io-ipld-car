(ns ipld.car-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            [ipld.car :as car]
            [ipld.car.bytes :as b]
            [ipld.car.index :as idx]
            [ipld.car.v2 :as v2]
            [ipld.core :as ipld]))

(defn- block [node] (ipld/node->block node))

(def leaf-a (block {"kind" "leaf" "v" 1}))
(def leaf-b (block {"kind" "leaf" "v" 2}))
(def root-node (block {"kind" "root"
                       "children" [(ipld/link (:cid leaf-a))
                                   (ipld/link (:cid leaf-b))]}))
(def blocks [root-node leaf-a leaf-b])

;; ── bytes ────────────────────────────────────────────────────────────────────

(deftest varint-roundtrip
  (doseq [n [0 1 127 128 255 256 16383 16384 1000000 (dec (long (Math/pow 2 32)))]]
    (let [bs (b/varint n)]
      (is (= {:value n :length (b/bcount bs)} (b/read-varint bs 0))
          (str "varint " n)))))

(deftest varint-refuses-unterminated
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (b/read-varint (b/->bytes [0x80 0x80 0x80]) 0))))

(deftest u64-refuses-inexact
  (testing "the format allows it, this library does not — a JS host cannot hold it"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (b/u64-le (+ b/max-safe-integer 1))))))

(deftest u64-roundtrip
  (doseq [n [0 1 255 4294967295 4294967296 1099511627776 b/max-safe-integer]]
    (is (= n (b/read-u64-le (b/u64-le n) 0)) (str "u64 " n))))

;; ── CARv1 ────────────────────────────────────────────────────────────────────

(deftest carv1-roundtrip
  (let [{:keys [bytes entries]} (car/encode {:roots [(:cid root-node)] :blocks blocks})
        parsed (car/decode bytes)]
    (is (= [(:cid root-node)] (:roots parsed)))
    (is (= (mapv :cid blocks) (mapv :cid (:entries parsed))))
    (is (= (mapv #(dissoc % :cid) entries) (mapv #(dissoc % :cid) (:entries parsed)))
        "the writer's offsets are the reader's offsets")
    (doseq [{:keys [cid bytes]} blocks]
      (is (b/equal? bytes (get (:blocks parsed) cid))))))

(deftest carv1-offsets-address-the-real-frame
  (let [{:keys [bytes entries]} (car/encode {:roots [] :blocks blocks})]
    (doseq [{:keys [cid frame-offset frame-length block-offset block-length]} entries]
      (let [frame (b/slice bytes frame-offset (+ frame-offset frame-length))
            data (b/slice bytes block-offset (+ block-offset block-length))]
        (is (= cid (:cid (v2/read-frame frame 0))) "frame slice parses standalone")
        (is (b/equal? data (:bytes (v2/read-frame frame 0))))))))

(deftest carv1-rejects-truncation
  (let [{:keys [bytes]} (car/encode {:roots [] :blocks blocks})]
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (car/decode (b/slice bytes 0 (- (b/bcount bytes) 3)))))))

(deftest verify-block-catches-substitution
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (car/verify-block (:cid leaf-a) (:bytes leaf-b)))))

;; ── CARv2 ────────────────────────────────────────────────────────────────────

(deftest carv2-header-shape
  (let [{:keys [bytes data-offset data-size index-offset]}
        (v2/pack {:roots [(:cid root-node)] :blocks blocks})]
    (is (= 51 data-offset) "11-byte pragma + 40-byte header")
    (is (= {:data-offset data-offset :data-size data-size :index-offset index-offset}
           (v2/parse-header bytes)))
    (is (= index-offset (+ data-offset data-size)))
    (is (b/equal? v2/pragma (b/slice bytes 0 11)))))

(deftest carv2-roundtrip
  (let [{:keys [bytes]} (v2/pack {:roots [(:cid root-node)] :blocks blocks})
        parsed (v2/read-all bytes)]
    (is (= [(:cid root-node)] (:roots parsed)))
    (is (= (set (map :cid blocks)) (set (keys (:blocks parsed)))))))

(deftest carv2-index-locates-every-block
  (let [{:keys [bytes entries] :as packed} (v2/pack {:roots [] :blocks blocks})
        records (v2/read-index bytes)
        hdr (v2/parse-header bytes)]
    (is (= (count blocks) (count records)))
    (doseq [{:keys [cid file-offset]} entries]
      (is (= {:file-offset file-offset} (v2/locate hdr records cid))
          "the index agrees with what the writer recorded"))
    (is (nil? (v2/locate hdr records (:cid (block {"kind" "absent"})))))
    (is (= (:index-offset packed) (:index-offset hdr)))))

(deftest carv2-index-is-optional
  (let [{:keys [bytes index-offset]} (v2/pack {:roots [] :blocks blocks :index? false})]
    (is (zero? index-offset))
    (is (nil? (v2/read-index bytes)))
    (is (= 3 (count (:blocks (v2/read-all bytes)))))))

(deftest index-roundtrip-preserves-offsets
  (let [{:keys [entries]} (v2/pack {:roots [] :blocks blocks})
        wire (idx/encode (mapv #(select-keys % [:cid :payload-offset]) entries))
        back (idx/decode wire)]
    (is (= (set (map :payload-offset entries)) (set (map :payload-offset back))))))

;; ── the point of the whole repository: one range read ───────────────────────

(deftest a-range-read-returns-a-verified-block
  (testing "a client holding only (pack bytes, cid) fetches one range and proves it"
    (let [{:keys [bytes entries]} (v2/pack {:roots [] :blocks blocks})
          hdr (v2/parse-header bytes)
          records (v2/read-index bytes)
          target (nth entries 2)
          {:keys [file-offset]} (v2/locate hdr records (:cid target))
          ;; what an HTTP range GET would hand back, and nothing else
          body (b/slice bytes file-offset (+ file-offset (:frame-length target)))
          {:keys [cid] :as got} (v2/read-frame body 0)]
      (is (= (:cid target) cid))
      (is (b/equal? (:bytes (nth blocks 2)) (:bytes got)))
      (is (= (v2/range-header target) (str "bytes=" file-offset "-"
                                           (+ file-offset (:frame-length target) -1)))))))

(deftest a-tampered-range-fails-closed
  (let [{:keys [bytes entries]} (v2/pack {:roots [] :blocks blocks})
        {:keys [file-offset frame-length]} (first entries)
        body (b/slice bytes file-offset (+ file-offset frame-length))
        flipped (b/concat [(b/slice body 0 (dec (b/bcount body)))
                           (b/->bytes [(bit-xor 0xff (b/bget body (dec (b/bcount body))))])])]
    (is (thrown? #?(:clj Exception :cljs js/Error) (v2/read-frame flipped 0)))))

(deftest a-short-range-says-so-instead-of-returning-half-a-block
  (let [{:keys [bytes entries]} (v2/pack {:roots [] :blocks blocks})
        {:keys [file-offset frame-length]} (first entries)
        body (b/slice bytes file-offset (+ file-offset (quot frame-length 2)))]
    (is (thrown? #?(:clj Exception :cljs js/Error) (v2/read-frame body 0)))))
