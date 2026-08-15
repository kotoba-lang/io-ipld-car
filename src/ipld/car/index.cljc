(ns ipld.car.index
  "`MultihashIndexSorted` (multicodec 0x0401) — the index a CARv2 carries so a
  reader can find one block without scanning the archive.

  Layout, little-endian throughout except the leading multicodec varint:

      varint(0x0401)
      int32   number of distinct multihash codes
        uint64  multihash code                      (ascending)
        int32   number of distinct digest widths
          uint32  width          = digest length + 8
          int64   byte length of the record run     = count * width
          records                                    (ascending by digest)
            digest bytes || uint64 offset

  The offset is relative to the **start of the CARv1 payload**, not to the
  start of the file, and it points at the frame's length varint rather than at
  the block bytes. Both choices are the format's, not this library's; a reader
  that assumes otherwise reads a valid file wrongly, which is why
  `ipld.car.v2/locate` returns absolute file offsets and never exposes these."
  (:require [ipld.car.bytes :as b]
            [ipld.car :as car]))

(def ^:const codec 0x0401)

(defn- digest-of [cid]
  (let [{:keys [digest mh-code]} (car/read-cid (car/cid->bytes cid) 0)]
    {:digest digest :mh-code mh-code}))

(defn- cmp-bytes [x y]
  (let [n (min (b/bcount x) (b/bcount y))]
    (loop [i 0]
      (cond
        (= i n) (compare (b/bcount x) (b/bcount y))
        (not= (b/bget x i) (b/bget y i)) (compare (b/bget x i) (b/bget y i))
        :else (recur (inc i))))))

(defn encode
  "Index bytes for `entries` — `[{:cid s :payload-offset n} ...]`, where
  `:payload-offset` is the frame offset **within the CARv1 payload**."
  [entries]
  (let [recs (map (fn [{:keys [cid payload-offset]}]
                    (let [{:keys [digest mh-code]} (digest-of cid)]
                      {:digest digest :mh-code mh-code :offset payload-offset}))
                  entries)
        by-code (into (sorted-map) (group-by :mh-code recs))]
    (b/concat
     (into [(b/varint codec) (b/u32-le (count by-code))]
           (mapcat
            (fn [[code rs]]
              (let [by-width (into (sorted-map)
                                   (group-by #(+ 8 (b/bcount (:digest %))) rs))]
                (into [(b/u64-le code) (b/u32-le (count by-width))]
                      (mapcat
                       (fn [[width ws]]
                         (let [sorted (sort-by :digest cmp-bytes ws)]
                           (into [(b/u32-le width)
                                  (b/u64-le (* width (count sorted)))]
                                 (mapcat (fn [{:keys [digest offset]}]
                                           [digest (b/u64-le offset)])
                                         sorted))))
                       by-width))))
            by-code)))))

(defn- read-width-run
  "One `uint32 width || int64 run || records` group at `q`."
  [buf q code]
  (let [width (b/read-u32-le buf q)
        run (b/read-u64-le buf (+ q 4))
        start (+ q 12)
        dlen (- width 8)]
    (when (or (<= width 8) (pos? (mod run width)))
      (throw (ex-info "car: index width/run disagree"
                      {:type :car/index-malformed :width width :run run})))
    {:next (+ start run)
     :records (mapv (fn [k]
                      (let [r (+ start (* k width))]
                        {:mh-code code
                         :digest (b/slice buf r (+ r dlen))
                         :payload-offset (b/read-u64-le buf (+ r dlen))}))
                    (range (quot run width)))}))

(defn- read-code-group
  "One `uint64 code || int32 nwidths || width-runs` group at `p`."
  [buf p]
  (let [code (b/read-u64-le buf p)
        n-widths (b/read-u32-le buf (+ p 8))]
    (loop [j 0 q (+ p 12) acc []]
      (if (= j n-widths)
        {:next q :records acc}
        (let [{:keys [next records]} (read-width-run buf q code)]
          (recur (inc j) next (into acc records)))))))

(defn decode
  "Parse index bytes at `off` → `[{:mh-code n :digest bytes :payload-offset n} ...]`."
  ([buf] (decode buf 0))
  ([buf off]
   (let [{c :value cl :length} (b/read-varint buf off)]
     (when-not (= codec c)
       (throw (ex-info "car: unsupported index codec — only MultihashIndexSorted (0x0401)"
                       {:type :car/index-codec :codec c})))
     (let [pos (+ off cl)
           n-codes (b/read-u32-le buf pos)]
       (loop [i 0 p (+ pos 4) acc []]
         (if (= i n-codes)
           acc
           (let [{:keys [next records]} (read-code-group buf p)]
             (recur (inc i) next (into acc records)))))))))
