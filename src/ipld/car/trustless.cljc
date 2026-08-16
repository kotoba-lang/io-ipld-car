(ns ipld.car.trustless
  "CAR response core for trustless IPLD selection and pathing.

  HTTP parsing, redirects, Cache-Control, Range, and status mapping belong to
  the gateway. This namespace owns the verifiable payload: run a bounded
  selector through CID-verified blocks, then encode exactly the touched blocks
  as a root-first CARv1."
  (:require [ipld.car :as car]
            [ipld.graph :as graph]))

(def content-type "application/vnd.ipld.car")

(defn selection-car
  "Execute `selector` at `root-cid` and encode the proof blocks as CARv1.
  Returns traversal matches/stats plus CAR bytes and offsets."
  [get-fn root-cid selector limits]
  (let [selection (graph/select-blocks get-fn root-cid selector limits)
        archive (car/encode {:roots [root-cid] :blocks (:blocks selection)})]
    (assoc selection
           :content-type content-type
           :car archive)))

(defn path-car
  "Resolve a logical Data Model path through Links and return a CAR proof.
  A missing path fails closed; this function does not claim an absence proof."
  [get-fn root-cid path limits]
  (let [result (selection-car get-fn root-cid (graph/path-selector path) limits)]
    (when (empty? (:matches result))
      (throw (ex-info "trustless path did not resolve"
                      {:type :ipld/path-not-found
                       :root root-cid :path (vec path)})))
    (assoc result :path (vec path) :value (get-in result [:matches 0 :value]))))
