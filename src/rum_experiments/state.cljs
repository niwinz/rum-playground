(ns rum-experiments.state
  (:require [promesa.core :as p]
            [cats.labs.lens :as l]))

(defprotocol IStore
  "A low-level store interface."
  (-init [_] "Initialize the store")
  (-emit [_ t k p] "Emit event to store."))

(defrecord Store [state init read novelty]
  cljs.core/IDeref
  (-deref [_] (deref state))

  cljs.core/IWatchable
  (-add-watch [self key cb]
    (add-watch state key (fn [key _ oldval newval]
                          (if (not= oldval newval)
                            (cb key self oldval newval)))))
  (-remove-watch [_ key]
    (remove-watch state key))

  IReset
  (-reset! [self newval]
    (cljs.core/-reset! state newval))

  ISwap
  (-swap! [self f]
    (swap! state f))

  (-swap! [self f x]
    (swap! state f x))

  (-swap! [self f x y]
    (swap! state f x y))

  (-swap! [self f x y more]
    (swap! state f x y more))

  IStore
  (-init [this]
    (init this))

  (-emit [this type key params]
    (case type
      :read (read this key params)
      :novelty (novelty this key params)
      (throw (ex-info "Unknown event type." {})))))

(def ^:private noop (constantly nil))

(defn store
  "Default store constructor."
  ([state]
   (store state {}))
  ([state {:keys [init read novelty]
           :or {init noop read noop novelty noop}
           :as opts}]
   {:pre [(ifn? read) (ifn? novelty)
          (instance? cljs.core.Atom state)]}
   (let [store (Store. state init read novelty)]
     (-init store)
     store)))

(defn- interpret-result
  [store result]
  (cond
    (p/promise? result)
    (p/then result #(interpret-result store %))

    (fn? result)
    (do
      (swap! store result)
      (p/resolved nil))

    (sequential? result)
    (p/resolved
     (mapv (fn [item]
             (if (fn? item)
               (do (swap! store item)
                   nil)
               item)) result))

    :else
    (p/resolved result)))

(defn- interpret-params
  [store type params]
  (let [numparams (count params)]
    (cond
      (= 0 numparams)
      (throw (ex-info "Wrong arguments" {}))

      (= 1 (count params))
      (let [params (first params)]
        (->> (-emit store type (first params) (second params))
             (interpret-result store)))

      :else
      (let [fun #(-emit store type (first %) (second %))]
        (->> (p/all (mapv fun params))
             (interpret-result store))))))

(defn read
  [store & params]
  (interpret-params store :read params))

(defn novelty
  [store & params]
  (interpret-params store :novelty params))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State management experiments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Local state management
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; The global state transtions should be predecible
  ;; and testable and this way offers to have flexible
  ;; and extensible way to handle state transitions.

  (defmulti state-transition
    (fn [_ [event]] event))

  (defmethod state-transition :index-counters
    [state [_ counters]]
    (letfn [(index-counter [state item]
              (assoc-in [:counters-by-id (:id item)] item))]
      (reduce index-counters state counters)))

  (defmethod state-transition :add-counters
    [state [_ counters]]
    (let [counter-ids (mapv :id counters)]
      (-> (assoc state :counters counter-ids)
          (state-transition [:index-counters data]))))

  ;; Example how to be used in a single operation
  ;; in a "transaction" over state
  (swap! state state-transition [:add-counters foobar])

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Client Server Architecture
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; The client-server architecture is suitable for build
  ;; client-server communications or also client only architectures
  ;; where requires communication with some local databases
  ;; such as datascript, localstorage or indexeddb...
  ;;
  ;; It consists in two main operations: read that consists
  ;; for retrieve data from the "database" and novelty for
  ;; mutate the persistent layer sotorage state.

  (defmulti read-fn (fn [k p] k))
  (defmulti novelty-fn (fn [k p] k))

  ;; The return value of that functions can be plain value or promise
  ;; instance (internally all is coerced to promise for uniform api).
  ;; The value can be any object or function. If function is provided,
  ;; it will be automatically executed in context of the state value
  ;; (just like executing it with swap!).

  ;; This is completly agnostic of the persistence layer so you
  ;; can use whatever you want for access the data. The good examples
  ;; are use funcool/muse and datascript.

  (defmethod read-fn :counters
    [store key params]
    ;; Here you can use any method for obtain date such as using
    ;; traditional api rest, postal, websockets, whatever.
    (m/alet [items (api/get-counters)]
      (swap! store state-transition [:add-counters items])))

  (defmethod novelty-fn :increment
    [_ _ {:keys [id] :as params}]
    #(state-transition % [:increment id]))

  ;; The init function is some kind of entry point of the storage
  ;; and it will be executed as soon as posible. It is responsible
  ;; to populate some initial state.

  (defn init
    [s]
    (m/mlet [p (read store [:counters :useless-param])]
      (println "Initialized")))

  ;; The store should be created with

  (defonce state (atom {}))
  (defonce store
    (st/store state {:init init
                     :read read-fn
                     :novelty novelty-fn}))
  )
