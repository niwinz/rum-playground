(ns rum-experiments.state)

(defprotocol IResponseHandler
  (-dd [] ))

(defprotocol IStore
  (-transact []))

(deftype Store [data read novelty]
  cljs.core/IDeref
  (-deref [_] @data)

  cljs.core/IWatchable
  (-add-watch [self key cb]
    (add-watch data key (fn [key _ oldval newval]
                          (if (not= oldval newval)
                            (cb key self oldval newval)))))
  (-remove-watch [_ key]
    (remove-watch data key))

  IStore
  (-emit [_ k params]
    (let [res (read k params)]
      )))

(defn store
  [{:keys [state read novelty] :as opts}]
  {:pre [(fn? read) (fn? novelty)
         (instance cljs.core.Atom state)]}
  (Store. state read novelty))

(defn read
  ([store k]
   (read store k nil))
  ([store k params]
   (-read store k params)))

(comment
  (defmulti read-fn (fn [k p] k))
  (defmulti novelty-fn (fn [k p] k))

  ;; The return value
  (defmethod read-fn :user-by-id
    [_ {:keys [id] :as params}]
    (m/alet [user (api/get-user id)
             friends (api/get-user-friends id)]
      (let [user (assoc user :friends friends)]
        (m/return
         {:value user
          :action
       (fn [state]
         (let [user (assoc user :friends friends)]
           (assoc state :user user))))))))

  (def state (atom {}))

  (def store
    (s/store {:state state
              ;; :init init-fn
              :read read-fn
              :novelty novelty-fn}))

  ;; RUM component functions

  (defn will-mount
    [slocal]
    (read state :user-by-id {:id 1})
    slocal)

  (defn render
    [slocal]
    )

  (def root-component
    (util/component {:render render
                     :will-mount will-mount}))


  (rum/mount (root-component) ...)
  )

