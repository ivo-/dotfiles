(ns lt.init
  (:require [lt.object :as object]
            [lt.objs.context :as ctx]
            [lt.util.load :as load]
            [lt.objs.editor.pool :as pool]
            [lt.objs.files :as files]
            [lt.objs.popup :as popup]
            [fetch.core :as fetch]
            [lt.objs.notifos :as notifos]
            [lt.objs.sidebar.command :as cmd :refer [command]]
            [lt.objs.editor :as editor])
  (:require-macros [lt.macros :refer [behavior]]))

(comment

  (notifos/set-msg! "I will stay 3s" {:timeout 3000})

)

(comment

  (defn current-file []
    (-> @(pool/last-active) :info :path))

  (current-file)

  (defn rename-current-file [to]
    (files/move! (current-file) to))

  (defn delete-current-file []
    (files/delete! (current-file)))

)

(comment

  (popup/popup! {:header "Header"
                 :body (str "Body")
                 :buttons [{:label "ok"}]})

)

(comment

  (fetch/xhr "http://localhost:8000/" {}
             (fn [data]
               (notifos/set-msg! (pr-str data) {:timeout 3000})
               ))

)


(comment

  (def request (load/node-module "request"))
  (request "http://translate.google.com/translate_a/t?client=p&ie=UTF-8&oe=UTF-8&sl=en&tl=bg&text=hello"
           (fn [e r b]
             (.log js/console b)))

)
