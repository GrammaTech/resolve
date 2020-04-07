(defpackage :resolve/routes/diff
  (:use :gt/full :resolve/commands)
  (:import-from :snooze :defroute))
(in-package :resolve/routes/diff)

(defroute diff (:post :application/json)
  (with-output-to-string (*standard-output*)
    (rest-diff :json t)))

(defroute diff (:post :text/plain)
  (with-output-to-string (*standard-output*)
    (rest-diff)
    (format t "~&")))

(defroute show (:get :text/html hash)
  (render-json-diff-to-html (lookup-json-diff-hash (symbol-name hash))))

(defroute diff (:post :text/html)
  (render-json-diff-to-html (with-output-to-string (*standard-output*)
                              (rest-diff :json t))))
