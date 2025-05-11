(in-package :aero-fighter)

#+nil
(launch)

#++
(declaim (optimize (speed 1) (debug 3) (safety 1)))
#++
(flamegraph:save-flame-graph ("sbcl.fg")
  (launch))

;; --- Dependency Graph --- ;;

#++
(defun string->keyword (s)
  (intern (string-upcase s) "KEYWORD"))

(defun all-deps (graph system)
  (when (stringp system)
    (simple-graph:add-node graph system)
    (t:transduce (t:map (lambda (dep)
                          (when (and (stringp dep)
                                     (simple-graph:add-node graph dep))
                            (simple-graph:add-edge graph system dep)
                            (all-deps graph dep))))
                 #'t:for-each
                 (slot-value (asdf:find-system system)
                             'asdf/system::depends-on))))

#++
(let ((graph (simple-graph:make-graph)))
  (all-deps graph "aero-fighter")
  (with-open-file (stream #p"deps.dot" :direction :output :if-exists :supersede)
    (format stream (simple-graph:to-dot graph))))

;; --- Benchmarking --- ;;

#+sbcl
(require :sb-sprof)

#+nil
(sb-sprof:with-profiling (:max-samples 100000 :sample-interval 0.0001 :report :graph :mode :alloc)
  (launch))
