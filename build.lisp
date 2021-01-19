(load "~/quicklisp/setup.lisp")
(ql:quickload '(:gt
		:diff
		:cl-strftime
		:jsown
		:cl-interpol
                :cl-json
                :babel
                :software-evolution-library
                :resolve))

(asdf:initialize-output-translations
 (list :output-translations :ignore-inherited-configuration
       (list (merge-pathnames "**/*.*" #+ccl (ccl::current-directory))
             (merge-pathnames "build/**/*.*" #+ccl (ccl::current-directory)))))
(asdf:initialize-source-registry
 (list :source-registry :ignore-inherited-configuration
       (list :directory (merge-pathnames "src/" #+ccl (ccl::current-directory)))
       (list :tree (merge-pathnames "quicklisp/dists/" #+ccl (ccl::current-directory)))))
(asdf:operate :deliver-asd-op :resolve/ast-diff)
