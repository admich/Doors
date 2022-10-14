(in-package :cl-user)

(defpackage :doors-main (:use :cl) (:export :main))
(in-package :doors-main)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (net.didierverna.clon:nickname-package))

(clon:defsynopsis ()
  (text :contents
	"Start Doors a CLIM window manager")
  (group (:header "Options:")
    (flag :short-name "h" :long-name "help"
	      :description "Print this help and exit.")
    (flag :short-name "r" :long-name "replace"
	      :description "Replace running window manager.")
    (flag :short-name "q" :long-name "no-config"
	      :description "Do not load the configuration file.")
    (path :short-name "c" :long-name "config-file"
	    :description "Load Doors configuration from FILE."
	    :argument-name "FILE"
	    :type :file
	    :default-value doors:*config-file*)))

(defun main ()
  "Entry point for the doors window manager application."
  (clon:make-context)
  (cond ((clon:getopt :short-name "h")
         (clon:help))
        (t
         (clim-debugger:install-debugger)
         (doors:doors :new-process t
                      :replace-wm (clon:getopt :short-name "r")
                      :config-file (and (not (clon:getopt :short-name "q"))
                                        (clon:getopt :short-name "c")))
         (clim-doors::emergency-loop)))
  (uiop:quit))
