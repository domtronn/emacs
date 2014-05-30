;;; Compiled snippets and support files for `cpp-omnet-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'cpp-omnet-mode
										 '(("ev" "EV << \"${1:string}\"$0;" "EV" nil nil nil nil nil nil)
											 ("emit" "emit(${1:signal_id}, ${2:long});" "emit_signal" nil nil nil nil nil nil)
											 ("intuni" "intuniform(${1:0}, ${2:1})" "intuniform" nil nil nil nil nil nil)
											 ("math" "#include <cmath>" "math" nil nil nil nil nil nil)
											 ("nan" "isnan(${1:x})" "nan" nil nil nil nil nil nil)
											 ("omnet" "#include <omnetpp.h>" "omnet" nil nil nil nil nil nil)
											 ("par" "${1:var} = par(\"${2:par}\");" "parameter_omnetpp" nil nil nil nil nil nil)
											 ("sched" "scheduleAt(simTime()+${1:1.0}, ${2:event});" "scheduleAt" nil nil nil nil nil nil)
											 ("uni" "uniform(${1:0}, ${2:1})" "uniform" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Fri May 30 09:30:39 2014
