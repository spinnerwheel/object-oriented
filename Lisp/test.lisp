(load "ool.lisp")
(defun run (test)
  (if (eval test)
      t
    (eval test)))
(format t "~A~%" 
        (map 'list 'run
                      (list
                       '(def-class 'person nil '(fields (name "Eve") (age 21)))
                       '(def-class 'student '(person)
                          '(fields
                            (name "Eva Lu Ator")
                            (university "Bicocca" string))
                          '(methods
                            (talk (&optional (out *standard-output*))
				  (format out "My name is ~A~%My age is ~D~%"
					  (field this ’name)
					  (field this ’age)))))
                       '(equal (defparameter eve (make 'person)) 'eve)
                       '(equal (defparameter adam (make 'person 'name "Adam")) 'adam)
                       '(equal (defparameter age (make 'person 'age 69)) 'age)
                       '(equal (defparameter s1 (make 'student 'name "Eduardo De Filippo" 'age 108)) 's1)
                       '(equal (defparameter s2 (make 'student)) 's2)
                       ;; should fail beacuse "42" is not an integer but it doesn't
                       '(not (defparameter s3 (make 'student 'age "42")))
                       '(equal (field age 'age) 69)
                       '(equal (field eve 'age) 21)
                       ;; make non sovrascrive i campi correttamente
                       ;; age = 21
                       '(equal (field s1 'age) 108)
                       '(equal (field s2 'name) "Eva Lu Ator")
                       '(equal (talk s1) T)
                       '(equal (talk s2) T)
                       )))
(def-class 'c1 nil '(fields (name "c1.name")))
(def-class 'c2 nil '(fields (name "c2.name")))
(def-class 'c3 '(c1 c2) '(fields (name "c3.name")))

