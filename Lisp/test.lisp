(load "ool.lisp")
(defun run (test)
  (if (eval test)
      t
    (eval test)))
(format t "~A~%" 
        (map 'list 'run
                      (list
                       '(def-class 'person nil '(get-fields (name "Eve") (age 21)))
                       '(def-class 'student '(person)
                          '(get-fields
                            (name "Eva Lu Ator")
                            (university "Bicocca" string))
                          '(methods
                            (talk (&optional (out *standard-output*))
				  (format out "My name is ~A~%My age is ~D~%"
					  (get-field this ’name)
					  (get-field this ’age)))))
                       '(equal (defparameter eve (make 'person)) 'eve)
                       '(equal (defparameter adam (make 'person 'name "Adam")) 'adam)
                       '(equal (defparameter age (make 'person 'age 69)) 'age)
                       '(equal (defparameter s1 (make 'student 'name "Eduardo De Filippo" 'age 108)) 's1)
                       '(equal (defparameter s2 (make 'student)) 's2)
                       ;; should fail beacuse "42" is not an integer but it doesn't
                       '(not (defparameter s3 (make 'student 'age "42")))
                       '(equal (get-field age 'age) 69)
                       '(equal (get-field eve 'age) 21)
                       ;; make non sovrascrive i campi correttamente
                       ;; age = 21
                       '(equal (get-field s1 'age) 108)
                       '(equal (get-field s2 'name) "Eva Lu Ator")
                       )))
