(ql:quickload "fiveam")
(load "ool.lisp")

(5am:def-suite my-system)
(5am:def-suite* def-class-tests :in my-system)

(5am:test def-class-tests
  (5am:is (def-class 'person nil '(fields (name "person.name") (age 21))))
  (5am:is (def-class 'student '(person)
             '(fields
                (name "student.name")
                (university "student.university" string))
             '(methods
                (talk ()
                      (values (field this ’name)
                              (field this ’age))))))
  (5am:is (def-class 'object nil '(fields (name "object.name"))))
  (5am:is (def-class 's-bicocca '(student)
                     '(methods (talk () (values "s-bicocca.talk")))
                     '(fields (university "s-bicocca.university"))))
  (5am:is (equal (field 's-bicocca 'university) "s-bicocca.university"))
  (5am:is (equal (field 'student 'name) "student.name"))
  (5am:is (eql (length (fields 'person)) 2))
  (5am:is (eql (length (fields 'student)) 3))
  (5am:is (eql (length (fields 's-bicocca)) 3))
  (5am:is (equal (make 'person)
                 '(oolinst person)))
  (5am:is (equal (make 'person 'name "newname")
                 '(oolinst person ((name "newname")))))
  )

(5am:test field-tests
 (5am:is (equal (field (make 'person)
                       'name) "person.name"))
 (5am:is (equal (field (make 'person)
                       'age) 21))
 (5am:is (equal (field (make 'person 'name "stost" 'age 200)
                       'name) "stost"))
 (5am:is (equal (field (make 'person 'name "stost" 'age 200)
                       'age) 200))
 )

(5am:test make-fail-cases
 (5am:is-false (make 'person 'name 1))
 (5am:is-false (def-class 'c1 '(fields (name "string" integer))))
 )


