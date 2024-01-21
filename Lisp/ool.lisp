;;;; -*- Mode: Lisp -*-

(defparameter *classes-specs* (make-hash-table))

;; add-class-spec
;; Add a class to the global variable *classes-spec*
(defun add-class-spec (name class-spec)
  (setf (gethash name *classes-specs*) class-spec))

(defun class-spec (name)
  (gethash name *classes-specs*))

;; TODO could be rewrited to use 'type-error
(defun param-error (param should-be instead-is)
  (error "~A should be ~A, not ~A."
         param should-be instead-is))

(defun field-value (field)
  "Return the value of given field."
  (second field))

(defun field-type (field)
  "Return the type of given field."
  (third field))

;; PERF could be much improved implementing it like %anchestors
(defun subclassp (class superclass)
  "True if CLASS is a subclass of SUPERCLASS"
  (if (or (equal class superclass)
          (member superclass (anchestors class)))
      T
    NIL))


;; valid-value-for-type
(defun valid-value-for-type (value type)
  "Verify value is a valid subtype of type"
  (print value)
  (print type)
  (if (and (listp value)
           (equal (first value)
                  'OOLINST))
      (subclassp (second value) type)
  (subtypep (type-of value) type)))

;; FIXME duplicate code, could be improved
(defun field2-p (field)
  "True if field is a field with NAME and VALUE."
  (cond ((not (atom (car field)))
         (param-error 'field-name 'atom (type-of (car field))))
        (T (append field '(t)))))

(defun field3-p (field)
  "True if field is a field with NAME, VALUE and TYPE."
  (cond ((not (atom (car field)))
         (param-error 'field-name 'atom (type-of (car field))))
        ((valid-value-for-type (field-value field) (field-type field))
         field)
        (T (error "Invalid value ~w for type ~w~%"
                   (field-value field)
                   (field-type field)))))

(defun field-p (field)
  (if (eql (length field) 2)
      (field2-p field)
    (field3-p field)))

(defun fields-p (fields)
  (map 'list 'field-p fields))

;; TODO method-p
(defun method-p (method)
  method)

;; TODO methods
(defun methods-p (methods)
  (map 'list 'method-p methods))

;; parts-p
;; FIXME
(defun parts-p (parts)
  "Verify if parts are valid parts are returns
  them splitted into two lists"
  (cond ((null parts) nil)
        ((equal (caar parts) 'fields)
         (values (fields-p (cdar parts))
                 (parts-p (cdr parts))))
        ((equal (caar parts) 'methods)
         (values (parts-p (cdr parts))
                 (methods-p (cdar parts))))
        (t nil)))

(defun is-class (class)
  "Verify if class exist in *classes-specs*"
  (cond ((null class)
         (error "This method should be used to check NULL objects;
                Use IS-CLASS-OR-NULL instead."))
        ((class-spec class) class)
        (t nil)))

;; parents-p
;; TODO uniform the return type (value and true, only true, nil ...)
(defun parents-p (parents)
  "Return parents if PARENTS are valid classes, error otherwise."
  (cond ((null parents) NIL)
        ((is-class (car parents))
         (parents-p (cdr parents)))
        (t (param-error 'parents 'list (type-of parents)))))

(defun parents (class)
  "Returns parents of CLASS"
  (third (class-spec class)))

;; anchestors
;; Returns the list of anchestors
;; PERF some improvements could be done to speed it up
(defun %anchestors (queue sofar)
  "Returns all the anchestors of the classes in QUEUE, without duplicates"
  (let ((class (car queue))
        (classes (cdr queue))
        (class-parents (parents (car queue))))
    (cond ((null queue) sofar)
          ((subsetp (list class)
                    sofar)
           (%anchestors (append classes
				class-parents)
			sofar))
          (t (%anchestors (append classes
				  class-parents)
			  (append sofar (list class)))))))

(defun anchestors (class)
  "Wrapper function for (anchestors queue sofar)"
  (%anchestors (parents class) nil))

(defun class-fields (class)
  "Returns the stricly defined fields of class."
  (fourth (class-spec class)))

(defun inherited-fields (class)
  "Returns the inherited fields of class."
  (apply #'append (map 'list 'class-fields (anchestors class))))

(defun fields (class)
  "Return the fields of class."
  (remove-duplicates
   (reverse
    (append (class-fields class)
            (inherited-fields class)))
   :key #'car))

(defun %entire-field (object field-name)
  (assoc field-name (fields object)))

;; field
;; Given a class and a field-name returns it's value (if exist)
;; TODO add instance case
(defun field (object field-name)
  (field-value 
   (assoc field-name (fields object))))

;; def-class
(defun def-class (name parents &rest parts)
  "Define a new class."
  (cond ((or (not (atom name))
             (null name))
         (param-error 'name 'atom (type-of name)))
        ((is-class name)
         (error "Class of name ~A has already been defined." name))
        ((not (listp parents))
         (param-error 'parents 'list (type-of parents)))
        ((not (listp parts))
         (param-error 'parts 'list (type-of parts)))
        ((add-class-spec
          name
          (append
            (list 'class name)
            (list (parents-p parents))  ; error handling is left to the method
            (list (parts-p parts))))  ; error handling is left to the method
         name)
        (t (error "def-class: something unexpected happend."))))

(defun redefine-field (name new-value type class)
  (cond ((not (field class name))
         (error "~w is not a valid field name for ~A."
          name class))
        ((not (valid-value-for-type new-value type))
         (error
           "~w is not a valid value for field ~A of class ~A, should be ~A."
           new-value name class type))
        (T (list (list name new-value type)))))


(defun process-arguments (arguments class sofar)
  (if (null arguments)
      sofar
      (process-arguments
        (nthcdr 2 arguments)
        class
        (append sofar
                (redefine-field (first arguments)
                                (second arguments)
                                (field-type
                                  (%entire-field class (first arguments)))
                                class)))))


;; make
(defun make (class &rest arguments)
  (cond ((not (is-class class))
         (param-error 'class 'CLASS (type-of class)))
        (t (append (list 'OOLINST class)
                   (process-arguments arguments class nil)))))

;; is-instance

;; field*

(def-class 'person nil '(fields (name "person.name" string) (age 21)))
(def-class 
  'student 
  '(person)
  '(fields (friend '(oolinst person ((name "frank"))) person)))
(process-arguments '(name "s" age 21) 'person nil)
(make 'person 'name 20)
