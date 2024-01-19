;;;; -*- Mode: Lisp -*-

(defparameter *classes-specs* (make-hash-table))

;; add-class-spec
;; Add a class to the global variable *classes-spec*
(defun add-class-spec (name class-spec)
  (setf (gethash name *classes-specs*) class-spec))

(defun class-spec (name)
  (gethash name *classes-specs*))


(defun param-error (param should-be instead-is)
  (error
   (format nil "~A should be ~A, not ~A."
           param should-be instead-is)))

(defun field-value (field)
  "Return the value of given field."
  (second field))

(defun field-type (field)
  "Return the type of given field."
  (third field))

;; PERF could be much improved implementing it like %anchestors
(defun subclassp (class superclass)
  "True if CLASS is a subclass of SUPERCLASS"
  (if (member superclass (anchestors class))
      T
    NIL))


(defun subtypep-with-class (type1 type2)
  "True if TYPE1 is a valid subtype or subclass of TYPE2"
  (cond ((subtypep type1 type2) T)
        ((subclassp type1 type2) T)
        (T NIL)))

;; valid-value-for-type
;; TODO add instance-class case
(defun valid-value-for-type (value type)
  "Verify value is a valid subtype of type"
  (subtypep-with-class (type-of value) type))

(defun field2-p (field)
  (and (atom (car field))
       (append field '(t))))

(defun field3-p (field)
  (and (atom (car field))
       (valid-value-for-type (field-value field) (field-type field))
       field))

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

;; field
;; Given a class and a field-name returns it's value (if exist)
;; TODO add instance case
(defun field (class field-name)
  (field-value 
   (assoc field-name (fields class))))

;; def-class
(defun def-class (name parents &rest parts)
  "Define a new class."
  (cond ((or (not (atom name))
             (null name))
         (param-error 'name 'atom (type-of name)))
        ((is-class name)
         (error(format nil "Class of name ~A has already been defined." name)))
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

;; make

;; is-instance

;; field

;; field*
