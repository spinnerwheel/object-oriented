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

;; valid-value-for-type
;; TODO add instance-class case
(defun valid-value-for-type (value type)
  "Verify value is a valid subtype of type"
  (subtypep (type-of value) type))

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
;; Verify if parts are valid parts are returns them splitted
;; into two lists.
(defun parts-p (parts)
  (cond ((equal (caar parts) 'fields)
         (append (fields-p (cdar parts))
                 (parts-p (cdr parts))))
        ((equal (caar parts) 'methods)
         (append (parts-p (cdr parts))
                 (methods-p (cdar parts))))
        (t nil)))

;; is-class
;; Verify if class exist in *classes-specs*
(defun is-class (class)
  (cond ((null class)
         (error "This method should be used to check NULL objects;
                Use IS-CLASS-OR-NULL instead."))
        ((class-spec class) class)
        (t nil)))

(defun is-class-or-null (class)
  (cond ((null class) nil)
        ((class-spec class) class)
        (t (error (format nil "~A is not a valid class." class)))))

;; parents-p
;; TODO uniform the return type (value and true, only true, nil ...)
(defun parents-p (parents)
  "Return parents if parents are valid classes, error otherwise."
  (cond ((atom parents) (is-class-or-null parents))
        ((listp parents) (mapcar 'is-class-or-null parents))
        (t (param-error 'parents '(atom list) (type-of parents)))))

;; Returns parents of class
(defun parents (class)
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

;; fields
(defun class-fields (class)
  "Returns the stricly defined fields of class."
  (fourth (class-spec class)))

(defun inherited-fields (class)
  "Returns the inherited fields of class."
  (mapcan 'class-fields (anchestors class)))

(defun fields (class)
  (append (class-fields class)
          (inherited-fields class)))
;; field
;; Given a class and a field-name returns it's value (if exist)
;; TODO add instance case
(defun field (class field-name)
    (field-value (assoc field-name (fields class))))

;; def-class
(defun def-class (name parents &rest parts)
  "Define a new class."
  (cond ((not (atom name))
         (param-error 'name 'atom (type-of name)))
        ((null name)
         (error "name shouldn't be NULL."))
        ((is-class name)
         (error 
           (format nil "Class of name ~A has already been defined." name)))
        ((not (listp parents))
         (param-error 'parents 'list (type-of parents)))
        ((not (listp parts))
         (param-error 'parts 'list (type-of parts)))
        ((add-class-spec
             name
             (list 'class
                   name
                   (parents-p parents)  ; error handling is left to the method
                   (parts-p parts)))  ; error handling is left to the method
        name)
        (t (error "def-class: Something unexpected happend."))))

;; make

;; is-instance

;; field

;; field*


(def-class 'person nil '(fields (name "jon" string)))
(def-class 'record nil '(fields (id 0 integer)))
(def-class 'student '(person record) '(fields (uni "uni" string)))
(inherited-fields 'student)
