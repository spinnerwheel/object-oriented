;;;; -*- Mode: Lisp -*-

;;;; Nicoletta Davide 858101
;;;; Rocca Tommaso 869171

;;; make-hash-table and gethash manipulate hash tables in Common Lisp.
;;;; classes-specs
(defparameter *classes-specs* (make-hash-table))

;;;; add-class-spec 
(defun add-class-spec (name class-spec)
  (setf (gethash name *classes-specs*) class-spec))

;;;; get-class-spec 
(defun get-class-spec (name) 
  (gethash name *classes-specs*))

;;;; def-class/3
;;; defines a class as a global variable
(defun def-class (class-name parents &rest part)
  ;; necessary checks to verify the correctness of the parameters
  (cond ((or (not (atom class-name)) 
             (equal class-name '()) 
             (null class-name) 
             (not (listp parents))
             (is-class class-name))
         (error (format nil "Error: Class-name invalid.")))
        ((not (check-parents parents))
         (error (format nil "Error: Parents invalid."))))
  ;; check to verify that the field values ​​
  ;; are consistent with the assigned type
  (if (check-coerence-type part)
      (add-class-spec class-name 
                      (append (list class-name)
                              (list parents)
                              ;; checks if there are fields or methods in the
                              ;; first part of the parts list
                              (if (equal (car (first part)) 'fields)
                                  (execute-first-field part)
                                  (execute-first-method part))))
      (error (format nil "Error: inconsistent type.")))
  class-name)

;;;; execute-first-field/1
;;; processes the fields contained in the parts list
(defun execute-first-field (part)
  (list (append
         (slot-structure
          (redefine-struc (first part)))
         (slot-structure-methods
          (add-methods-prefix
           (cdr (second part)))))))

;;;; execute-first-method/1
;;; processes the method contained in the parts list
(defun execute-first-method (part)
  (list (append
         (slot-structure
          (redefine-struc  (second part)))
         (slot-structure-methods
          (add-methods-prefix
           (cdr (first part)))))))

;;;; type-of-with-class/1
;;; returns the type of value.
;;; If value is an instance it returns the class
(defun type-of-with-class (value)
  (if (and
       (listp value)
       (equal (car value) 'make)
       (is-instance (make (cadadr value))))
      (cadadr value)
      (type-of value)))

;;;; check-coerence-type /1
;;; returns T if in all fields present in the parts list
;;; the fields argument and declared type are consistent
(defun check-coerence-type (parts)
  (if (equal (car (car parts)) 'fields)
      (subtypep-list-check
       (get-type-define (cdar  parts))
       (get-type-value (cdar parts)))
      (subtypep-list-check
       (get-type-value (cddr  parts))
       (get-type-define (cddr parts)))))

;;;; get-type-define/1
;;; returns the list of field types
;;; initially declared through the def-class function
(defun get-type-define (parts)
  (mapcar #'(lambda (sublist)
              (if (= (length sublist) 3)
                  (third sublist)
                  'T))
          parts))

;;;; get-type-value/1
;;; returns the list of field value types
(defun get-type-value (parts)
  (mapcar #'(lambda (sublist)
              (if (= (length sublist) 3)
                  (type-of-with-class (second sublist))
                  'T))
          parts))

;;;; check-parents/1
;;; verifies that all parents in the parents list exist
(defun check-parents (parents)
  (if parents
      (every #'is-class parents)
      t))

;;;; add-methods-prefix/1
;;; adds the "methods" prefix to each method in the parts list
(defun add-methods-prefix (part)
  (mapcar (lambda (sottolista) (list 'methods sottolista)) part))

;;;; get-method-names:  
;;; given the method list (which contains the methods), returns a list
;;; with the method names
(defun get-method-names (method)
  (cond ((null method) NIL) 
        (T (cons (caar  method) (get-method-names (cdr method))))))

;;;; redefine-struc/1
;;; starting from the parts list redefines the list into a new list
;;; to then be analyzed by the slot-structure function
(defun redefine-struc (parts)
  (apply #'append (check-type-validation parts)))

;;;; check-type-validation/1
;;; if the type is not present in the fields, add T
(defun check-type-validation (parts)
  (if (equal (car parts) 'fields)
      (mapcar (lambda (subparts)
                (if (or (null subparts) (null (cddr subparts)))
                    (append subparts (list T))
                    subparts))
              (cdr parts))
      (mapcar (lambda (subparts)
                (if (or (null subparts) (null (cddr subparts)))
                    (append subparts (list T))
                    subparts))
              parts)))

;;;; slot-structure/1
;;; returns as a return value a list with the values ​​passed
;;; into the slots list following
;;; the OOLINST structure (specific function for fields)
(defun slot-structure (parts)
  (cond ((= (list-length parts) 0) nil)
        ((cons (cons (first parts)
                     (cons
                      (execute-make (second parts)) (third parts)))
               (slot-structure (cdddr parts))))))

;;;; execute-make/1
;;; if one of the fields is an instance, create the instance
(defun execute-make (value)
  (if (and (listp value) (equal (car value) 'make))
      (make (cadadr value))
      value))

;;;; slot-structure-methods/1
;;; returns as a return value a list with the values ​​passed
;;; into the slots list following
;;; the OOLINST structure (specific function for methods)
(defun slot-structure-methods (parts)
  (cond ((= (list-length parts) 0) nil) 
        ((member (caadar  parts)
                 (get-method-names (remove nil (check-method parts))))
         (cons (cons (caadar  parts)
                     (list (process-method
                            (caadar parts)
                            (cdadar parts))))
               (slot-structure-methods (cdr parts))))))

;;;; check-method/1
;;; extracts methods from various past slots
;;; as argument elements and returns them in a cons.
(defun check-method (parts) 
  (cond ((null parts) nil) 
        ((and
          (listp (cadr parts))
          (member 'methods (car parts)))
         (cons (car (cdr (car parts)))
               (cons
                (cadar parts)
                (check-method (cdr parts)))))
        (T (check-method (cdr parts)))))

;;;; process-method/2
;;; takes as input the method name and body
;;; and generates the code needed to create a method.
(defun process-method (method-name method-spec)
  (if (not (and (equal 'nil method-name)
                (equal 'nil method-spec)))
      (setf (fdefinition method-name) 
            (lambda (this &rest args) 
              (apply (field this method-name)
                     (append (list this) args)))))
  (eval (rewrite-method-code method-spec)))

;;;; rewrite-method-code/1
;;; takes the body of the motor as input and rewrites it as a lambda
(defun rewrite-method-code (method-spec)
  (cons 'lambda
        (cons (append (list 'this) (car  method-spec)) 
              (cdr  method-spec))))

;;;; is-class/1
;;; returns T if class-name is a class
(defun is-class (class-name) 
  (if (get-class-spec class-name) T nil))

;;;; get-data/1
;;; extracts the value of the part-name
;;; from the instance passed as parameters.
(defun get-data (instance part-name)
  (cond  
    ((null instance) nil)
    ((atom (car instance)) (get-data (caddr instance) part-name))
    ((and (symbolp (caar instance))
          (equal (symbol-name (caar instance))
                 (symbol-name part-name))
          (listp (cdar instance))
          (equal 'methods (first instance)))
     (caddar instance))
    ((and (symbolp (caar instance))
          (equal (symbol-name (caar instance))
                 (symbol-name  part-name)))
     (second (car instance)))
    (T (get-data (cdr instance) part-name))))

;;;; get-parents
;;; given a class, returns a list with all the parents of that class
(defun get-parents (class) 
  (cond ((null (cadr (get-class-spec class))) nil) 
        ((remove-duplicates 
          (append 
           (append (get-parents (caadr (get-class-spec class)))
                   (get-parents (cdadr (get-class-spec class))))
           (cadr (get-class-spec class)))
          :from-end t))))

;;;; sublist/1
;;; given a list of n elements returns a list of sublists by matching
;;; the elements two by two
(defun sublist (list-elements)
  (if (<= (length list-elements) 1)
      list-elements
      (cons (list (first list-elements) (second list-elements))
            (sublist (nthcdr 2 list-elements)))))

;;;; make/2
;;; crea una nuova istanza di una classe
(defun make (class-name &rest parts)
  ;;checking the existence of the class
  (cond ((not (is-class class-name)))                            
        ((append
          (list 'oolinst)
          (list class-name
                (if (get-class-type-slot
                     class-name
                     (slot-structure
                      (redefine-struc (sublist parts))))
                    (slot-structure
                     (redefine-struc
                      (sublist
                       (check-slot-exists class-name parts)))))
                )))))

;;;; check-slot-exists/2
;;; checks if each slot in the list of slots passed
;;; as arguments are present in the specified class.
;;; If the slots exist, a cons. is returned
;;; containing all valid slots
(defun check-slot-exists (class parts)
  (cond ((null parts) nil) 
        ((get-class-data class (car parts)) 
         (cons (car parts) 
               (cons (cadr parts) (check-slot-exists class (cddr parts)))))
        (T (check-slot-exists class (cddr parts)))))

;;;; get-data-type
;;; given a class it returns the types of the fields.
(defun get-data-type (class field-name)
  (cond 
    ((null class) nil)
    ((atom (car class)) (get-data-type (caddr class) field-name))
    ((and (symbolp (caar class))
          (equal (symbol-name (caar class))
                 (symbol-name  field-name)))
     (cddar  class))
    (T (get-data-type (cdr class) field-name))))

;;;; get-class-type-slot/2
;;; given a class it returns the types of the fields.
;;; If the fields don't exist in that class look in the parents
(defun get-class-type-slot (class field-name)
  (subtypep-list-check
   (mapcar (lambda (element)
             (if (equal nil (get-parents class))
                 (get-data-type
                  (get-class-spec  class)
                  (first element))
                 (get-data-type
                  (get-class-spec
                   (first (second (get-class-spec class))))
                  (first element))))
           field-name)
   (mapcar (lambda (element)
             (if (eq (second element) T)
                 T
                 (type-of (second element))))
           field-name)))

;;;; subtypep-list-check/2
;;; takes as input two lists containing values
;;; of the types. Orignal-t contains the types declared in the class,
;;; while new-t contains the types of the values ​​passed in the creation
;;; of the instance. If the values ​​are equal the function returns T,
;;; otherwise it invokes an error
(defun subtypep-list-check (new-t original-t)
  (cond ((and
          (null original-t)
          (null new-t))
         T)
        ((subtypep-or-class (car new-t)(car original-t))
         (subtypep-list-check (cdr original-t)(cdr new-t)))
        (t (error (format nil "New: ~A~%Original: ~A"
                          (car new-t) (car original-t))))))

;;;; subtypep-or-class/2
;;; verifies that the new and original are consistently of the same type
(defun subtypep-or-class(new original)
  (cond ((equal new 'T) T)
        ((superclass new original) t)
        ((or (subtypep new original)
             (subtypep original new))
         t)
        (T nil)))

;;;; superclass/2
;;; checks that super-class is a parents of class
(defun superclass(class super-class)
  (cond
    ((and (equal class super-class)
          (equal (get-parents class) nil))
     T)
    ((member super-class (get-parents class)) t)
    (T nil)))

;;;; get-class-data/2
;;; extracts the value of the specified slot-name from
;;; desired class. If slot-name is not present in the class,
;;; is searched for in the parents of the class.
;;; If the slot-name is not present in the class or in the parents, the
;;; function reports an error.
(defun get-class-data (class slot-name)
  (cond ((get-data (get-class-spec class) slot-name)) 
        ;; Se la classe non ha lo slotname cerca nei padri 
        ((get-parent-slot (get-parents class) slot-name))
        ((error 
          (format nil 
                  "Error: no method or slot named ~a found." slot-name)))))

;;;; get-parent-slot/2
;;; returns the value of the first slot-name present
;;; in parents classes passed as list. In practice if a slot-name
;;; is not present in one of the parents, goes to look for it and possibly
;;; inherit it from the next class in the parents list.
(defun get-parent-slot (parents slot-name) 
  (cond ((null parents) nil) 
        ((null (get-data (get-class-spec (car parents)) slot-name)) 
         (get-parent-slot (cdr parents) slot-name))
        ((get-data (get-class-spec (car parents)) slot-name))))

;;;; is-instance/2
;;; returns T if an instance of a class is passed as an object
(defun is-instance (value &optional (class-name T)) 
  (cond ((not (listp value)) nil)
        ((and (equal (car value) 'OOLINST)
              (equal class-name 'T)) T) 
        ((equal (cadr value) class-name) T) 
        ((member class-name (cadr (get-class-spec (cadr value)))) T)))

;;;; field/2
;;; extracts the value of a field from a class.
(defun field (instance slot-name)
  ;; If the instance has no slotname, check its class 
  (cond ((get-data instance slot-name)) 
        ;; If the class does not have the slotname it searches the parents
        ((get-data (get-class-spec (cadr instance)) slot-name))
        ((get-parent-slot (get-parents (cadr instance)) slot-name))
        ((error 
          (format nil 
                  "Error: no method or slot named ~a found." slot-name)))))

;;;; field*/2
;;; extracts the value from a class by going through a chain of attributes.
;;; The result is the value associated with the last element of slot-name
;;; in the last instance.
(defun field* (instance &rest slot-name)
  (cond 
    ((null (is-instance (field instance (if (listp (car slot-name))
                                            (caar slot-name) (car slot-name)))))
     (error "Error, it's not a instance"))
    ((eq (length slot-name) 1)
     (field instance (if (listp (car slot-name))
                         (caar slot-name) (car slot-name))))
    (T (field* (field instance (if (listp (car slot-name))
                                   (caar slot-name) (car slot-name)))
               (cdr slot-name)))))

;;; end of file -- ool.lisp
