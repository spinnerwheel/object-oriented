;;;; -*- Mode: Lisp -*-


;;;; classes-specs !
(defparameter *classes-specs* (make-hash-table))

;;;; add-class-spec !
(defun add-class-spec (name class-spec) 
  (setf (gethash name *classes-specs*) class-spec))

;;;; get-class-spec !
(defun get-class-spec (name) 
  (gethash name *classes-specs*))


(defun stampa-oggetto (oggetto)
  (print oggetto))


;;;; def-class !
;;; definisce una classe come variabile globale
(defun def-class (class-name parents &rest part)
  (cond ((or (not (atom class-name)) 
             (equal class-name '()) 
             (null class-name) 
             (not (listp parents))
	     (is-class class-name)
	     ) 
         (error (format nil "Error: Class-name invalid.")))
	((not (check-parents parents))
         (error (format nil "Error: Parents invalid."))))
  (add-class-spec class-name 
		  (append (list class-name)
			  (list parents)
			  (list (append
				 (slot-structure
				  (redefine-struc (first part)))
				 (slot-structure (second part))))))
  class-name)


(defun check-parents (lista)
  (if lista
      (every #'is-class lista)
      t))


;;;; get-method-names: ! 
;;; data una lista di metodi, restituisce un lista con i nomi dei metodi
(defun get-method-names (method)
  (cond ((null method) NIL) 
        (T (cons (car method) (get-method-names (cddr method))))))


(defun redefine-struc (parts)
  (apply #'append (check-third-element (check-ty parts))))


;;;; check-ty: se non è presente il type nei field aggiunge T
(defun check-ty (lista)
  (mapcar (lambda (sottolista)
            (if (or (null sottolista) (null (cddr sottolista)))
                (append sottolista (list T))
                sottolista))
          (cdr lista)))



(defun redefine-struc-reduce (parts)	        
  (apply #'append  (check-third-element (check-ty-reduce parts))))




(defun check-ty-reduce (lista)
  (mapcar (lambda (sottolista)
            (if (or (null sottolista) (null (cddr sottolista)))
                (append sottolista (list T))
                sottolista))
           lista))



(defun check-ty-reload (lista)
  ;(stampa-oggetto "Lista in check-ty")
  ;(stampa-oggetto lista)
  (mapcar (lambda (sottolista)
            (if (or (null sottolista) (null (cddr sottolista)))
                (append sottolista (list T))
                sottolista))
          lista))



;;;controlla se il terzo elemento appartiene ai tipi base
(defun check-third-element (list)
  (mapcar (lambda (sottolista)
	    (if (not (or (equal '(integer) (cddr sottolista))
			(equal '(T) (cddr sottolista))
			(equal '(string) (cddr sottolista))
			(equal '(real) (cddr sottolista))
			(is-class (cddr sottolista))))
		(error "Il terzo elemento non è un tipo base di Lisp")
		sottolista))
	  list))


(defun slot-structure (slots)
  ;(stampa-oggetto "Slot-structure slots")
  ;(stampa-oggetto slots)
  (cond ((= (list-length slots) 0) nil) 
        ((member (car slots) (get-method-names (check-method slots))) 
         (cons (cons (car (car (cdr slots)))
                     (list (process-method (car (car (cdr slots))) (cdr (car (cdr slots))))))
               (slot-structure (cdr (cdr slots))))) 
        ((cons (cons (first slots) (cons (second slots) (third slots))) 
		 (slot-structure (cdr (cdr (cdr slots)))))
	 )))




(defun slot-structure-redefinition (slots)
  ;(stampa-oggetto "Slot-structure-reduce slots")
  ;(stampa-oggetto slots)
  (cond ((= (list-length slots) 0) nil) 
        ((member (car slots) (get-method-names (check-method slots))) 
         (cons (cons (car slots) 
                     (list  (process-method (car (car (cdr slots))) (cdr (car (cdr slots))))))
               (slot-structure (cdr (cdr slots))))) 
        ((cons (cons  (car slots) (car (cdr slots))) 
               (slot-structure (cdr (cdr slots)))))))
    

;;;; process-method: !
;;; genera il codice necessaria per creare un metodo.
(defun process-method (method-name method-spec)
  ;(stampa-oggetto "Proces-method method-name")
  ;(stampa-oggetto method-name)
  ;(stampa-oggetto "Proces-method method-spec")
  ;(stampa-oggetto method-spec)
  (setf (fdefinition method-name) 
        (lambda (this &rest args) 
          (apply (field this method-name) (append (list this) args)))) 
  (eval (rewrite-method-code method-name method-spec)))



;;;; rewrite-method-code: !
;;; riscrive il metodo come una lambda
(defun rewrite-method-code (method-name method-spec)
  ;(stampa-oggetto "Method-name")
  ;(stampa-oggetto method-name)
  ;(stampa-oggetto " ")
  ;(stampa-oggetto "Method-spec")
  ;(stampa-oggetto method-spec)
  ;; Riscrive il metodo come una funzione lambda 
  (cons 'lambda
        (cons (append (list 'this) (car  method-spec)) 
              (cdr  method-spec))))


;;; check-method: !
;;; estrae i metodi dai parts passati li restituisce in una cons.
(defun check-method (slots) 
  ;(stampa-oggetto "Check-ethod")
  ;(stampa-oggetto slots)
  ;;Estrae i metodi dagli slots 
  (cond ((null slots) nil) 
        ((and
	  (listp (cadr slots))
	  (member 'methods  slots))
         (cons (car slots) 
               (cons (cadr slots) (check-method (cdr (cdr slots)))))) 
        (T (check-method (cdr slots)))))



;;;; is-class !
;;; ritorna T se class-name è il nome di una classe
(defun is-class (class-name) 
  (if (get-class-spec class-name) T nil);(error ""Class-name not found!"))
  )


;;;; get-field
(defun get-field (instance slot-name) 
  ;; Se l'instanza non ha lo slotname, vedi la sua classe 
  (cond ((get-data instance slot-name)) 
        ;; Se la classe non ha lo slotname cerca nei padri 
        ((get-data (get-class-spec (cadr instance)) slot-name))
        ((get-parent-slot (get-parents (cadr instance)) slot-name))
        ((error 
          (format nil 
                  "Error: no method or slot named ~a found." slot-name)))))


;;;; get-data: !
(defun get-data (instance slot-name)
  ;(stampa-oggetto "Get-data Instance: ")
  ;(stampa-oggetto instance)
  ;(stampa-oggetto "Get-data Slot-name: ")
  ;(stampa-oggetto slot-name)
  (cond 
    ;; Caso base 
    ((null instance) nil)
    ;; Se è un atom 
    ((atom (car instance)) (get-data (caddr instance) slot-name))
    ;; Se è un metodo 
    ((and (symbolp (caar instance)) 
          (equal (intern (symbol-name (caar instance)) "KEYWORD") 
                 (intern (symbol-name slot-name) "KEYWORD")) 
          (listp (cdar instance)) 
          (equal 'methods (first instance)))
     (caddar instance))
    ;; Se è un attributo 
    ((and (symbolp (caar instance)) 
          (equal (intern (symbol-name (caar instance)) "KEYWORD") 
                 (intern (symbol-name  slot-name) "KEYWORD"))) 
     ;; Se è nil ma esistente 
     (if (null (cdar instance)) "undefined" (second (car instance)))) 
    ;; Altrimenti 
    (T (get-data (cdr instance) slot-name))))




;;;; get-parents
(defun get-parents (class) 
  (cond ((null (cadr (get-class-spec class))) nil) 
        ((remove-duplicates 
          (append 
           (append (get-parents (car (cadr (get-class-spec class)))) 
                   (get-parents (cdr (cadr (get-class-spec class))))) 
           (cadr (get-class-spec class))) :from-end t))))



(defun sublist (lista)
  (if (<= (length lista) 1)
      lista
      (cons (list (first lista) (second lista))
            (sublist (nthcdr 2 lista)))))

;;; make: crea una nuova istanza di una classe. !
(defun make (class-name &rest slot)
					;(stampa-oggetto "Slots in make")
					;(stampa-oggetto slot)
  ;; Non instanzio metodi non esistenti nella classe 
  (cond ((not (is-class class-name)))                            
        ((append (list 'oolinst) 
                 (list class-name
		       (if (get-class-type-slot class-name (slot-structure (redefine-struc-reduce (sublist slot))))
			   (slot-structure
                            (redefine-struc-reduce (sublist (check-slot-exists class-name slot)))))
		       )))))


;;; check-slot-exists: controlla se ogni slot nella lista di slots passata !
;;; come argomento sono presenti nella class specificata.
;;; Se gli slots esistono viene restituita una cons contenente tutti gli
;;; slots validi, altrimenti la funzione segnala un errore
(defun check-slot-exists (class slots)
  (stampa-oggetto "Class in checkslot-exist")
  (stampa-oggetto class)
  (stampa-oggetto "Slots in checkslot-exist")
  (stampa-oggetto slots)
  (cond ((null slots) nil) 
        ((get-class-data class (car slots)) 
         (cons (car slots) 
               (cons (cadr slots) (check-slot-exists class (cddr slots)))))
        (T (check-slot-exists class (cddr slots)))))



(defun get-data-type (instance slot-name)
  (stampa-oggetto "Get-data Instance: ")
  (stampa-oggetto instance)
  (stampa-oggetto "Get-data Slot-name: ")
  (stampa-oggetto slot-name)
  (cond 
    ;; Caso base 
    ((null instance) nil)
    ;; Se è un atom 
    ((atom (car instance)) (get-data-type (caddr instance) slot-name))
    ;; Se è un metodo 
    ;; Se è un attributo 
    ((and (symbolp (caar instance)) 
          (equal (intern (symbol-name (caar instance)) "KEYWORD") 
                 (intern (symbol-name  slot-name) "KEYWORD"))) 
     ;; Se è nil ma esistente 
     (if (null (cdar instance)) "undefined" (cdr (cdr (car instance)))))
    ;; Altrimenti 
    (T (get-data-type (cdr instance) slot-name))))




(defun get-class-type-slot (class slot-name)
  (subtypep-list-check
   (mapcar (lambda (element)
	     (get-data-type
	      (get-class-spec (first (second (get-class-spec class))))
	      (first element)))
	   slot-name)
   (mapcar (lambda (x)
            (if (eq (second x) T)
                T
                (type-of (second x))))
          slot-name)
   )
  )
"""
(mapcar (lambda (sublist)
            (let ((second-element (second sublist)))
              (if (eq second-element T)
                  (cons (car sublist) T)
                  (cons (car sublist) (type-of second-element)))))
          lst)
"""

;;; far passar T come corretto

(defun subtypep-list-check (original-t new-t)
  (stampa-oggetto "Subtypep-list-check original-t: ")
  (stampa-oggetto original-t)
  (stampa-oggetto "Subtypep-list-check new-t: ")
  (stampa-oggetto  new-t)
  (if (equal (length original-t) (length new-t))
      (if (every #'subtypep (mapcar #'list new-t)(mapcar #'list original-t))
          t
          (error "invalid type"))
      (error "invalid type")))

;;; get-class-data: estrae il valore dello slot-name specificato dalla !
;;; classe desiderata. Se slot-name non è presente nella classe,
;;; viene cercato nei parents della classe.
;;; Se non è presente lo slot-name nella classe o nei parents, la
;;; funzione segnala un errore.
(defun get-class-data (class slot-name)
  (cond ((get-data (get-class-spec class) slot-name)) 
        ;; Se la classe non ha lo slotname cerca nei padri 
        ((get-parent-slot (get-parents class) slot-name))
        ((error 
          (format nil 
                  "Error: no method or slot named ~a found." slot-name)))))



;;; get-parent-slot: restituisce il valore del primo slot-name presente !
;;; nelle classi parents passate come lista. In pratica se uno slot-name
;;; non è¨ presente in uno dei parents, va a cercarlo ed eventualmente
;;; ereditarlo dalla prossima classe della lista parents.
(defun get-parent-slot (parents slot-name) 
  (cond ((null parents) nil) 
        ((null (get-data (get-class-spec (car parents)) slot-name)) 
         (get-parent-slot (cdr parents) slot-name))
        ((get-data (get-class-spec (car parents)) slot-name))))



;;; is-instance: !
;;; ritorna T se viene passato come oggetto l'istanza di una classe
(defun is-instance (value &optional (class-name T)) 
  (cond ((and (equal (car value) 'OOLINST) 
              (equal class-name 'T)) T) 
        ((equal (cadr value) class-name) T) 
        ;; Ereditarietà
        ((member class-name (cadr (get-class-spec (cadr value)))) T)))



;;; field: estrae il valore di un campo da una classe.
;;; Se slot-name non è presente nella classe dell'istanza
;;; viene segnalato un errore.
(defun field (instance slot-name)
  ;(stampa-oggetto "field instance")
  ;(stampa-oggetto instance)
  ;(stampa-oggetto "field slot-name")
  ;(stampa-oggetto slot-name)
    ;; Se l'instanza non ha lo slotname, vedi la sua classe 
        (cond ((get-data instance slot-name)) 
            ;; Se la classe non ha lo slotname cerca nei padri 
            ((get-data (get-class-spec (cadr instance)) slot-name))
            ((get-parent-slot (get-parents (cadr instance)) slot-name))
            ((error 
                (format nil 
			"Error: no method or slot named ~a found." slot-name)))))



;;; field*: estrae il valore da una classe percorrendo una catena di attributi.
;;; Il risultato è il valore associato all'ultimo elemento di slot-name
;;; nell'ultima istanza.
;;; Se uno degli elementi di slot-name non esiste nella classe
;;; dell'istanza, viene segnalato un errore.
(defun field* (instance &rest slot-name)
  (cond 
    ((null (is-instance (field instance (if (listp (car slot-name)) 
					 (caar slot-name) (car slot-name))))) 
     (error "Errore field* non è un'istanza"))
    ((eq (length slot-name) 1) 
     (field instance (if (listp (car slot-name)) 
                      (caar slot-name) (car slot-name))))
    (T (field* (field instance (if (listp (car slot-name)) 
                             (caar slot-name) (car slot-name))) 
            (cdr slot-name)))))


;;; end of file -- ool.lisp
