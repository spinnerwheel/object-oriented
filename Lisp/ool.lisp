;;;; -*- Mode: Lisp -*-


;;;; classes-specs !
(defparameter *classes-specs* (make-hash-table))



;;;; add-class-spec !
(defun add-class-spec (name class-spec) 
  (setf (gethash name *classes-specs*) class-spec))



;;;; get-class-spec !
(defun get-class-spec (name) 
  (gethash name *classes-specs*))



;;;; def-class !
;;; definisce una classe come variabile globale
(defun def-class (class-name parents &rest part)
  (cond ((or (not (atom class-name)) 
             (equal class-name '()) 
             (null class-name) 
             (not (listp parents))
	     (is-class class-name)) 
         (error (format nil "Error: Class-name invalid.")))
	((not (check-parents parents))
         (error (format nil "Error: Parents invalid."))))
  (add-class-spec class-name 
		  (append (list class-name)
			  (list parents)
			  (cond ((equal (car (first part)) 'fields)
				 (list
				  (append
				   (slot-structure
				    (redefine-struc (first part)))
				   (slot-structure-methods
				    (add-methods-prefix
				     (cdr (second part)))))))
				(T (list (append
					  (slot-structure
					   (redefine-struc  (second part)))
					  (slot-structure-methods
					   (add-methods-prefix
					    (cdr (first part))))))))))
  class-name)


;;;; check-parents/1
;;; verifica che tutti i parent presenti nella lista parents esistano
(defun check-parents (parents)
  (if parents
      (every #'is-class parents)
      t))


;;;; add-methods-prefix/1
;;; aggiunge il prefisso methods ad ogni metodo della lista part 
(defun add-methods-prefix (part)
  (mapcar (lambda (sottolista) (list 'methods sottolista)) part))



;;;; get-method-names:  
;;; data la lista method (che contiene i metodi), restituisce un lista
;;; con i nomi dei metodi
(defun get-method-names (method)
  (cond ((null method) NIL) 
        (T (cons (car (car  method)) (get-method-names (cdr method))))))


;;;; redefine-struc/1
;;; a partire dalla lista parts ridefinisce la lista in una nuova lista
;;; per poi essere essere analizzata dalla funzione slot-strucure
(defun redefine-struc (parts)
  (apply #'append (check-third-element (check-type-validation parts))))



;;;; check-type-validation/1
;;; se non è presente il type nei field aggiunge T
(defun check-type-validation (lista)
  (if (equal (car lista) 'fields)
      (mapcar (lambda (sottolista)
            (if (or (null sottolista) (null (cddr sottolista)))
                (append sottolista (list T))
                sottolista))
          (cdr lista))
          (mapcar (lambda (sottolista)
            (if (or (null sottolista) (null (cddr sottolista)))
                (append sottolista (list T))
                sottolista))
          lista)))

;;;; check-third-element/1
;;; controlla se il type di ogni field appartiene ai tipi base di CL
(defun check-third-element (list)
  (mapcar (lambda (sottolista)
	    (if (not (or (equal '(integer) (cddr sottolista))
			 (equal '(T) (cddr sottolista))
			 (equal '(string) (cddr sottolista))
			 (equal '(real) (cddr sottolista))
			 (is-class (cddr sottolista))))
		(error "Il terzo elemento non Ã¨ un tipo base di Lisp")
		sottolista))
	  list))


(defun slot-structure (slots)
  (cond ((= (list-length slots) 0) nil) 
        ((member (car slots) (get-method-names (check-method  slots)))
         (cons (cons (car (car (cdr slots)))
                     (list (process-method
			    (car (cdr (car slots))) (cdr (cdr (car slots))))))
               (slot-structure (cdr (cdr slots))))) 
        ((cons  (cons (first slots) (cons (second slots) (third slots))) 
		(slot-structure (cdr (cdr (cdr slots))))))))



(defun slot-structure-methods (slots)
  (cond ((= (list-length slots) 0) nil) 
        ((member (car (car (cdr (car slots))))
		 (get-method-names (remove nil (check-method-new  slots))))
         (cons (cons (car (car (cdr (car slots))))
                     (list (process-method
			    (car (car (cdr (car slots))))
			    (cdr (car (cdr (car slots)))))))
               (slot-structure-methods (cdr slots)))) 
        ((cons  (cons (first slots) (cons (second slots) (third slots))) 
		(slot-structure-methods (cdr (cdr (cdr slots))))))))


;;; check-method: 
;;; estrae i metodi dai parts passati li restituisce in una cons.
(defun check-method (parts)
  (cond ((null parts) nil) 
        ((and
	  (listp (cadr parts))
	  (member 'methods  parts))
         (cons (car parts) 
               (cons (cadr parts) (check-method (cdr (cdr parts)))))) 
        (T (check-method (cdr parts)))))



(defun check-method-new (parts) 
  (cond ((null parts) nil) 
        ((and
	  (listp (cadr parts))
	  (member 'methods (car parts)))
         (cons (car (cdr (car parts)))
               (cons
		(cdr (cdr (car parts)))
		(check-method-new (cdr parts)))))
        (T (check-method-new (cdr parts)))))

    

;;;; process-method/2
;;; prende in input il nome e il corpo del meotodo
;;; e genera il codice necessaria per creare un metodo.
(defun process-method (method-name method-spec)
  (if 
   (not (and (equal 'nil method-name)
             (equal 'nil method-spec)))
   
   (setf (fdefinition method-name) 
         (lambda (this &rest args) 
           (apply (field this method-name) (append (list this) args))))) 
  (eval (rewrite-method-code method-spec)))



;;;; rewrite-method-code/1
;;; prende in input il corpo del motodo e lo riscrive come una lambda
(defun rewrite-method-code (method-spec)
  (cons 'lambda
        (cons (append (list 'this) (car  method-spec)) 
              (cdr  method-spec))))





;;;; is-class/1
;;; ritorna T se class-name è il nome di una classe
(defun is-class (class-name) 
  (if (get-class-spec class-name) T nil)
  )




;;;; get-data/1
;;; estrae il valore dello part-name dall'istanza passati come parametri.
(defun get-data (instance part-name)
  (cond 
    ;; Caso base 
    ((null instance) nil)
    ;; Se è un atom 
    ((atom (car instance)) (get-data (caddr instance) part-name))
    ;; Se è un metodo 
    ((and (symbolp (caar instance)) 
          (equal (intern (symbol-name (caar instance)) "KEYWORD") 
                 (intern (symbol-name part-name) "KEYWORD")) 
          (listp (cdar instance)) 
          (equal 'methods (first instance)))
     (caddar instance))
    ;; Se è un attributo 
    ((and (symbolp (caar instance)) 
          (equal (intern (symbol-name (caar instance)) "KEYWORD") 
                 (intern (symbol-name  part-name) "KEYWORD"))) 
     ;; Se è nil ma esistente 
     (if (null (cdar instance)) "undefined" (second (car instance)))) 
    ;; Altrimenti 
    (T (get-data (cdr instance) part-name))))




;;;; get-parents
;;; data una classe, ritorna una lista con i tutti parents di quella classe
(defun get-parents (class) 
  (cond ((null (cadr (get-class-spec class))) nil) 
        ((remove-duplicates 
          (append 
           (append (get-parents (car (cadr (get-class-spec class)))) 
                   (get-parents (cdr (cadr (get-class-spec class))))) 
           (cadr (get-class-spec class)))
	  :from-end t))))



;;;; sublist/1
;;; data una lista di n elementi ritorna una lista di sottoliste accoppiando
;;; gli elementi due a due
(defun sublist (lista)
  (if (<= (length lista) 1)
      lista
      (cons (list (first lista) (second lista))
            (sublist (nthcdr 2 lista)))))



;;;; make/2
;;; crea una nuova istanza di una classe
(defun make (class-name &rest slot)
  ;; Non instanzio metodi non esistenti nella classe 
  (cond ((not (is-class class-name)))                            
        ((append (list 'oolinst) 
                 (list class-name
		       (if (get-class-type-slot
			    class-name
			    (slot-structure
			     (redefine-struc (sublist slot))))
			   
			   (slot-structure
                            (redefine-struc
			     (sublist
			      (check-slot-exists class-name slot)))))
		       )))))




;;;; check-slot-exists/2
;;; controlla se ogni slot nella lista di slots passata 
;;; come argomento sono presenti nella class specificata.
;;; Se gli slots esistono viene restituita una cons
;;; contenente tutti gli slots validi
(defun check-slot-exists (class slots)
  (cond ((null slots) nil) 
        ((get-class-data class (car slots)) 
         (cons (car slots) 
               (cons (cadr slots) (check-slot-exists class (cddr slots)))))
        (T (check-slot-exists class (cddr slots)))))



(defun get-data-type (instance slot-name)
  (cond 
    ;; Caso base 
    ((null instance) nil)
    ;; Se è un atom 
    ((atom (car instance)) (get-data-type (caddr instance) slot-name))
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
	     (cond ((equal nil (get-parents class))
		    (get-data-type
		     (get-class-spec  class)
		     (first element)))
		   (T (get-data-type
		       (get-class-spec (first (second (get-class-spec class))))
		       (first element)))))
	   slot-name)
   (mapcar (lambda (x)
	     (if (eq (second x) T)
		 T
		 (type-of (second x))))
	   slot-name)))



;;;; subtypep-list-check/2
;;; prende in input due liste che contengono valori
;;; dei type. Orignal-t contiene i type dichairati nella classe,
;;; mentre new-t contiene i type dei valori passati nella creazione
;;; dell'istanza. Se i valori sono uguali la funzione restituisce T,
;;; altrimenti invoca un errore
(defun subtypep-list-check (original-t new-t)
  (if (equal (length original-t) (length new-t))
      (if (member t original-t)
          (every (lambda (orig-t new-t)
                   (cond((equal orig-t t) T)
			(T (subtypep new-t orig-t))))
                 original-t
                 new-t)
          (if (every #'subtypep (mapcar #'list new-t)
		     (mapcar #'list original-t))
	      t
	      (error "invalid type")))      
      (error "invalid type")))



;;; get-class-data: estrae il valore dello slot-name specificato dalla !
;;; classe desiderata. Se slot-name non Ã¨ presente nella classe,
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
;;; non è presente in uno dei parents, va a cercarlo ed eventualmente
;;; ereditarlo dalla prossima classe della lista parents.
(defun get-parent-slot (parents slot-name) 
  (cond ((null parents) nil) 
        ((null (get-data (get-class-spec (car parents)) slot-name)) 
         (get-parent-slot (cdr parents) slot-name))
        ((get-data (get-class-spec (car parents)) slot-name))))



;;; is-instance/2
;;; ritorna T se viene passato come oggetto l'istanza di una classe
(defun is-instance (value &optional (class-name T)) 
  (cond ((and (equal (car value) 'OOLINST) 
              (equal class-name 'T)) T) 
        ((equal (cadr value) class-name) T) 
        ;; EreditarietÃ 
        ((member class-name (cadr (get-class-spec (cadr value)))) T)))



;;; field/2
;;; estrae il valore di un campo da una classe.
(defun field (instance slot-name)
  ;; Se l'instanza non ha lo slotname, vedi la sua classe 
  (cond ((get-data instance slot-name)) 
        ;; Se la classe non ha lo slotname cerca nei padri 
        ((get-data (get-class-spec (cadr instance)) slot-name))
        ((get-parent-slot (get-parents (cadr instance)) slot-name))
        ((error 
          (format nil 
		  "Error: no method or slot named ~a found." slot-name)))))



;;; field*/2
;;; estrae il valore da una classe percorrendo una catena di attributi.
;;; Il risultato è il valore associato all'ultimo elemento di slot-name
;;; nell'ultima istanza.
(defun field* (instance &rest slot-name)
  (cond 
    ((null (is-instance
	    (field instance (if (listp (car slot-name)) 
				(caar slot-name)
				(car slot-name))))) 
     (error "Errore field* non Ã¨ un'istanza"))
    ((eq (length slot-name) 1) 
     (field instance (if (listp (car slot-name)) 
			 (caar slot-name) (car slot-name))))
    (T (field* (field instance (if (listp (car slot-name)) 
				   (caar slot-name) (car slot-name))) 
               (cdr slot-name)))))


;;; end of file -- ool.lisp
