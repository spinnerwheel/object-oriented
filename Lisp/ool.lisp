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
             (not (listp parents))) 
         (error (format nil "Error: Class-name or Parents invalid."))))
  (add-class-spec class-name 
		  (append (list class-name) 
			  (append (list parents) (list (slot-structure part))))) 
  class-name)



;;;; check-parents


;;;; get-method-names: ! 
;;; data una lista di metodi, restituisce un lista con i nomi dei metodi
(defun get-method-names (method)
  (cond ((null method) NIL) 
        (T (cons (car method) (get-method-names (cddr method))))))

;;;; slot-structer: !
(defun slot-structure (slots) 
  (cond ((= (list-length slots) 0) nil) 
        ((member (car slots) (get-method-names (check-method slots))) 
         (cons (cons (car slots) 
                     (list  '=> (process-method (car slots) (car (cdr slots))))) 
               (slot-structure (cdr (cdr slots))))) 
        ((cons (cons  (car slots) (car (cdr slots))) 
               (slot-structure (cdr (cdr slots)))))))


;;;; process-method: !
;;; genera il codice necessaria per creare un metodo.
(defun process-method (method-name method-spec)
  (setf (fdefinition method-name) 
        (lambda (this &rest args) 
          (apply (<< this method-name) (append (list this) args)))) 
  (eval (rewrite-method-code method-name method-spec)))


;;;; rewrite-method-code: !
;;; riscrive il metodo come una lambda
(defun rewrite-method-code (method-name method-spec) 
  ;; Riscrive il metodo come una funzione lambda 
  (cons 'lambda 
        (cons (append (list 'this) (car (remove '=> method-spec))) 
              (cdr (remove '=> method-spec)))))


;;; check-method: !
;;; estrae i metodi dai parts passati li restituisce in una cons.
(defun check-method (slots) 
  ;;Estrae i metodi dagli slots 
  (cond ((null slots) nil) 
        ((and (listp (cadr slots)) (member '=> (cadr slots))) 
         (cons (car slots) 
               (cons (cadr slots) (check-method (cdr slots))))) 
        (T (check-method (cdr slots)))))


;;;; is-class !
;;; ritorna T se class-name � il nome di una classe
(defun is-class (class-name) 
  (if (get-class-spec class-name) T (error "Class-name not found!")))


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
         (member '=> (cdar instance))) 
    (caddar instance))
   ;; Se è un attributo 
   ((and (symbolp (caar instance)) 
         (equal (intern (symbol-name (caar instance)) "KEYWORD") 
                (intern (symbol-name slot-name) "KEYWORD"))) 
    ;; Se è nil ma esistente 
    (if (null (cdar instance)) "undefined" (cdar instance))) 
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



;;; make: crea una nuova istanza di una classe. !
(defun make (class-name &rest slot) 
  ;; Non instanzio metodi non esistenti nella classe 
  (cond ((not (is-class class-name)))                            
        ((append (list 'oolinst) 
                 (list class-name 
                       (slot-structure 
                        (check-slot-exists class-name slot)))))))


;;; check-slot-exists: controlla se ogni slot nella lista di slots passata !
;;; come argomento sono presenti nella class specificata.
;;; Se gli slots esistono viene restituita una cons contenente tutti gli
;;; slots validi, altrimenti la funzione segnala un errore
(defun check-slot-exists (class slots) 
  (cond ((null slots) nil) 
        ((get-class-data class (car slots)) 
         (cons (car slots) 
               (cons (cadr slots) (check-slot-exists class (cddr slots)))))
        (T (check-slot-exists class (cddr slots)))))


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
;;; non è presente in uno dei parents, va a cercarlo ed eventualmente
;;; ereditarlo dalla prossima classe della lista parents.
(defun get-parent-slot (parents slot-name) 
  (cond ((null parents) nil) 
        ((null (get-data (get-class-spec (car parents)) slot-name)) 
         (get-parent-slot (cdr parents) slot-name))
        ((get-data (get-class-spec (car parents)) slot-name))))


;;; is-instance: !
;;; ritorna T se viene passato coem oggetto l'istanza di una classe
(defun is-instance (value &optional (class-name T)) 
  (cond ((and (equal (car value) 'OOLINST) 
              (equal class-name 'T)) T) 
        ((equal (cadr value) class-name) T) 
        ;; Ereditarietà 
        ((member class-name (cadr (get-class-spec (cadr value)))) T)))

;;; end of file -- ool.lisp
