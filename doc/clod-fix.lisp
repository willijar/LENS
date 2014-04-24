(in-package :clod)

(defgeneric write-slot-properties(slot)
  (:method ((slot slot-definition))
    (write-bullet-point "Value type: ~(~A~)"
                        (org-safe-symbol
                         (type->string (slot-definition-type slot))))
    (write-bullet-point "Initial value: =~S="
                        (slot-definition-initform slot))
    (write-bullet-point "Initargs: ~(~A~)"
                        (if (slot-definition-initargs slot)
                            (list->string-with-commas
                             (slot-definition-initargs slot))
                            'none))
    (write-bullet-point "Allocation: ~(~A~)"
                        (slot-definition-allocation slot)))
  (:method((slot lens::parameter-slot))
    (call-next-method)
    (write-bullet-point "Parameter: t")
    (write-bullet-point "Properties: ~(~A~)"
                        (lens::slot-definition-properties slot)))
  (:method((slot lens::parameter-direct-slot-definition))
    (call-next-method)
    (write-bullet-point "Parameter: t")
    (when (lens::slot-definition-volatile slot)
      (write-bullet-point "Volatile: t"))
    (write-bullet-point "Properties: ~(~A~)"
                        (lens::slot-definition-properties slot))))

(defgeneric write-class-extras(class)
  (:method(class) (declare (ignore class)))
  (:method((c lens::parameter-class))
    (let ((parameter-slots
           (mapcan
            #'(lambda(s)
                (when (typep s 'lens::parameter-slot)
                  (list s)))
            (class-direct-slots c))))
      (writing-section ("Parameters")
        (unless parameter-slots (write-out "None."))
        (dolist (slot parameter-slots)
          (let ((properties (lens::slot-definition-properties slot)))
            (write-out
           "- ~(~A~) :: ~@[a =~(~S~)=.~] ~@[Default: =~A=.~] ~@[=Volatile=~*~]~A"           (lens::slot-definition-parameter-name slot)
           (if (getf properties :format)
               (lens::slot-definition-format slot)
               (lens::slot-definition-type slot))
           (lens::slot-definition-initform slot)
           (lens::slot-definition-volatile slot)
           (documentation slot t))))))))

(defmethod document :around ((slot slot-definition)
                             (doctype (eql :slot)))
  (writing-section-for-symbol (:slot (slot-definition-name slot))
    (writing-bulleted-list (write-slot-properties slot))
    (when (typep slot 'standard-slot-definition)
    #+sbcl(if (documentation slot t)
        (write-docstring-section "Description" (documentation slot t)))
    #-sbcl
    (if (and (slot-exists-p slot 'documentation)
             (slot-boundp slot 'documentation)
             (documentation slot t))
        (write-docstring-section "Description" (documentation slot t)))
    (call-next-method))))

(defmethod document ((sym symbol) (doctype (eql :class)))
  (let* ((c (find-class sym)))
    (ensure-finalized c nil)
    (writing-section-for-symbol (:class sym)
      (progn
        (unless (class-finalized-p c)
          (error "Not finalised"))
        (writing-section ("Inheritance")
          (write-bullet-point "Parent classes:")
          (write-indented (4)
            (write-list-as-paragraph
             (or
              (mapcar #'make-class-link
                      (mapcar
                       #'string-downcase
                       (mapcar
                        #'class-name
                        (class-direct-superclasses c))))
              (list "None."))))
          (write-bullet-point "Precedence list:")
          (write-indented (4)
            (write-list-as-paragraph
             (or
              (mapcar #'make-class-link
                      (mapcar
                       #'string-downcase
                       (mapcar
                        #'class-name
                        (class-precedence-list c))))
              (list "None."))))
          (write-bullet-point "Direct subclasses:")
          (write-indented (4)
            (write-list-as-paragraph
             (or
              (mapcar #'make-class-link
                      (mapcar
                       #'string-downcase
                       (mapcar
                        #'class-name
                        (class-direct-subclasses c))))
              (list "None.")))))
        (write-class-extras c)
        (write-docstring-section "Description" (documentation c t))
        (writing-section ("Direct slots")
          (dolist (slot (class-direct-slots c))
            (document slot :slot)))
        (when (list-all-indirect-slots (list c))
          (writing-section ("Indirect slots")
            (dolist (slot (list-all-indirect-slots (list c)))
              (document slot :slot))))))))

(defun write-preamble ()
  "* Arguments
None.
* Return Value
Ignored.
* Description
Writes some org instructions, intended to be placed at the start of the
document. These specify the document's author, title, and set some
export options."
  (write-out "~&#+TITLE: ~A" *document-title*)
  (write-out "~&#+AUTHOR: ~A" *document-author*)
  (write-out "~&#+EMAIL: ~A" *document-email*)
  (write-out "~&#+LINK: hs ~A/%s" (if (string-starts-with?
                                       *hyperspec-root* "http:")
                                      *hyperspec-root*
                                      (format nil "file:~A" *hyperspec-root*)))
  (if *document-style-sheet*
      (write-out
       "~&#+STYLE: <link rel=\"stylesheet\" type=\"text/css\" href=~S />"
       *document-style-sheet*))
  (write-out "~&#+STARTUP: showall")
  ;; H:NNN = below this many levels deep, headings become bulleted lists
  ;; toc:NNN = go this many levels deep in table of contents
  (write-out "~&#+OPTIONS: toc:4 H:10 num:3 @:t tags:nil~%~%"))


(defun lens::document-package(pkg)
  (clod::document-package
   pkg
   (namestring
   (merge-pathnames
    (string-downcase (substitute #\- #\. (string pkg)))
    #p"/home/willijar/dev/lisp/src/lens/doc/*.org"))
    :author "Dr. John A.R. Williams"
    :email "J.A.R.Williams@aston.ac.uk"
    :brief-methods t
    :internal-symbols? nil
    :class-diagram t
    :style-sheet "clod.css"
    :lines-between-sections nil))

;;(clod:document-package :lens.wsn "/home/willijar/dev/lisp/src/lens/doc/lens-wsn.org" :author "Dr. John A.R. Williams" :email "J.A.R.Williams@aston.ac.uk" :brief-methods t :internal-symbols? nil :style-sheet "clod.css" :lines-between-sections nil)
