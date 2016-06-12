;;;; cl-libhoedown.lisp
(in-package #:cl-user)
(defpackage #:cl-libhoedown
  (:use #:cl)
  (:nicknames #:libhoedown #:hoedown)
  (:export #:hoedown-version
           #:*hoedown-buffer-unit-size*
           #:*hoedown-extensions*
           #:hoedown-html-flags*
           #:*hoedown-html-renderer-nesting-levels*
           #:*hoedown-document-max-nesting*
           #:add-hoedown-html-flags #:delete-hoedown-html-flags
           #:add-hoedown-extensions #:delete-hoedown-extensions
           #:render)
  (:documentation "CL-LIBHOEDOWN Package."))
(in-package #:cl-libhoedown)

;;; Utilities
(defun compute-enum-value (enum-type keywords)
  (declare (type (or symbol list) keywords)
           (ignorable enum-type))
  (if keywords
      (let ((numbers
             (mapcar (lambda (k) (cffi:foreign-enum-value enum-type k))
                     keywords)))
        (apply 'logior numbers))
      0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Load Foreign Library ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library libhoedown
    (:darwin (:or "libhoedown.dylib.3" "libhoedown.dylib" "libhoedown.so.3" "libhoedown.so"))
    (:unix (:or "libhoedown.so.3" "libhoedown.so"))
    (t "libhoedown"))
  (unless (cffi:foreign-library-loaded-p 'libhoedown)
    (with-simple-restart
        (skip "Skip loading foreign library libhoedown.")
      (cffi:use-foreign-library libhoedown))))

(cffi:defctype size-t :unsigned-int)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Version ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar %hoedown-version% nil
    "The hoedown version information, acting like a cache for function 'hoedown-version'.")
  (defun hoedown-version ()
    "Retrieve Hoedown's version numbers, returning 4 values where the first is a string representing the version and
the 2nd, 3rd and 4th are major, minor and revision number repsectively."
    (if %hoedown-version%
        ;; then use cache
        (apply 'values %hoedown-version%)
        ;; else call foreign function
        (cffi:with-foreign-objects
            ((major :int) (minor :int) (revision :int))
          (cffi:foreign-funcall "hoedown_version"
                                :pointer major
                                :pointer minor
                                :pointer revision
                                :void)
          (let* ((metas (mapcar (lambda (x) (cffi:mem-ref x :int))
                                (list major minor revision)))
                 (version* (reduce (lambda (x y)
                                     (concatenate 'string x "." y))
                                   (mapcar 'write-to-string metas)))
                 (version (cons version* metas)))
            (setf %hoedown-version% version)
            (apply 'values version)))))
  (hoedown-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Hoedown Buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *hoedown-buffer-unit-size* 16
  "Reallocation unit size (0 = read-only buffer) for a hoedown buffer, set to 16 by default.")

(cffi:defcstruct hoedown-buffer
  (data (:pointer :uint8)) ;; actual character data
  (size size-t) ;; size of the string
  (asize size-t) ;; allocated size (0 = volatile buffer)
  (unit size-t) ;; reallocation unit size (0 = read-only buffer)
  (data-alloc :pointer)
  (data-free :pointer)
  (buffer-free :pointer))

(cffi:defctype hoedown-buffer (:struct hoedown-buffer))

(cffi:defcfun ("hoedown_buffer_new" hoedown-buffer-new) :pointer
  "Allocate a new hoedown buffer."
  (unit size-t))

(cffi:defcfun ("hoedown_buffer_free" hoedown-buffer-free) :void
  "Free the hoedown buffer."
  (buffer :pointer))

(defmacro with-hoedown-buffer ((var) &body body)
  "Create a hoedown buffer by specifying the unit parameter which means
'reallocation unit size (0 = read-only buffer)' and its default value is 16, finally free the buffer."
  `(let ((,var (hoedown-buffer-new *hoedown-buffer-unit-size*)))
     (unwind-protect
          (progn ,@body (values))
       (hoedown-buffer-free ,var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HTML Hoedown Renderer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:defcenum hoedown-html-flags
  (:hoedown-html-skip-html #.(ash 1 0)) ;; All HTML tags are stripped.
  (:hoedown-html-escape #.(ash 1 1)) ;; All HTML is escaped.
  (:hoedown-html-hard-wrap #.(ash 1 2)) ;; Line breaks are translated into <br> tags.
  (:hoedown-html-use-xhtml #.(ash 1 3))) ;; Renders XHTML instead of HTML.

(defvar *hoedown-html-flags* nil
  "Hoedown html flags, which derectly affects the behavior of hoedown html renderers. The enabled flags are:

'(:hoedown-html-skip-html :hodown-html-escape :hoedown-html-hard-wrap :hoedown-html-use-html)

You can enable multiple flags by using 'add-hoedown-html-flags'.")

(defvar %hoedown-enabled-html-flags%
  (cffi:foreign-enum-keyword-list 'hoedown-html-flags)
  "All enabled hoedown html flags.")

(defun add-hoedown-html-flags (&rest flags)
  "Add flag(s) to *hoedown-html-flags* ."
  (dolist (flag flags)
    (if (member flag %hoedown-enabled-html-flags% :test 'eql)
        (pushnew flag *hoedown-html-flags* :test 'eql)
        (warn "~A is not one of the enabled hoedown html flags: ~A~%Give up on it..."
              flag %hoedown-enabled-html-flags%)))
  *hoedown-html-flags*)

(defun delete-hoedown-html-flags (&rest flags)
  "Delete flag(s) from *hoedown-html-flags* ."
  (dolist (flag flags)
    (if (member flag %hoedown-enabled-html-flags% :test 'eql)
        (setf *hoedown-html-flags*
              (delete flag *hoedown-html-flags* :test 'eql))
        (warn "~A is not one of the enabled hoedown html flags: ~A~%Give up on it..."
              flag %hoedown-enabled-html-flags%)))
  *hoedown-html-flags*)

(defvar *hoedown-html-renderer-nesting-levels* 0
  "Nesting levels for a hoedown html renderer, set to 0 by default.")

(cffi:defcfun ("hoedown_html_renderer_new" hoedown-html-renderer-new) :pointer
  "Allocates a regular HTML renderer."
  (flags :unsigned-int) (nesting-level :int))

#+ignore
(cffi:defcfun ("hoedown_html_toc_renderer_new" hoedown-html-toc-renderer-new) :pointer
  "Like hoedown_html_renderer_new, but the returned renderer produces the Table of Contents."
  (nesting-level :int))

(cffi:defcfun ("hoedown_html_renderer_free" hoedown-html-renderer-free) :void
  "Deallocate an HTML renderer."
  (renderer :pointer))

(defmacro with-hoedown-html-renderer ((var) &body body)
  `(let ((,var
          (hoedown-html-renderer-new
           (compute-enum-value 'hoedown-html-flags *hoedown-html-flags*)
           *hoedown-html-renderer-nesting-levels*)))
       (unwind-protect
            (progn ,@body (values))
         (hoedown-html-renderer-free ,var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Hoedown Markdown Extensions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:defcenum hoedown-extensions
  ;; block-level extensions 
  (:hoedown-ext-tables #.(ash 1 0)) ;; Enables Markdown Extra style tables.
  (:hoedown-ext-fenced-code #.(ash 1 1)) ;; Enables fenced code blocks.
  (:hoedown-ext-footnotes #.(ash 1 2)) ;; Enables Markdown Extra style footnotes.

  ;; span-level extensions
  (:hoedown-ext-autolink #.(ash 1 3)) ;; Automatically parse URLs into links.
  (:hoedown-ext-strikethrough #.(ash 1 4)) ;;  Enables ~~striking~~ text.
  (:hoedown-ext-underline #.(ash 1 5)) ;; Translates <em> tags into <u> tags.
  (:hoedown-ext-highlight #.(ash 1 6)) ;; Enables ==marking== text.
  (:hoedown-ext-quote #.(ash 1 7)) ;; "Quotes" are translated into <q> tags.
  (:hoedown-ext-superscript #.(ash 1 8)) ;; Enables super^script.
  (:hoedown-ext-math #.(ash 1 9))

  ;; other flags
  (:hoedown-ext-no-intra-emphasis #.(ash 1 11)) ;; Disables emphasis_between_words.
  (:hoedown-ext-space-headers #.(ash 1 12)) ;; ATX style headers require a space after the opening number sign(s).
  (:hoedown-ext-math-explicit #.(ash 1 13))

  ;; negative flags
  (:hoedown-ext-disable-indented-code #.(ash 1 14)) ;; Disables indented code blocks.
  )

(defvar *hoedown-extensions* nil
  "Hoedown Extensions, which affects the behavior of Markdown syntax such as tables, autolinking, footnotes... The enabled extensions are:

'(:hoedown-ext-tables :hoedown-ext-fenced-code :hoedown-ext-footnotes
  :hoedown-ext-autolink :hoedown-ext-strikethrough :hoedown-ext-underline
  :hoedown-ext-highlight :hoedown-ext-quote :hoedown-ext-superscript
  :hoedown-ext-math :hoedown-ext-no-intra-emphasis :hoedown-ext-space-headers
  :hoedown-ext-math-explicit :hoedown-ext-disable-indented-code)

You can enable multiple extensions by using 'add-hoedown-extensions'.")

(defvar %hoedown-enabled-extensions%
  (cffi:foreign-enum-keyword-list 'hoedown-extensions)
  "All enabled hoedown extensions.")

(defun add-hoedown-extensions (&rest exts)
  "Add extension(s) to *hoedown-extensions* ."
  (dolist (ext exts)
    (if (member ext %hoedown-enabled-extensions% :test 'eql)
        (pushnew ext *hoedown-extensions* :test 'eql)
        (let ((*print-length* 5))
          (warn "~A is not one of the enabled hoedown extensions: ~A~%Give up on it..."
                ext %hoedown-enabled-extensions%))))
  *hoedown-extensions*)

(defun delete-hoedown-extensions (&rest exts)
  "Delete extension(s) from *hoedown-extensions* ."
  (dolist (ext exts)
    (if (member ext %hoedown-enabled-extensions% :test 'eql)
        (setf *hoedown-extensions*
              (delete ext *hoedown-extensions* :test 'eql))
        (let ((*print-length* 5))
          (warn "~A is not one of the enabled hoedown extensions: ~A~%Give up on it..."
                ext %hoedown-enabled-extensions%))))
  *hoedown-extensions*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Hoedown Document ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *hoedown-document-max-nesting* 16
  "Max nesting for a hoedown document, set to 16 by default.")

(cffi:defcfun ("hoedown_document_new" hoedown-document-new) :pointer
  "Allocate a new document processor instance."
  (renderer :pointer) (extensions :unsigned-int) (max-nesting size-t))

(cffi:defcfun ("hoedown_document_free" hoedown-document-free) :void
  "Deallocate a document processor instance."
  (document :pointer))

(defmacro with-hoedown-document ((var) &body body)
  (let ((renderer (gensym "RENDERER-")))
    `(with-hoedown-html-renderer (,renderer)
       (let ((,var
              (hoedown-document-new
               ,renderer (compute-enum-value 'hoedown-extensions *hoedown-extensions*) *hoedown-document-max-nesting*)))
         (unwind-protect
              (progn ,@body (values))
           (hoedown-document-free ,var))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Hoedown Render ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:defcfun ("hoedown_document_render" hoedown-document-render) :void
  "Render regular Markdown using the document processor."
  (document :pointer) (buffer :pointer) (data :pointer) (size size-t))

#+ignore
(cffi:defcfun ("hoedown_document_render_inline" hoedown-document-render-inline) :void
  "Render inline Markdown using the document processor."
  (document :pointer) (buffer :pointer) (data :pointer) (size size-t))

(defun render (markdown)
  "Rendering a markdown file or string."
  (declare (type (or pathname simple-string) markdown)
           (optimize speed (safety 0) (space 0) (debug 0) (compilation-speed 0)))
  (when (pathnamep markdown)
    (setf markdown (uiop:read-file-string markdown)))
  (let ((result (make-array 0 :element-type '(unsigned-byte 8)
                            :fill-pointer 0 :adjustable t)))
    (cffi:with-foreign-string (md markdown)
      (with-hoedown-document (document)
        (with-hoedown-buffer (buffer)
          (let ((len (babel:string-size-in-octets markdown)))
            (declare (type fixnum len))
            (hoedown-document-render document buffer md len)
            (cffi:with-foreign-slots ((data size) buffer hoedown-buffer)
              (dotimes (i size)
                (vector-push-extend (cffi:mem-ref data :uint8 i) result)))))))              
    (babel:octets-to-string result)))
