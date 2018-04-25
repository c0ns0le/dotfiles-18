(deftheme github "The github theme")

(defcustom github-override-colors-alist '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist."
  :group 'github-theme
  :type '(alist
	  :key-type (string :tag "Name")
	  :value-type (string :tag " Hex")))

;; Color Palette

(defvar github-default-colors-alist
  '(("bg" . "#FFFFFF")
    ("fg" . "#2A292E")
    ("linenumber" . "#BABBBD")
    ("comment" . "#6A737D")
    ("red" . "#D73A49")
    ("navy" . "#032F62")
    ("blue" . "#005CC5")
    ("purple" . "#6F42C1"))
  "List of Github colors.
Each element has the form (NAME . HEX).")

;; Helper for customizing face colors

(defmacro github-with-color-variables (&rest body)
  "`let' bind all colors defined in `github-default-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append github-default-colors-alist
                           github-override-colors-alist)))
     ,@body))

(github-with-color-variables
  (custom-theme-set-faces
   'github
   ;; Built-In
   
   ;; -> Basic Coloring
   `(default ((t (:foreground ,fg :background ,bg))))
   `(fringe ((t (:background nil))))
   
   ;; Mode Specific
   
   ;; -> Linum
   `(linum ((t (:foreground ,linenumber))))
   
   ;; Font Lock
   `(font-lock-keyword-face ((t (:foreground ,red))))
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-string-face ((t (:foreground ,navy))))
   `(font-lock-constant-face ((t (:foreground ,blue))))
   `(font-lock-function-name-face ((t (:foreground ,purple))))
   `(font-lock-variable-name-face ((t (:foreground ,purple))))
   ))

(provide-theme 'github)
