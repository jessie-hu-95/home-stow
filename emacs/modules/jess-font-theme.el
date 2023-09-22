;;; Font and theme configurations


;;; Set my favorite fonts
(use-package emacs
  :when
  (and (display-graphic-p)
       (find-font (font-spec :name "Lucida Grande Mono DK"))
       (find-font (font-spec :name "Lucida Sans OT")))
  :custom
  (line-spacing 0.15)
  :custom-face
  (default        ((t (:family "Lucida Grande Mono DK" :height 130))))
  (fixed-pitch    ((t (:family "Lucida Grande Mono DK"))))
  (variable-pitch ((t (:family "Lucida Sans OT" :height 1.15)))))


;;; Modus theme customizations
(use-package emacs
  :when
  (display-graphic-p)
  :init
  ;; Use exponentially increasing font sizes for headings.  Set the
  ;; height of level-0 heading to `one-golden-ratio' times to the
  ;; default height.  Heading of each level decreases expotentially
  ;; until the 5th (totally 6 levels of heading).
  (defconst jess/one-golden-ratio (+ (/ (- (sqrt 5) 1) 2) 1))
  (defconst jess/1gr-6th-rt (expt jess/one-golden-ratio (/ 1.0 6.0)))

  :custom
  ;; Appearance for different components
  (modus-themes-mode-line '(accented))
  (modus-themes-hl-line '(accented intense))
  (modus-themes-region '(no-extend bg-only accented))

  ;; Remain monospaced when users opt for something like the command
  ;; ‘variable-pitch-mode’.
  (modus-themes-mixed-fonts t)

  ;; Disable fringes allow window borders
  (modus-themes-fringes nil)

  ;; Use red/blue color-coding instead of red/green
  (modus-themes-deuteranopia t)
  (modus-themes-diffs 'desaturated)

  ;; Appearance for different completion styles
  (modus-themes-completions
   '((matches   . (underline intense))
     (selection . (intense accented))
     (popup     . (intense accented))))

  ;; Customize syntax highlighting style.  The `faint' property fades
  ;; color saturation saturations.  It is enabled because an MIT
  ;; course suggests to avoid saturated colors in UI color design
  ;; (https://web.mit.edu/6.813/www/sp16/classes/16-color/#design-guidelines).
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-syntax '(faint alt-syntax green-strings yellow-comments))

  ;; Set sizes for 0~5th level headings
  (modus-themes-headings
   `((5 . (variable-pitch ,(expt jess/1gr-6th-rt 1) semibold))
     (4 . (variable-pitch ,(expt jess/1gr-6th-rt 2) semibold))
     (3 . (variable-pitch ,(expt jess/1gr-6th-rt 3) bold))
     (2 . (variable-pitch ,(expt jess/1gr-6th-rt 4) bold))
     (1 . (variable-pitch ,(expt jess/1gr-6th-rt 5) heavy))
     (0 . (variable-pitch ,(expt jess/1gr-6th-rt 6) heavy))
     (t . (variable-pitch 1.0))))

  :config
  (load-theme 'modus-operandi))


(provide 'jess-font-theme)
