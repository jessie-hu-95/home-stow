;;; Font and theme configurations


;;; Set my favorite fonts
(use-package emacs
  :when
  (and (display-graphic-p)
       (find-font (font-spec :name "Lucida Grande Mono DK"))
       (find-font (font-spec :name "Lucida Bright OT"))
       )
  :custom
  (line-spacing 0.15)
  :custom-face
  (default        ((t (:family "Lucida Grande Mono DK" :height 130))))
  (fixed-pitch    ((t (:family "Lucida Grande Mono DK"))))
  (variable-pitch ((t (:family "Lucida Bright OT" :height 1.15)))))


;;; Modus theme customizations
(use-package emacs
  :when
  (and (display-graphic-p)
       (find-font (font-spec :name "Lucida Sans OT")))

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
  ;; color saturation saturations.  I enable it because an MIT course
  ;; suggests to avoid saturated colors in UI color design
  ;; (https://web.mit.edu/6.813/www/sp16/classes/16-color/#design-guidelines).
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-syntax '(faint alt-syntax green-strings yellow-comments))

  :config
  (defface jess/heading-font
    '((t :inherit variable-pitch :family "Lucida Sans OT"))
    "Lucida Sans OT")

  ;; Use exponentially increasing font sizes for headings.  Set the
  ;; height of level-0 heading to `golden-ratio-1.5' times to the
  ;; default height.  Heading of each level decreases expotentially
  ;; until the 8th (totally 9 levels of heading).
  (defconst jess/golden-ratio-1.5 (expt (+ (/ (- (sqrt 5) 1) 2) 1) 1.5))
  (defconst jess/gr1.5-9th-root (expt jess/golden-ratio-1.5 (/ 1.0 9.0)))

  (custom-set-faces
   `(modus-themes-heading-0 ((t :inherit jess/heading-font
                                :height ,(expt jess/gr1.5-9th-root 9)
                                :weight heavy)))
   `(modus-themes-heading-1 ((t :inherit jess/heading-font
                                :height ,(expt jess/gr1.5-9th-root 8)
                                :weight heavy)))
   `(modus-themes-heading-2 ((t :inherit jess/heading-font
                                :height ,(expt jess/gr1.5-9th-root 7)
                                :weight heavy)))
   `(modus-themes-heading-3 ((t :inherit jess/heading-font
                                :height ,(expt jess/gr1.5-9th-root 6)
                                :weight bold)))
   `(modus-themes-heading-4 ((t :inherit jess/heading-font
                                :height ,(expt jess/gr1.5-9th-root 5)
                                :weight bold)))
   `(modus-themes-heading-5 ((t :inherit jess/heading-font
                                :height ,(expt jess/gr1.5-9th-root 4)
                                :weight bold)))
   `(modus-themes-heading-6 ((t :inherit jess/heading-font
                                :height ,(expt jess/gr1.5-9th-root 3)
                                :weight semibold)))
   `(modus-themes-heading-7 ((t :inherit jess/heading-font
                                :height ,(expt jess/gr1.5-9th-root 2)
                                :weight semibold)))
   `(modus-themes-heading-8 ((t :inherit jess/heading-font
                                :height ,(expt jess/gr1.5-9th-root 1)
                                :weight semibold))))

  (load-theme 'modus-operandi))


(provide 'jess-font-theme)
