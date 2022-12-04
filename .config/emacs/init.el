;; emacs config-file

;; hack for LARBS
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (eq
     (if (numberp alpha)
         alpha
       (cdr alpha)) ; may also be nil
     100)
    (set-frame-parameter nil 'alpha '(85 . 50))
      (set-frame-parameter nil 'alpha '(100 . 100)))))

;; (global-set-key (kbd "C-c t") 'toggle-transparency)

;; general package-cfg
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)


;; needs package org-modern
(global-org-modern-mode)
;; olivetti-mode is a good package idea on top
;; set directory for org-mode attachments
(setq org-attach-id-dir "~/Documents/org-pkm/assets")

(use-package org-roam
	     :ensure t
	     :init
	     (setq org-roam-v2-ack t)
	     :custom
	     (org-roam-directory "~/Documents/org-pkm")
	     :bind (("C-c n l" . org-roam-buffer-toggle)
		    ("C-c n f" . org-roam-node-find)
		    ("C-c n i" . org-roam-node-insert))
	     :config
	     (org-roam-setup))
;; exclude all headlines wiht attachments from org-roam caching
(setq org-roam-db-node-include-function
      (lambda ()
        (not (member "ATTACH" (org-get-tags)))))


;; org-templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+olp "~/Documents/org-pkm/gtd.org" "Capture:" "Tasks:")
         "* TODO %?\n  %i")
	("n" "short note" entry (file+olp "~/Documents/org-pkm/gtd.org" "Capture:" )
	 "* NOTE %?\n")
        ;; ("m" "Meeting" entry (file+olp "~/Documents/org-pkm/gtd.org" "capture:" "Meetings:")
        ;;  (file "~/Documents/org-pkm/templates/tpl-meeting.org"))
	("j" "Journal entry" plain (function org-journal-find-location)
                               "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                               :jump-to-captured t :immediate-finish t)
	)
)
(setq org-agenda-files (quote ("~/Documents/org-pkm/" "~/Documents/org-pkm/journals/")))

(setq org-hide-emphasis-markers t)

;; the org-journal-dir has to be set before loading org-journal (see https://www.emacswiki.org/emacs/OrgJournal)
(setq org-journal-dir "~/Documents/org-pkm/journals/")
(setq org-journal-file-format "%Y-%m-%d")
(setq org-journal-date-format "%Y-%m-%d")
(setq org-journal-enable-agenda-integration t)
(require 'org-journal)
(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

;; set variable height for font-faces
;; (let* ((variable-tuple
;;           (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
;;                 ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;                 ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;                 ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;                 ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;                 (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;          (base-font-color     (face-foreground 'default nil 'default))
;;          (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

;;     (custom-theme-set-faces
;;      'user
;;      `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;      `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;      `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;      `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;      `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

;; TODO midterm: replace Selectrum by Helm or Ivy
;; Selectrum -- Completion in minibuffer
(selectrum-mode +1)
;; to make sorting and filtering more intelligent
(selectrum-prescient-mode +1)
;; to save your command history on disk, so the sorting gets more
;; intelligent over time
(prescient-persist-mode +1)


;; color-theme. zenburn, monokai, spacemacs and solarized are best-supported regarding syntax-hl
(use-package spacemacs-dark-theme
  :config (load-theme 'spacemacs-dark t))
;; week starts on Monday, not Sunday
(setq calendar-week-start-day 1)
;; shows possible key commands after typing
(require 'guide-key)
(setq guide-key/guide-key-sequence t)
(guide-key-mode 1)

;;
;; user-keys
;;

(global-set-key (kbd "\C-c a") 'org-agenda)
(global-set-key (kbd "\C-c j n") 'org-journal-next-entry)
(global-set-key (kbd "\C-c j p") 'org-journal-previous-entry)
(global-set-key (kbd "\C-c j j") 'org-journal-new-entry)
(global-set-key (kbd "<f6>") 'org-capture)

;;
;; custom-GUI-cfgs
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/home/maw/Documents/org-pkm/gtd.org" "/home/maw/Documents/org-pkm/done.org"))
 '(org-log-done 'time)
 '(org-log-into-drawer t)
 '(org-log-reschedule 'note)
 '(org-refile-allow-creating-parent-nodes 'confirm)
 '(org-refile-use-outline-path t)
 '(org-roam-completion-everywhere t)
 '(package-selected-packages
   '(lsp-ui lsp-mode evil magit org-journal spacemacs-theme zenburn-theme guide-key monokai-pro-theme selectrum-prescient selectrum org-roam))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
