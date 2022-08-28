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
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; general package-cfg
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
;; color-theme. zenburn, monokai, spacemacs and solarized are best-supported regarding syntax-hl
(use-package spacemacs-dark-theme
  :config (load-theme 'spacemacs-dark t))
;; shows possible key commands after typing
(require 'guide-key)
(setq guide-key/guide-key-sequence t)
(guide-key-mode 1)

;; needs package org-modern
(global-org-modern-mode)
;; olivetti-mode is a good package idea on top

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

(setq org-capture-templates '(("j" "Journal entry" plain (function org-journal-find-location)
                               "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                               :jump-to-captured t :immediate-finish t)))
;; enable org-roam at startup and start caching (takes forever)
;;(org-roam-db-autosync-mode)

;; week starts on Monday
(setq calendar-week-start-day 1)
;; TODO midterm: replace Selectrum by Helm or Ivy
;; Selectrum -- Completion in minibuffer
(selectrum-mode +1)
;; to make sorting and filtering more intelligent
(selectrum-prescient-mode +1)
;; to save your command history on disk, so the sorting gets more
;; intelligent over time
(prescient-persist-mode +1)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "<f6>") 'org-capture)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/home/maw/Documents/org-pkm/20220824173402-programming_languages.org" "/home/maw/Documents/org-pkm/20220824174829-emacs_lisp.org" "/home/maw/Documents/org-pkm/20220824175119-scheme.org" "/home/maw/Documents/org-pkm/20220824175721-schedule.org" "/home/maw/Documents/org-pkm/20220824180753-system_crafters.org" "/home/maw/Documents/org-pkm/done.org" "/home/maw/Documents/org-pkm/gtd.org" "/home/maw/Documents/org-pkm/journals/2022-08-28"))
 '(org-log-done 'time)
 '(org-log-into-drawer t)
 '(org-log-reschedule 'note)
 '(org-refile-allow-creating-parent-nodes 'confirm)
 '(org-refile-use-outline-path t)
 '(org-roam-completion-everywhere t)
 '(package-selected-packages
   '(org-journal spacemacs-theme zenburn-theme guide-key monokai-pro-theme selectrum-prescient selectrum org-roam))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
