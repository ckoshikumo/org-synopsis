;;; -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)

(defcustom org-synopsis-bg-color "#2e2e3c"
  "Color to paint synopsis drawer with."
  :type 'color
  :group 'org-synopsis)

(defvar org-synopsis--status nil)
(defvar org-synopsis-mode-map (make-sparse-keymap))

(define-minor-mode org-synopsis-mode
  "Minor mode for using a synopsis drawer."
  :lighter " synopsis"
  (if org-synopsis-mode
      (progn
        (add-to-invisibility-spec 'org-synopsis)
        (add-to-invisibility-spec 'org-synopsis-text)
        (advice-add 'org-cycle-global :before #'org-synopsis--before-org-cycle-global)
        (advice-add 'org-cycle-internal-local :before #'org-synopsis--before-org-cycle-local))

    (remove-from-invisibility-spec 'org-synopsis)
    (remove-from-invisibility-spec 'org-synopsis-text)
    (advice-remove 'org-cycle-global #'org-synopsis--before-org-cycle-global)
    (advice-remove 'org-cycle-internal-local #'org-synopsis--before-org-cycle-local)
    (org-synopsis--remove-all)))

(defun org-synopsis-prepare-pop ()
  (let* ((synopsis-buffer-name (concat "*synopsis: " (buffer-name) "*"))
         (synopsis-buffer
          (or (get-buffer synopsis-buffer-name)
              (make-indirect-buffer (buffer-name) synopsis-buffer-name t t)))
         main-window synopsis-window)
    (if (setq synopsis-window
              (get-window-with-predicate
               (lambda (win)
                 (string= (buffer-name (window-buffer win)) synopsis-buffer-name))))
        (setq main-window (selected-window))
      (split-window-right 60)
      (setq synopsis-window (selected-window))
      (other-window 1)
      (setq main-window (selected-window)))
    (select-window synopsis-window)
    (switch-to-buffer synopsis-buffer)
    (widen)
    (org-fold-show-all '(headings drawers))
    (org-synopsis--remove-all)
    main-window))

(defun org-synopsis-pop-all ()
  (interactive)
  (org-synopsis-mode 1)
  (let ((orig-pt (point))
        (main-win (org-synopsis-prepare-pop)))

    (org-synopsis--synopsis-only 'pop)
    (goto-char (point-min))
    (outline-next-heading)
    (unless (bobp) (forward-line -1))
    (let ((ov (make-overlay (point-min) (pos-eol) nil t)))
      (overlay-put ov 'org-synopsis t)
      (overlay-put ov 'invisible 'org-synopsis)
      (overlay-put ov 'modification-hooks '(org-synopsis-delete-overlay-if-changed)))

    (goto-char orig-pt)
    (recenter-top-bottom)
    (select-window main-win)))

(defun org-synopsis-pop ()
  (interactive)
  (org-synopsis-mode 1)
  (let ((orig-pt (point))
        (main-win (org-synopsis-prepare-pop))
        (case-fold-search t))

    (goto-char orig-pt)
    (org-back-to-heading)
    (search-forward ":synopsis:" nil t)
    (let* ((drawer (org-element-at-point-no-context (pos-bol)))
           (positions (org-synopsis--get-positions drawer)))
      ;; (org-synopsis--hide-beg-line positions)
      (org-synopsis--hide-above positions)
      (org-synopsis--paint-background positions)
      (org-synopsis--hide-below positions))

    (org-narrow-to-subtree)
    (goto-char (point-min))
    (select-window main-win)))

(defun org-synopsis-insert-drawer ()
  (interactive)
  (unless (org-at-heading-p) (org-back-to-heading))
  (forward-line 1)
  (insert ":synopsis:\n\n:end:\n")
  (backward-char 7))

(defun org-synopsis-cycle ()
  (interactive)
  (save-excursion
    (org-synopsis-mode 1)
    (if (not (eq last-command this-command))
        (progn (org-synopsis--synopsis-only))
      (cond ((eq org-synopsis--status 'synopsis)
             (org-synopsis--synopsis-and-text))
            ((eq org-synopsis--status 'synopsis-and-text)
             (org-synopsis--synopsis-only))
            (t (org-synopsis--synopsis-only))))))

(defun org-synopsis--before-org-cycle-global (&rest _)
  (org-synopsis--remove-all))

(defun org-synopsis--before-org-cycle-local (&rest _)
  (let ((beg (pos-bol))
        (end (save-excursion (outline-next-heading) (point))))
    (org-synopsis--remove-all beg end)))

(defun org-synopsis--prepare ()
  "Do a bit of housekeeping before displaying the synopsis of an item."
  (unless org-synopsis-mode
    (org-synopsis-mode 1))
  (org-synopsis--remove-all)
  (goto-char (point-min))
  (unless (org-at-heading-p) (outline-next-heading)))

(defun org-synopsis--synopsis-and-text ()
  (interactive)
  (setq org-synopsis--status 'synopsis-and-text)
  (org-fold-show-all)
  (org-synopsis--prepare)

  ;; TODO: Try 'greater-element
  (org-element-map (org-element-parse-buffer 'element) 'drawer
    (lambda (drawer)
      (when (org-synopsis--synopsis-drawer-p drawer)
        (let ((positions (org-synopsis--get-positions drawer)))
          (org-synopsis--open-drawer positions)
          (org-synopsis--hide-above positions)
          ;; (org-synopsis--hide-beg-line positions)
          (org-synopsis--hide-end-line positions)
          (org-synopsis--paint-background positions)
          (outline-next-heading))))))

(defun org-synopsis--synopsis-only (&optional pop)
  (interactive)
  (setq org-synopsis--status 'synopsis)
  (org-cycle-overview)
  (org-synopsis--prepare)

  (org-element-map (org-element-parse-buffer 'element) 'drawer
    (lambda (drawer)
      (when (org-synopsis--synopsis-drawer-p drawer)
        (let ((positions (org-synopsis--get-positions drawer)))
          (save-excursion
            (org-back-to-heading)
            (outline-show-entry))
          (org-synopsis--open-drawer positions)
          ;; (org-synopsis--hide-beg-line positions)
          (org-synopsis--hide-above positions pop)
          (org-synopsis--paint-background positions)
          (org-synopsis--hide-below positions pop)
          (outline-next-heading))))))

(defun org-synopsis--remove-all (&optional beg end)
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (remove-overlays beg end 'org-synopsis t))

(defun org-synopsis--synopsis-drawer-p (drawer)
  (string= "synopsis" (downcase (org-element--property :drawer-name drawer))))

(defun org-synopsis--level ()
  (save-excursion
    (org-back-to-heading)
    (org-reduced-level (skip-chars-forward "*"))))

(defun org-synopsis--get-positions (drawer)
  (let ( above-beg above-end
         end-line-beg end-line-end
         content-beg content-end
         below-beg below-end )
    (save-excursion
      (org-back-to-heading)
      (setq above-beg (1+ (pos-eol)))

      (goto-char (org-element-property :begin drawer))
      (setq above-end (1+ (pos-eol))
            content-beg above-end)

      (goto-char (org-element-property :end drawer))
      (forward-line (- (1+ (org-element-property :post-blank drawer))))
      (setq end-line-beg (pos-bol)
            content-end end-line-beg
            below-beg content-end
            end-line-end (pos-eol))

      (outline-next-heading)
      (unless (eobp) (forward-line -1))
      (setq below-end (1+ (pos-eol))))

    `( :above-beg ,above-beg :above-end ,above-end
       :content-beg ,content-beg :content-end ,content-end
       :end-line-beg ,end-line-beg :end-line-end ,end-line-end
       :below-beg ,below-beg :below-end ,below-end )))

(defun org-synopsis--open-drawer (pos)
  (save-excursion
    (goto-char (plist-get pos :end-line-beg))
    (org-fold-hide-drawer-toggle 'off t)))

(defun org-synopsis--hide-above (pos &optional pop)
  (let ((ov (if pop (make-overlay (plist-get pos :above-beg) (plist-get pos :above-end) nil nil t)
              (make-overlay (plist-get pos :above-beg) (plist-get pos :above-end) nil nil t))))
    (overlay-put ov 'org-synopsis t)
    (overlay-put ov 'invisible 'org-synopsis)
    (overlay-put ov 'modification-hooks '(org-synopsis-delete-overlay-if-changed))))

(defun org-synopsis--hide-beg-line (pos)
  (let ((ov (make-overlay (plist-get pos :beg-line-beg) (plist-get pos :beg-line-end) nil nil t)))
    (overlay-put ov 'org-synopsis t)
    (overlay-put ov 'invisible 'org-synopsis)
    (overlay-put ov 'modification-hooks '(org-synopsis-delete-overlay-if-changed))))

(defun org-synopsis--paint-background (pos)
  (let ((ov (make-overlay (plist-get pos :content-beg) (plist-get pos :content-end))))
    (overlay-put ov 'org-synopsis t)
    (overlay-put ov 'face `(:background ,org-synopsis-bg-color :extend t))))

(defun org-synopsis--hide-end-line (pos)
  (let ((ov (make-overlay (plist-get pos :end-line-beg) (plist-get pos :end-line-end) nil t)))
    (overlay-put ov 'org-synopsis t)
    (overlay-put ov 'invisible 'org-synopsis)
    (overlay-put ov 'modification-hooks '(org-synopsis-delete-overlay-if-changed))))

(defun org-synopsis--hide-below (pos &optional pop)
  (let ((ov (if pop (make-overlay (plist-get pos :below-beg) (plist-get pos :below-end) nil nil t)
              (make-overlay (plist-get pos :below-beg) (plist-get pos :below-end) nil t))))
    (overlay-put ov 'org-synopsis t)
    (overlay-put ov 'invisible 'org-synopsis)
    (overlay-put ov 'modification-hooks '(org-synopsis-delete-overlay-if-changed))))

(defun org-synopsis-delete-overlay-if-changed (ov &rest _)
  (when ov (delete-overlay ov)))

(provide 'org-synopsis)

;;; org-synopsis.el ends here
