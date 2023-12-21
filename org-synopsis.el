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

(defun org-synopsis-insert-drawer ()
  (interactive)
  (unless (org-at-heading-p) (org-back-to-heading))
  (forward-line 1)
  (insert ":synopsis:\n\n:end:\n")
  (backward-char 7))

(defun org-synopsis-only ()
  (interactive)
  (org-synopsis-mode 1)
  (goto-char (point-min))
  (unless (org-at-heading-p) (org-next-visible-heading 1))
  (save-excursion
    (org-element-map (org-element-parse-buffer 'element) 'drawer
      (lambda (drawer)
        (goto-char (goto-char (org-element-property :begin drawer)))
        (when (and (org-synopsis--synopsis-drawer-p drawer)
                   (save-excursion (org-back-to-heading 'invisible) (not (invisible-p (point)))))

          (outline-show-entry)

          (when (save-excursion (forward-line 1) (org-fold-core-folded-p))
            (put-text-property (point) (1+ (point)) 'org-synopsis-was-folded t))

          (org-fold-hide-drawer-toggle 'off)

          (let ((pos (org-synopsis--get-positions drawer)))
            (org-synopsis--hide (plist-get pos :above-beg) (plist-get pos :above-end))
            (org-synopsis--paint (plist-get pos :above-end) (plist-get pos :below-beg))
            (org-synopsis--hide (plist-get pos :below-beg) (plist-get pos :below-end))
            (org-next-visible-heading 1)))))))

(defun org-synopsis-sidebar-single ()
  (interactive)
  (org-synopsis-mode 1)
  (let* ((orig-pt (point))
         (windows (org-synopsis--get-sidebar-windows))
         (main-window (plist-get windows :main-window))
         (synopsis-window (plist-get windows :synopsis-window)))
    (select-window synopsis-window)
    (org-synopsis-mode 1)
    (widen)
    (org-fold-show-all '(headings drawers))
    (org-synopsis--remove-all)
    (goto-char orig-pt)
    (org-back-to-heading)
    (save-excursion
      (let ((case-fold-search t)
            (pt (pos-eol)))
        (org-synopsis--hide (pos-bol) (point-min))
        (search-forward ":synopsis:")
        (org-synopsis--hide pt (pos-eol))
        (setq pt (1+ (pos-eol)))
        (search-forward ":end:")
        (org-synopsis--paint pt (pos-bol))
        (org-synopsis--hide (pos-bol) (point-max))))
    (select-window main-window)))

(defun org-synopsis-sidebar ()
  (interactive)
  (org-synopsis-mode 1)
  (let* ((orig-pt (point))
         (windows (org-synopsis--get-sidebar-windows))
         (main-window (plist-get windows :main-window))
         (synopsis-window (plist-get windows :synopsis-window)))
    (select-window synopsis-window)
    (org-synopsis-mode 1)
    (org-synopsis--remove-all)
    (widen)
    (org-fold-show-all '(headings drawers))
    (goto-char (point-min))
    (outline-next-heading)
    (unless (org-at-heading-p) (outline-next-heading))
    (unless (bobp) (forward-line -1))

    (let ((ov (make-overlay (point-min) (1+ (pos-eol)) nil t)))
      (overlay-put ov 'org-synopsis t)
      (overlay-put ov 'invisible 'org-synopsis)
      (overlay-put ov 'modification-hooks '(org-synopsis--delete-overlay-if-changed)))

    (unless (org-at-heading-p) (outline-next-heading))

    (org-element-map (org-element-parse-buffer 'element) 'drawer
      (lambda (drawer)
        (when (and (org-synopsis--synopsis-drawer-p drawer)
                   (save-excursion (org-back-to-heading 'invisible) (not (invisible-p (point)))))
          (outline-show-entry)
          (let ((pos (org-synopsis--get-positions drawer)))
            (org-synopsis--hide (plist-get pos :above-beg) (plist-get pos :above-end))
            (org-synopsis--paint (plist-get pos :above-end) (plist-get pos :below-beg))
            (org-synopsis--hide (plist-get pos :below-beg) (plist-get pos :below-end) nil t)
            (org-next-visible-heading 1)))))

    (goto-char orig-pt)
    (recenter-top-bottom)
    (select-window main-window)))

(defun org-synopsis--get-sidebar-windows ()
  (let* ((synopsis-buffer-name (concat "*synopsis: " (buffer-name) "*"))
         synopsis-buffer main-window synopsis-window)

    (setq synopsis-buffer
          (or (get-buffer synopsis-buffer-name)
              (make-indirect-buffer (buffer-name) synopsis-buffer-name t t)))

    (if (setq synopsis-window
              (get-window-with-predicate
               (lambda (win)
                 (string= (buffer-name (window-buffer win)) synopsis-buffer-name))))
        (setq main-window (selected-window))
      (split-window-right 60)
      (setq synopsis-window (selected-window))
      (switch-to-buffer synopsis-buffer)
      (other-window 1)
      (setq main-window (selected-window)))

    `( :main-window ,main-window :synopsis-window ,synopsis-window )))

(defun org-synopsis--get-positions (drawer)
  (let (above-beg above-end below-beg below-end)
    (save-excursion
      (org-back-to-heading)
      (setq above-beg (1+ (pos-eol)))

      (goto-char (org-element-property :begin drawer))
      (setq above-end (1+ (pos-eol)))

      (let ((case-fold-search t))
        (search-forward ":end:")
        (setq below-beg (pos-bol)))

      (outline-next-heading)
      (setq below-end (pos-bol))

      `( :above-beg ,above-beg :above-end ,above-end
         :below-beg ,below-beg :below-end ,below-end ))))

(defun org-synopsis--hide (beg end &optional front rear spec)
  (unless spec (setq spec 'org-synopsis))
  (let ((ov (make-overlay beg end nil front rear)))
    (overlay-put ov 'org-synopsis t)
    (overlay-put ov 'invisible spec)
    (overlay-put ov 'modification-hooks '(org-synopsis--delete-overlay-if-changed))))

(defun org-synopsis--paint (beg end &optional folded)
  (let ((ov (make-overlay beg end nil nil t)))
    (overlay-put ov 'org-synopsis t)
    (overlay-put ov 'face `(:background ,org-synopsis-bg-color :extend t))
    (overlay-put ov 'org-folded folded)))

(defun org-synopsis--before-org-cycle-global (&rest _)
  (org-synopsis--remove-all))

(defun org-synopsis--before-org-cycle-local (&rest _)
  (let ((beg (pos-bol))
        (end (save-excursion (outline-next-heading) (point))))
    (org-synopsis--remove-all beg end)))

(defun org-synopsis--remove-all (&optional beg end)
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (remove-overlays beg end 'org-synopsis t)
  (save-excursion
    (goto-char beg)
    (let (pos)
      (while (< (setq pos (next-single-property-change (point) 'org-synopsis-was-folded nil end)) end)
        (goto-char pos)
        (remove-text-properties (point) (1+ (point)) '(org-synopsis-was-folded nil))
        (org-fold-hide-drawer-toggle 'hide)))))

(defun org-synopsis--synopsis-drawer-p (drawer)
  (string= "synopsis" (downcase (org-element--property :drawer-name drawer))))

(defun org-synopsis--delete-overlay-if-changed (ov &rest _)
  (when ov (delete-overlay ov)))

(provide 'org-synopsis)

;;; org-synopsis.el ends here
