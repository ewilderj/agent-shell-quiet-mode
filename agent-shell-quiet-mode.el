;;; agent-shell-quiet-mode.el --- Compact turn display for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Edd Wilder-James

;; Author: Edd Wilder-James
;; URL: https://github.com/ewilderj/agent-shell-quiet-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.35.2"))
;; Keywords: tools, convenience

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Provides quiet mode for agent-shell.  When enabled, all thought
;; process and tool call fragments within a turn are grouped under
;; collapsible sections, reducing vertical space.  Each phase of work
;; (a thought followed by tool calls) gets its own group with a
;; spinner and summary label.
;;
;; This is an add-on package that integrates with agent-shell entirely
;; via advice — no upstream changes are required.
;;
;; Usage:
;;   (require 'agent-shell-quiet-mode)
;;   (agent-shell-quiet-mode 1)

;;; Code:

(require 'map)
(require 'agent-shell-ui)

;; Forward declarations — these are defined in agent-shell and shell-maker
(defvar agent-shell--state)
(defvar font-lock-doc-markup-face)

(declare-function agent-shell--update-fragment "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell-viewport--buffer "agent-shell-viewport")
(declare-function agent-shell-viewport--shell-buffer "agent-shell-viewport")
(declare-function agent-shell-ui-toggle-fragment-at-point "agent-shell-ui")
(declare-function agent-shell-ui--nearest-range-matching-property "agent-shell-ui")

;;; --- Spinner ---

(defvar agent-shell-quiet-mode--spinner-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Braille spinner animation frames.")

(defun agent-shell-quiet-mode--spinner-start (state)
  "Start a spinner animation on the current quiet group in STATE."
  (agent-shell-quiet-mode--spinner-stop state)
  (when-let* ((group (map-elt state :quiet-group)))
    (let ((timer (run-with-timer
                  0 0.1 #'agent-shell-quiet-mode--spinner-tick state)))
      (map-put! group :spinner-timer timer)
      (map-put! group :spinner-index 0))))

(defun agent-shell-quiet-mode--spinner-stop (state)
  "Stop the spinner animation for the current quiet group in STATE."
  (when-let* ((group (map-elt state :quiet-group))
              (timer (map-elt group :spinner-timer)))
    (cancel-timer timer)
    (map-put! group :spinner-timer nil)))

(defun agent-shell-quiet-mode--spinner-tick (state)
  "Advance the spinner one frame and update the wrapper label in STATE."
  (condition-case err
      (when-let* ((group (map-elt state :quiet-group))
                  (buf (map-elt state :buffer)))
        (when (buffer-live-p buf)
          (let* ((idx (or (map-elt group :spinner-index) 0))
                 (frame (nth (mod idx (length agent-shell-quiet-mode--spinner-frames))
                             agent-shell-quiet-mode--spinner-frames))
                 (label (map-elt group :label))
                 (display-label (format "%s %s" frame label)))
            (map-put! group :spinner-index (1+ idx))
            (agent-shell--update-fragment
             :state state
             :namespace-id (map-elt group :request-count)
             :block-id (map-elt group :wrapper-block-id)
             :label-left (propertize display-label
                                     'font-lock-face 'font-lock-doc-markup-face)))))
    (error (message "quiet-mode spinner error: %S" err)
           (agent-shell-quiet-mode--spinner-stop state))))

;;; --- Group lifecycle ---

(defun agent-shell-quiet-mode--finalize-group (state)
  "Stop the spinner and show a checkmark on the current group in STATE."
  (agent-shell-quiet-mode--spinner-stop state)
  (when-let* ((group (map-elt state :quiet-group)))
    (let ((label (or (map-elt group :label) "Working…")))
      (agent-shell--update-fragment
       :state state
       :namespace-id (map-elt group :request-count)
       :block-id (map-elt group :wrapper-block-id)
       :label-left (propertize (format "✓ %s" label)
                               'font-lock-face 'font-lock-doc-markup-face))
      (when (map-elt group :child-block-ids)
        (agent-shell-quiet-mode--sync-children-visibility state group)))))

(defun agent-shell-quiet-mode--simplify-childless-groups (state)
  "Finalize any groups that were not yet finalized in STATE."
  (dolist (group (map-elt state :quiet-groups))
    (let ((label (or (map-elt group :label) "Working…")))
      (agent-shell--update-fragment
       :state state
       :namespace-id (map-elt group :request-count)
       :block-id (map-elt group :wrapper-block-id)
       :label-left (propertize (format "✓ %s" label)
                               'font-lock-face 'font-lock-doc-markup-face)))))

(defun agent-shell-quiet-mode--ensure-wrapper (state &optional new-thought-p)
  "Ensure a quiet-mode wrapper fragment exists for the current phase in STATE.
Creates a new wrapper when starting a new turn or when NEW-THOUGHT-P is
non-nil and the current group already has tool calls (indicating a new
phase of work).  Returns the current quiet-group alist."
  (let* ((group (map-elt state :quiet-group))
         (same-turn (and group
                         (equal (map-elt group :request-count)
                                (map-elt state :request-count))))
         (need-new (or (not same-turn)
                       (and same-turn new-thought-p
                            (map-elt group :has-tool-calls)))))
    (when need-new
      (when group
        (agent-shell-quiet-mode--finalize-group state))
      (let ((group-index (if same-turn
                             (1+ (or (map-elt state :quiet-group-index) 0))
                           1)))
        (setq group (list (cons :request-count (map-elt state :request-count))
                          (cons :wrapper-block-id
                                (format "quiet-%s-%s"
                                        (map-elt state :request-count)
                                        group-index))
                          (cons :child-block-ids nil)
                          (cons :has-tool-calls nil)
                          (cons :spinner-timer nil)
                          (cons :spinner-index 0)
                          (cons :thought-text "")
                          (cons :label "Working…")))
        (map-put! state :quiet-group group)
        (map-put! state :quiet-group-index group-index)
        (let ((groups (map-elt state :quiet-groups)))
          (map-put! state :quiet-groups (append groups (list group))))
        ;; Register invisibility spec in both buffers
        (dolist (buf (list (map-elt state :buffer)
                           (agent-shell-viewport--buffer
                            :shell-buffer (map-elt state :buffer)
                            :existing-only t)))
          (when (and buf (buffer-live-p buf))
            (with-current-buffer buf
              (add-to-invisibility-spec 'agent-shell-quiet))))
        ;; Create the wrapper fragment (collapsed by default)
        (agent-shell--update-fragment
         :state state
         :namespace-id (map-elt group :request-count)
         :block-id (map-elt group :wrapper-block-id)
         :label-left (propertize (map-elt group :label)
                                 'font-lock-face 'font-lock-doc-markup-face)
         :body " "
         :expanded nil)
        (agent-shell-quiet-mode--spinner-start state)))
    group))

;;; --- Label management ---

(defun agent-shell-quiet-mode--strip-markup (text)
  "Strip markdown bold/italic markup from TEXT.
Returns nil if the result is empty or whitespace-only."
  (let ((s text))
    (setq s (replace-regexp-in-string "\\*\\*\\|__" "" s))
    (setq s (string-trim s))
    (when (> (length s) 0) s)))

(defun agent-shell-quiet-mode--update-label (state text)
  "Accumulate thought TEXT into the current group in STATE.
Uses stripped text as the wrapper label, truncating at 72 chars."
  (when-let* ((group (map-elt state :quiet-group)))
    (let* ((accumulated (concat (or (map-elt group :thought-text) "") text))
           (label (agent-shell-quiet-mode--strip-markup accumulated)))
      (map-put! group :thought-text accumulated)
      (when label
        (let* ((first-line (car (split-string label "\n")))
               (too-long (> (length first-line) 72))
               (display-label (if too-long
                                  (concat (substring first-line 0 72) "…")
                                first-line))
               (need-child (or too-long (not (string= first-line label)))))
          (if need-child
              (let ((thought-id (format "%s-thought"
                                        (map-elt group :wrapper-block-id))))
                (map-put! group :label display-label)
                (agent-shell--update-fragment
                 :state state
                 :namespace-id (map-elt group :request-count)
                 :block-id thought-id
                 :label-left (propertize label
                                         'font-lock-face 'font-lock-doc-markup-face))
                (unless (member thought-id (map-elt group :child-block-ids))
                  (map-put! group :child-block-ids
                           (cons thought-id (map-elt group :child-block-ids))))
                (agent-shell-quiet-mode--sync-children-visibility state group))
            (map-put! group :label label)))))))

;;; --- Child fragment management ---

(defun agent-shell-quiet-mode--register-child (state block-id)
  "Register BLOCK-ID as a child of the current quiet group in STATE."
  (when-let* ((group (map-elt state :quiet-group)))
    (let ((children (map-elt group :child-block-ids)))
      (unless (member block-id children)
        (map-put! group :child-block-ids
                 (append children (list block-id)))))))

(defun agent-shell-quiet-mode--find-group-for-child (state block-id)
  "Find the quiet group in STATE that owns BLOCK-ID."
  (let ((found nil))
    (dolist (group (reverse (map-elt state :quiet-groups)))
      (when (and (not found)
                 (member block-id (map-elt group :child-block-ids)))
        (setq found group)))
    found))

(defun agent-shell-quiet-mode--mark-tool-call (state)
  "Mark the current quiet group in STATE as having tool calls."
  (when-let* ((group (map-elt state :quiet-group)))
    (map-put! group :has-tool-calls t)))

;;; --- Visibility / styling ---

(defun agent-shell-quiet-mode--style-child (start end collapsed)
  "Apply quiet-mode styling to child fragment region from START to END.
When COLLAPSED is non-nil, hide the region.  When nil, show it
with indentation."
  (if collapsed
      (save-excursion
        (let ((pos start))
          (while (< pos end)
            (let* ((val (get-text-property pos 'invisible))
                   (next (next-single-property-change pos 'invisible nil end)))
              (unless val
                (put-text-property pos next 'invisible 'agent-shell-quiet))
              (setq pos next)))))
    ;; Show: remove our invisible spec, leaving fragment-internal collapse
    (save-excursion
      (let ((pos start))
        (while (< pos end)
          (let* ((val (get-text-property pos 'invisible))
                 (next (next-single-property-change pos 'invisible nil end)))
            (when (eq val 'agent-shell-quiet)
              (remove-text-properties pos next '(invisible nil)))
            (setq pos next)))))
    (put-text-property start end 'line-prefix "  ")
    (put-text-property start end 'wrap-prefix "  ")
    ;; Reduce preceding newlines
    (save-excursion
      (goto-char start)
      (when (and (> start (point-min))
                 (eq (char-before start) ?\n)
                 (> (1- start) (point-min))
                 (eq (char-before (1- start)) ?\n))
        (put-text-property (1- start) start 'invisible 'agent-shell-quiet)))))

(defun agent-shell-quiet-mode--hide-wrapper-body (buf qualified-wrapper-id)
  "Hide the wrapper fragment's placeholder body in BUF.
QUALIFIED-WRAPPER-ID identifies the wrapper."
  (when (and buf (buffer-live-p buf))
    (with-current-buffer buf
      (save-mark-and-excursion
        (let ((inhibit-read-only t)
              (buffer-undo-list t))
          (goto-char (point-max))
          (when-let* ((match (text-property-search-backward
                              'agent-shell-ui-state nil
                              (lambda (_ s)
                                (equal (map-elt s :qualified-id) qualified-wrapper-id))
                              t))
                      (block-start (prop-match-beginning match))
                      (block-end (prop-match-end match))
                      (body-range (agent-shell-ui--nearest-range-matching-property
                                   :property 'agent-shell-ui-section :value 'body
                                   :from block-start :to block-end))
                      (label-end (or (map-elt (agent-shell-ui--nearest-range-matching-property
                                               :property 'agent-shell-ui-section :value 'label-right
                                               :from block-start :to block-end)
                                              :end)
                                     (map-elt (agent-shell-ui--nearest-range-matching-property
                                               :property 'agent-shell-ui-section :value 'label-left
                                               :from block-start :to block-end)
                                              :end))))
            (put-text-property label-end (map-elt body-range :end)
                               'invisible 'agent-shell-quiet)))))))

(defun agent-shell-quiet-mode--sync-children-visibility (state &optional group)
  "Sync quiet-mode child visibility with wrapper collapsed state in STATE.
Uses GROUP if provided, otherwise the current quiet group."
  (when-let* ((group (or group (map-elt state :quiet-group)))
              (wrapper-id (map-elt group :wrapper-block-id))
              (request-count (map-elt group :request-count))
              (namespace-id (format "%s" request-count))
              (qualified-wrapper-id (format "%s-%s" namespace-id wrapper-id)))
    (dolist (buf (list (map-elt state :buffer)
                       (agent-shell-viewport--buffer
                        :shell-buffer (map-elt state :buffer)
                        :existing-only t)))
      (when (and buf (buffer-live-p buf))
        (agent-shell-quiet-mode--hide-wrapper-body buf qualified-wrapper-id)
        (with-current-buffer buf
          (save-mark-and-excursion
            (let ((inhibit-read-only t)
                  (buffer-undo-list t))
              (goto-char (point-max))
              (when-let* ((match (text-property-search-backward
                                  'agent-shell-ui-state nil
                                  (lambda (_ s)
                                    (equal (map-elt s :qualified-id) qualified-wrapper-id))
                                  t))
                          (wrapper-state (get-text-property (prop-match-beginning match)
                                                           'agent-shell-ui-state)))
                (let ((wrapper-collapsed (map-elt wrapper-state :collapsed)))
                  (dolist (child-id (map-elt group :child-block-ids))
                    (let ((qualified-child-id (format "%s-%s" namespace-id child-id)))
                      (goto-char (point-max))
                      (when-let* ((child-match (text-property-search-backward
                                                'agent-shell-ui-state nil
                                                (lambda (_ s)
                                                  (equal (map-elt s :qualified-id) qualified-child-id))
                                                t)))
                        (let ((start (prop-match-beginning child-match))
                              (end (prop-match-end child-match)))
                          (save-excursion
                            (goto-char start)
                            (skip-chars-backward "\n")
                            (setq start (point)))
                          (agent-shell-quiet-mode--style-child
                           start end wrapper-collapsed))))))))))))))

;;; --- State initialization advice ---

(defun agent-shell-quiet-mode--init-state (state)
  "Add quiet-mode keys to STATE.  Used as :filter-return advice."
  (append state
          (list (cons :quiet-group nil)
                (cons :quiet-group-index 0)
                (cons :quiet-groups nil))))

;;; --- Notification advice ---
;;
;; This is the core integration.  We wrap `agent-shell--on-notification'
;; to inject quiet-mode behavior around the standard handling.

(defun agent-shell-quiet-mode--on-notification-advice (orig-fn &rest args)
  "Around advice for `agent-shell--on-notification'.
Injects quiet-mode wrappers and child registration around tool call
and thought chunk handling.  ORIG-FN and ARGS are the original function
and its arguments."
  (let* ((state (plist-get args :state))
         (notification (plist-get args :notification))
         (update (and notification
                      (map-nested-elt notification '(params update))))
         (session-update (and update (map-elt update 'sessionUpdate))))
    (cond
     ;; --- tool_call: ensure wrapper before, register child after ---
     ((equal session-update "tool_call")
      (agent-shell-quiet-mode--ensure-wrapper state)
      (apply orig-fn args)
      (agent-shell-quiet-mode--register-child state (map-elt update 'toolCallId))
      (agent-shell-quiet-mode--mark-tool-call state)
      (when-let* ((plan (map-nested-elt update '(rawInput plan))))
        (agent-shell-quiet-mode--register-child
         state (concat (map-elt update 'toolCallId) "-plan")))
      (agent-shell-quiet-mode--sync-children-visibility state))

     ;; --- agent_thought_chunk: replace standard display with label update ---
     ((equal session-update "agent_thought_chunk")
      (agent-shell-quiet-mode--ensure-wrapper state t)
      (let-alist update
        (when (and .content.text (not (string-empty-p .content.text)))
          (agent-shell-quiet-mode--update-label state .content.text)))
      ;; Update last-entry-type (normally done inside orig-fn)
      (map-put! state :last-entry-type "agent_thought_chunk"))

     ;; --- tool_call_update: call orig, then sync visibility ---
     ((equal session-update "tool_call_update")
      (apply orig-fn args)
      (let* ((tool-call-id (map-elt update 'toolCallId))
             (owning-group (agent-shell-quiet-mode--find-group-for-child
                            state tool-call-id)))
        (unless owning-group
          (agent-shell-quiet-mode--ensure-wrapper state)
          (agent-shell-quiet-mode--register-child state tool-call-id)
          (agent-shell-quiet-mode--mark-tool-call state)
          (setq owning-group (map-elt state :quiet-group)))
        (agent-shell-quiet-mode--sync-children-visibility state owning-group)))

     ;; --- everything else: pass through ---
     (t (apply orig-fn args)))))

;;; --- Turn completion advice ---

(defun agent-shell-quiet-mode--finish-output-advice (&rest _args)
  "Before advice for `shell-maker-finish-output'.
Finalizes the quiet group at the end of a turn."
  (when (and (bound-and-true-p agent-shell--state)
             (derived-mode-p 'agent-shell-mode))
    (let ((state (agent-shell--state)))
      (agent-shell-quiet-mode--finalize-group state)
      (agent-shell-quiet-mode--simplify-childless-groups state))))

;;; --- Toggle advice ---

(defun agent-shell-quiet-mode--toggle-children-advice (orig-fn)
  "Around advice for `agent-shell-ui-toggle-fragment-at-point'.
After toggling a quiet wrapper, also toggle its child fragments.
ORIG-FN is the original toggle function."
  (let* ((state-before (get-text-property (point) 'agent-shell-ui-state))
         (qualified-id (and state-before (map-elt state-before :qualified-id))))
    (funcall orig-fn)
    (when (and qualified-id
               (string-match "^\\(.+\\)-quiet-\\(.+\\)$" qualified-id))
      (let* ((shell-buf (cond
                          ((derived-mode-p 'agent-shell-mode) (current-buffer))
                          (t (agent-shell-viewport--shell-buffer))))
             (shell-state (and shell-buf (buffer-live-p shell-buf)
                               (buffer-local-value 'agent-shell--state shell-buf))))
        (when shell-state
          (let ((target-group nil))
            (dolist (group (map-elt shell-state :quiet-groups))
              (when (string-suffix-p (map-elt group :wrapper-block-id)
                                     qualified-id)
                (setq target-group group)))
            (agent-shell-quiet-mode--sync-children-visibility
             shell-state target-group)))))))

;;; --- Minor mode ---

;;;###autoload
(define-minor-mode agent-shell-quiet-mode
  "Toggle quiet mode for agent-shell.

When enabled, thought process and tool call fragments within a turn
are grouped under collapsible sections with spinner animations,
reducing vertical space."
  :global t
  :lighter nil
  :group 'agent-shell
  (if agent-shell-quiet-mode
      (progn
        (advice-add 'agent-shell--make-state
                    :filter-return #'agent-shell-quiet-mode--init-state)
        (advice-add 'agent-shell--on-notification
                    :around #'agent-shell-quiet-mode--on-notification-advice)
        (advice-add 'shell-maker-finish-output
                    :before #'agent-shell-quiet-mode--finish-output-advice)
        (advice-add 'agent-shell-ui-toggle-fragment-at-point
                    :around #'agent-shell-quiet-mode--toggle-children-advice))
    (advice-remove 'agent-shell--make-state
                   #'agent-shell-quiet-mode--init-state)
    (advice-remove 'agent-shell--on-notification
                   #'agent-shell-quiet-mode--on-notification-advice)
    (advice-remove 'shell-maker-finish-output
                   #'agent-shell-quiet-mode--finish-output-advice)
    (advice-remove 'agent-shell-ui-toggle-fragment-at-point
                   #'agent-shell-quiet-mode--toggle-children-advice)))

(provide 'agent-shell-quiet-mode)

;;; agent-shell-quiet-mode.el ends here
