;;; package --- init-exwm.el: emacs as a window manager
;;; Commentary:
;;; Code:



(use-package exwm
  :hook
  (after-init . display-time-mode)
  (after-init . display-battery-mode)
  :custom
  (exwm-workspace-number 9)

	:config
	(require 'exwm)
  ;; to manage the windows (renaming)
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-class-name))))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
                        (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-title))))

  ;; keybindings
  (setq exwm-input-global-keys
        `(([?\s-p] . (lambda () (interactive) (shell-command "dmenu")))
          ([?\s-q] . exwm-reset)
          ([?\s-w] . exwm-workspace-switch)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda () (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          ([?\s-&] . (lambda (command) (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ([?\s-f] . exwm-layout-set-fullscreen)))
	(keymap-set exwm-mode-map "C-q" 'exwm-input-send-next-key)
  ;; simulation keys
  (setq exwm-input-simulation-keys
				'(
          ;; movement
          ([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])
          ;; cut/paste.
          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])
          ;; search
          ([?\C-s] . [?\C-f])))

  ;; hide minibuffer and echo area when not used
  ;; (setq exwm-workspace-minibuffer-position 'bottom)


  (require 'exwm-randr)
  (setq exwm-randr-workspace-monitor-plist
        '(0 "eDP-1" 1 "DP-1-0"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "autorandr" nil "autorandr -l wide")))
  (exwm-randr-enable)

  (require 'exwm-systemtray)
  (exwm-systemtray-enable))


(provide 'init-exwm)
;;; init-exwm.el ends here
