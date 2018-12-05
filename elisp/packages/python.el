(require 'python)

(customize-set-variable 'python-shell-interpreter "ipython3")
(customize-set-variable 'python-shell-interpreter-args
                        "-i --simple-prompt")
(customize-set-variable 'python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters "ipython3")
