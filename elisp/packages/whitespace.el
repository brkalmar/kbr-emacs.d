(require 'whitespace)

(customize-set-variable 'whitespace-line-column nil)

(customize-set-variable
 'whitespace-display-mappings
 '((space-mark ?  [?·] [?⋅] [?•] [?.])
   (space-mark ?  [?¤] [?∘] [?_])
   (newline-mark #x0A [?↵ #x0A] [?↲ #x0A] [?↩ #x0A] [?⏎ #x0A] [?⤶ #x0A]
                 [?⮒ #x0A] [?$ #x0A])
   (tab-mark ?	 [?» ?	] [?\\ ?	])))
