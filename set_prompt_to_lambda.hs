-- nastavimo začetnico lambda v urejevalniku ghci
:set prompt "\ESC[34mλ> \ESC[m" 
-- \ESC[34m je za modro barvo; \ESC[m pa nastavi vse nazaj na normalno

-- :set prompt "\ESC[1;34m%s\n\ESC[0;34mλ> \ESC[m"
-- \ESC[1;34m%s\n produces a first line which lists the loaded modules in bold blue, whereas \ESC[0;34mλ> \ESC[m sets the prompt like above, but removes the bold-attribute.--
-- povzeto po: https://coderwall.com/p/13h9bw/colored-ghci-prompt-with-and-modules-on-separate-lines