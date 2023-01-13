build: hpack
	cabal build

hpack:
	hpack .

format: hpack
	alejandra -q .
	find app/ src/ test/ -name "*.hs" -exec fourmolu -i {} +

ghcid: hpack
	ghcid -c cabal repl

.PHONY: hpack build format ghcid
