build: hpack
	cabal --ghc-options='${GHC_OPTIONS}' build

hpack:
	hpack .

format: hpack
	alejandra -q .
	find app/ src/ test/ -name "*.hs" -exec fourmolu -i {} +

ghcid: hpack
	ghcid -c cabal --ghc-options='${GHC_OPTIONS}' repl

run: hpack
	cabal --ghc-options='${GHC_OPTIONS}' run

hlint: hpack
	hlint .

.PHONY: hpack build format ghcid run hlint
