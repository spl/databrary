tags:
	hothasktags Databrary/**/*.hs > $@

%.doctest: %.hs
	doctest -XMultiParamTypeClasses -XFlexibleContexts -XFlexibleInstances -XScopedTypeVariables -XConstraintKinds -XPatternGuards $<

haddock:
	cabal haddock --all --internal --hyperlink-source --haddock-options=--hyperlinked-source --contents-location=http://databrary.github.io/databrary/ --html-location='http://hackage.haskell.org/package/$$pkg-$$version/docs'

.PHONY: tags
