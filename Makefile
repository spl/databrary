tags:
	hothasktags Databrary/**/*.hs > $@

%.doctest: %.hs
	doctest -XMultiParamTypeClasses -XFlexibleContexts -XFlexibleInstances -XScopedTypeVariables -XConstraintKinds -XPatternGuards $<

haddock:
	cabal haddock --all --internal --hyperlink-source --haddock-options=--hyperlinked-source --contents-location=http://databrary.github.io/databrary/ --html-location='http://hackage.haskell.org/package/$$pkg-$$version/docs'
	perl -pi -e 's{<a href="([A-Z][A-Za-z0-9'\'']*(\.[A-Z][A-Za-z0-9'\'']*)*)\.html(#([a-z][A-Za-z0-9_'\'']*))?">}{"<a href=\"../".do{$$x=$$1;$$x=~tr/./-/;$$x}.".html".($$4&&"#v:".$$4)."\">"}ge' dist/doc/html/databrary/databrary/src/*.html

.PHONY: tags
