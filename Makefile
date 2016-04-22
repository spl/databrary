tags:
	hothasktags Databrary/**/*.hs > $@

%.doctest: %.hs
	doctest -XMultiParamTypeClasses -XFlexibleContexts -XFlexibleInstances -XScopedTypeVariables -XConstraintKinds -XPatternGuards $<

.PHONY: tags
