go:
	unset GHC_PACKAGE_PATH && cd src && cabal update -v && cabal install -v --installdir=.. --overwrite-policy=always
