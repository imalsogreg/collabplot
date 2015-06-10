all: static/collaborations.svg static/collaborations.html

static/collaborations.svg: src/*.hs collabdata/model.json
	cabal build
	dist/build/collabplot/collabplot collabdata/model.json
	mv collaborations.svg static/

static/collaborations.html: src/*.hs collabdata/model.json
	cabal build
	dist/build/collabplot/collabplot collabdawa/model.json
	mv collaborations.html static/
