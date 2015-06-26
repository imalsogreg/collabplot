SERVER=server/dist/build/server/server
CLIENT=server/static/all.js

all: $SERVER $CLIENT

$SERVER: server/server.cabal server/src/*.hs
	cabal build

$CLIENT: client/collabplot.cabal client/*.hs
	cabal configure client --ghcjs
  cabal build client
	cp client/dist/build/collabplot/collabplot.jsexe/*.js sever/static/
