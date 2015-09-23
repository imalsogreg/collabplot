LIB=$(wildcard collabplot/src/*.hs)
SERVER=server/dist/build/server/server
CLIENT=server/static/all.js
CLIENT_SRC=$(wildcard client/src/*.hs)

all: $(LIB) $(SERVER) $(CLIENT)

$(SERVER): $(LIB) server/server.cabal server/src/*.hs
	(cd server && cabal build)

$(CLIENT): $(LIB) client/client.cabal $(CLIENT_SRC)
	(cd client && cabal configure --ghcjs && cabal build)
	cp client/dist/build/client/client.jsexe/*.js server/static/js/
