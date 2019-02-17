PREFIX ?= .
BIN=$(PREFIX)/bin
SHARE=$(PREFIX)/share
BUILD=$(PREFIX)/.build
BINARY_NAME=try-purescript
MAIN ?= First.Main

all: clean static

start: ensure-bin server
	$(BIN)/$(BINARY_NAME) --from $(SHARE)

static: bundle copy

bundle: ensure-static
	spago bundle --main $(MAIN) --to $(SHARE)/index.js

copy: ensure-static
	cp -v index.html $(SHARE)/index.html
	cp -v index.css $(SHARE)/index.css

clean: ensure-bin ensure-static
	rm -rfv $(SHARE)/*
	rm -rfv $(BIN)/*
	rm -rfv $(BUILD)/*

ensure-static:
	mkdir -p $(SHARE)

server: ensure-bin
	ghc -hidir $(BUILD) -odir $(BUILD) -o $(BIN)/$(BINARY_NAME) Main.hs

ensure-bin:
	mkdir -p $(BIN)
