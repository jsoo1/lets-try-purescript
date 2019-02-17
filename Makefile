PREFIX ?= .
BIN=$(PREFIX)/bin
SHARE=$(PREFIX)/share
BUILD=$(PREFIX)/.build
BINARY_NAME=try-purescript
MAIN ?= First.Main

all: clean-static static

start: ensure-bin clean-bin server
	$(BIN)/$(BINARY_NAME) --from $(SHARE)

static: bundle copy

bundle: ensure-static build
	spago bundle --main $(MAIN) --to $(SHARE)/index.js

build:
	spago install
	spago build

copy: ensure-static
	cp -v index.html $(SHARE)/index.html
	cp -v index.css $(SHARE)/index.css

clean-static: ensure-bin ensure-static
	rm -rfv $(SHARE)/*
	rm -rfv $(BUILD)/*

clean-bin:
	rm -rfv $(BIN)/*

ensure-static:
	mkdir -p $(SHARE)

server: ensure-bin
	ghc -hidir $(BUILD) -odir $(BUILD) -o $(BIN)/$(BINARY_NAME) Main.hs

ensure-bin:
	mkdir -p $(BIN)
