PREFIX ?= .
BIN=$(PREFIX)/bin
SHARE=$(PREFIX)/share
MESSAGES=$(SHARE)/messages
BUILD=$(PREFIX)/.build
BINARY_NAME=lets-try-purescript
MAIN ?= First.Main
PORT ?= 8080

all: clean-static static

start: ensure-bin clean-bin server copy
	$(BIN)/$(BINARY_NAME) \
	--from $(SHARE) \
	--on $(PORT) \
	--users $(SHARE)/users.json \
	--messages $(MESSAGES)

static: bundle copy

bundle: ensure-static build
	spago bundle --main $(MAIN) --to $(SHARE)/index.js

build:
	spago install
	spago build

copy: ensure-static
	cp -v index.html $(SHARE)/index.html
	cp -v index.css $(SHARE)/index.css
	cp -v users.json $(SHARE)/users.json
	mkdir -p $(MESSAGES)

clean-static: ensure-bin ensure-static
	rm -rfv $(SHARE)/*
	rm -rfv $(BUILD)/*

clean-bin:
	rm -rfv $(BIN)/*

ensure-static:
	mkdir -p $(SHARE)

server: ensure-bin
	ghc -Wno-missing-methods -hidir $(BUILD) -odir $(BUILD) -o $(BIN)/$(BINARY_NAME) Main.hs

ensure-bin:
	mkdir -p $(BIN)
