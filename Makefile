ELM_MAKE_OUTPUT = Main.js
ELM_HTML_FILE = index.html
SCSS = styles
CSS_OUTPUT = Main.css
NATIVE = Native/**/*.js
SRC = src
DIST = build
VENDOR = vendor
ELM_SRC = $(SRC)/**/*.elm
SASSC_LOAD_PATH = bower_components/foundation/scss
VENDOR_FILES = $(addprefix $(VENDOR)/,router.js route-recognizer.js rsvp.js)
DIST_FILES = $(ELM_MAKE_OUTPUT) $(ELM_HTML_FILE) $(VENDOR_FILES) $(CSS_OUTPUT)

.PHONY: deps_osx db db_migrate db_setup db_clean deploy serve

$(DIST)/index.html: index.html
	cp $< $@

$(DIST)/%.js: $(SRC)/%.elm $(ELM_SRC) $(NATIVE)
	elm-make --output $@ $<

$(DIST)/%.css: $(SCSS)/%.scss
	sassc -t compressed -I $(SASSC_LOAD_PATH) -m $< $@

$(DIST)/%: %
	mkdir -p $(DIST)
	cp $< $@

deploy: Main.elm Main.css $(addprefix $(DIST)/,$(DIST_FILES))
	git add .
	git commit -m "Deploy :tada:"
	git subtree push --prefix $(DIST) deploy master

serve: $(DIST)/index.html $(DIST)/Main.js $(DIST)/main.css
	./serve

deps_osx:
	brew install sqitch
	brew install sqitch_pg
	brew install sassc
	brew install entr

db_migrate:
	sqitch deploy
	sqitch verify

db_clean:
	sqitch engine rm pg
	sqitch target rm devel
	dropdb diagrammer_devel

db_setup: db
	sqitch deploy
	sqitch verify

db:
	createdb diagrammer_devel
	sqitch target add devel db:pg:diagrammer_devel
	sqitch engine add pg devel
