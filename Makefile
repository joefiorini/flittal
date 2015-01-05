ELM_MAKE_OUTPUT = Main.js
CSS_OUTPUT = main.css
NATIVE = Native/**/*.js
SRC = src
ELM_HTML_FILE = $(SRC)/index.html
SCSS = $(SRC)/styles
DIST = build
VENDOR = vendor
ELM_SRC = $(SRC)/**/*.elm
SASSC_LOAD_PATH = bower_components/foundation/scss
VENDOR_FILES = $(addprefix $(VENDOR)/,router.js route-recognizer.js rsvp.js)
DIST_FILES = $(ELM_MAKE_OUTPUT) index.html $(CSS_OUTPUT)

.PHONY: deps_osx db db_migrate db_setup db_clean deploy deploy_alpha serve dist

$(DIST)/index.html: $(ELM_HTML_FILE)
	cp $< $@

$(DIST)/%.js: $(SRC)/%.elm $(ELM_SRC) $(NATIVE)
	elm-make --output $@ $<

$(DIST)/%.css: $(SCSS)/%.scss
	sassc -t compressed -I $(SASSC_LOAD_PATH) -m $< $@

dist: src/Main.elm $(SCSS)/main.scss $(addprefix $(DIST)/,$(DIST_FILES))

deploy_alpha: deploy
	git subtree push --prefix $(DIST) alpha master

deploy: dist
	git add .
	git commit -m "Deploy :tada:"

serve: $(DIST)/index.html $(DIST)/Main.js $(DIST)/main.css
	cd build && pushstate-server . 8000

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
