ELM_MAKE_OUTPUT = Main.js
CSS_OUTPUT = main.css
NATIVE = Native/**/*.js
SRC = src
ELM_HTML_FILE = $(SRC)/index.html
SCSS = $(SRC)/styles
DIST = build
VENDOR = vendor
ELM_SRC = $(SRC)/**/*.elm
SASSC_LOAD_PATH = bower_components/normalize-scss:bower_components/bourbon/app/assets/stylesheets
VENDOR_FILES = $(addprefix $(VENDOR)/,router.js route-recognizer.js rsvp.js)
DIST_FILES = $(ELM_MAKE_OUTPUT) index.html $(CSS_OUTPUT)

.PHONY: deps_osx db db_migrate db_setup db_clean deploy deploy_alpha serve dist watch

$(DIST)/index.html: $(ELM_HTML_FILE)
	cp $< $@

$(DIST)/images: $(SRC)/images/*
	cp -r $< $(DIST)/images

$(DIST)/%.js: $(SRC)/%.elm $(ELM_SRC) $(NATIVE)
	elm-make --output $@ $<

$(DIST)/%.css: $(SCSS)/%.scss
	sass --scss -t compressed -I bower_components/normalize-scss -I bower_components/bourbon/app/assets/stylesheets src/styles/main.scss build/main.css

dist: src/Main.elm $(SCSS)/main.scss $(addprefix $(DIST)/,$(DIST_FILES))

deploy_alpha: deploy
	git subtree push --prefix $(DIST) alpha master

deploy: dist
	git add .
	git commit -m "Deploy :tada:"

serve: $(DIST)/index.html $(DIST)/Main.js $(DIST)/main.css $(DIST)/images
	cd build && pushstate-server . 8000

watch:
	supervise .

deps_osx:
	brew install sassc
	brew install entr
	brew install daemontools

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
