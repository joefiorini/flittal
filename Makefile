.PHONY: deps_osx db

deps_osx:
	brew install sqitch
	brew install sqitch_pg

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
