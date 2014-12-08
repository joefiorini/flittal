.PHONY: deps_osx db

deps_osx:
	brew install sqitch
	brew install sqitch_pg

db_migrate:
	sqitch deploy
	sqitch verify

db_clean:
	dropdb diagrammer_devel
	sqitch engine rm pg
	sqitch target rm devel

db_setup: db
	sqitch deploy
	sqitch verify

db:
	createdb diagrammer_devel
	sqitch target add devel db:pg:diagrammer_devel
	sqitch engine add pg devel
