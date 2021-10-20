all: build run

run:
	stack run main-exe
build:
	stack build --fast
watch:
	stack build --fast --file-watch
