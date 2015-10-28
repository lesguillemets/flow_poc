./build/flow.js: ./src/*.hs
	hastec --debug --preserve-names ./src/Flow.hs -isrc -o ./build/flow.js
