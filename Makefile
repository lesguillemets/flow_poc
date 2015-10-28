./build/flow.js: ./src/Flow.hs
	hastec --debug --preserve-names ./src/Flow.hs -isrc -o ./build/flow.js
