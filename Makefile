./build/flow.js: ./src/*.hs
	hastec --opt-all -O2 ./src/Flow.hs -isrc -o ./build/flow.js
