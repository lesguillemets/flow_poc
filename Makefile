./build/flow.js: ./src/Flow.hs
	hastec -O2 --debug ./src/Flow.hs -o ./build/flow.js
