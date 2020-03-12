all: bundle

bundle:
	npx spago bundle-module -t output/index.js
