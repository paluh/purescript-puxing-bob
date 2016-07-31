BUILD:

./build.sh

or by hand:

bower update
pulp build --to output/bundle.js

RUN:

Serve index.html somehow...

I prefer:

npm install webpack-dev-server
webpack-dev-server --history-api-fallback
