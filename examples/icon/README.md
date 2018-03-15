## What is it
This example was transcribed from the [deck.gl icon example](https://github.com/uber/deck.gl/tree/master/examples/icon). It is showing data about known meteorite landings. The landings are clustered, and the radius of the clustering changes dynamically with the zoom level.

## Building
From the root of the project:

```bash
> npm install
> npm run build
> npm run example-icon
```

and serve `dist/` using a webserver.

Alternatively, build it from the current directory:

```bash
> npm install
> npm run webpack-dev-server
```

## Preview
![icon example](https://github.com/f-o-a-m/purescript-deck-gl/blob/initial-branch/examples/icon/image.png)
