# purescript-deck-gl

[![Build Status](https://travis-ci.org/f-o-a-m/purescript-deck-gl.svg?branch=master)](https://travis-ci.org/f-o-a-m/purescript-deck-gl)



This is a purescript-react wrapper around Uber's [deck.gl](https://github.com/uber/deck.gl) library -- a react library for creating data visualizations on top of map-gl. Each vizualization (e.g. icon plotting, screen grid, etc.) is attached as a "layer". Each individual layer has its own configuration options, and there are some configurations that are common among all layers. It is also easy to hook in your own handlers that respond to dom events eminating from the layers.

There is an example project demonstrating the basic features of icon layer. You can build it yourself or view the hosted version [here](https://f-o-a-m.github.io/purescript-deck-gl/).

## installation

```bash
> npm i
> npm run build
```

## examples

```bash
> npm run example-icon
```

and serve `example/icon/dist`
