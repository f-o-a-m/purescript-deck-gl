# purescript-deck-gl

<img src=https://github.com/f-o-a-m/purescript-web3/blob/master/purescript-web3-logo.png width="75">

[![Build Status](https://travis-ci.org/f-o-a-m/purescript-deck-gl.svg?branch=master)](https://travis-ci.org/f-o-a-m/purescript-deck-gl)

![FOAM spatial-index](https://github.com/f-o-a-m/purescript-deck-gl/blob/master/sample.png)


This is a purescript-react wrapper around Uber's [deck.gl](https://github.com/uber/deck.gl) library -- a react library for creating data visualizations on top of map-gl. Each vizualization (e.g. icon plotting, screen grid, etc.) is attached as a "layer". Each individual layer has its own configuration options, and there are some configurations that are common among all layers. It is also easy to hook in your own handlers that respond to dom events eminating from the layers.

There is an example project demonstrating the basic features of icon layer. You can build it yourself or view the hosted version [here](https://f-o-a-m.github.io/purescript-deck-gl/).

You can see the full gallery of examples [here](https://uber.github.io/deck.gl/#/examples/overview?section=core-layers)

## installation

```bash
> npm i
> npm run build
```


## Example

You can see some examples in `example` directory. It is also hosted [here](https://f-o-a-m.github.io/purescript-deck-gl/)

## npm requirements
- react >= 16.0.0
- deck.gl >= 5.1.1"


## acknowledgements
[ben](https://github.com/benweitzman) did a lot of the work setting up the configuration layers before he went on to make astral charts at [costar](https://www.costarastrology.com/)
