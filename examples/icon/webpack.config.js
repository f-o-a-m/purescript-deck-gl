'use strict';
const path = require('path');

module.exports = {
  entry: './src/App',

  debug: true,

  devtool: 'eval',

  devServer: {
    contentBase: path.join(__dirname, "dist"),
    port: 4008,
    stats: 'errors-only'
  },

  output: {
    path: path.join(__dirname, "dist"),
    pathinfo: true,
    filename: 'bundle.js'
  },

  module: {
    loaders: [
      {
        test: /\.purs$/,
        loader: 'purs-loader',
        exclude: /node_modules/,
        query: {
          src: [
            'bower_components/purescript-*/src/**/*.purs',
            'src/**/*.purs'
          ]
        }
      }
    ]
  },

  resolve: {
    modulesDirectories: [
      'node_modules',
      'bower_components'
    ],
    extensions: [ '', '.purs', '.js']
  }
};
