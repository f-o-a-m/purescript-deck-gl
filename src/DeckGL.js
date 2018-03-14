var DeckGL = require('deck.gl');

exports.initializeGL = function(gl) {
    gl.enable(gl.DEPTH_TEST);
    gl.depthFunc(gl.LEQUAL);
};

exports.deckGL = DeckGL.DeckGL;
