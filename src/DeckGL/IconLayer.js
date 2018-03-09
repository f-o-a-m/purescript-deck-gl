const DeckGL = require('deck.gl');

exports.makeIconLayer = function (props) {
    return new DeckGL.IconLayer(props);
};
