const DeckGL = require('deck.gl');

exports.mkLayer = function (props) {
    return new DeckGL.IconLayer(props);
};
