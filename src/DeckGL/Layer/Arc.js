var DeckGL = require('deck.gl');

exports.makeArcLayer = function (props) {
    return new DeckGL.ArcLayer(props);
};

exports.defaultArcProps = DeckGL.ArcLayer.defaultProps;
