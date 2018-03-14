var DeckGL = require('deck.gl');

exports.makeScatterplotLayer = function (props) {
    return new DeckGL.ScatterplotLayer(props);
};

exports.defaultScatterplotProps = DeckGL.ScatterplotLayer.defaultProps;
